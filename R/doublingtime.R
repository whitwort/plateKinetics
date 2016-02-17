#' Calculate doubling times
#' 
#' This function calculates doubling times for microbial cultures using time 
#' series data for each well in an experiment.  By default this analysis 
#' proceedure picks optical density readings using lag and plateau phase filters
#' and fits a log model.
#' 
#' @param experiment An experiment object, usually created by 
#'   \code{\link{loadExperiment}}.
#' @param value The name of a value on experiment$map to use for optical 
#'   density.
#' @param mapName The name of the column on the $map data.frame to use to save 
#'   information about which time points were included when calculating the 
#'   doubling time.
#' @param reduceName The name of the column on the $reduce data.frame to use to 
#'   save the doubling time for each well.
#' @param filters A character vector with the ids of filters to use.  See 
#'   \code{\link{dt.filters}} for available filters. Alternatively, use
#'   filterFuncs to pass in a list of functions directly.
#' @param filterFuncs A list of OD filter functions.  Each should take an od 
#'   vector as the sole argument.  Alternatively, specify filters.
#' @param timeFormat A chacater vector naming the time formatter to use.  See
#'   \code{\link{dt.timeFromats}} for available options.  Alternatively use
#'   formatFunc to pass the function in directly.
#' @param formatFunc A time formatter function.
#'   
#' @return An updated experiment object with new/updated columns on $map and 
#'   $reduce.
#'   
#' @export
doublingTime <- function( experiment
                        , value
                        , mapName     = paste(reduceName, "included", sep = ".")
                        , reduceName  = 'doublingTime'
                        , filters     = c('Lag', 'Plateau')
                        , filterFuncs = lapply( filters
                                              , function(name) { 
                                                  dt.filters[[name]]$func 
                                                }
                                              )
                        , timeFormat  = 'none'
                        , formatFunc  = dt.timeFormats[[timeFormat]]$func
                        ) {
  
  experiment$map[[mapName]] <- c(sapply( row.names(experiment$reduce)
                                       , filterDT
                                       , experiment  = experiment
                                       , value       = value
                                       , filterFuncs = filterFuncs
                                       )
                                 )
  experiment$reduce[[reduceName]] <- sapply( row.names(experiment$reduce)
                                           , calculateDT
                                           , experiment = experiment
                                           , value      = value
                                           , mapName    = mapName
                                           )
  
  experiment
}

filterPoints <- function(od, filterFuncs) {
  m <- sapply( filterFuncs
             , function(f) { f(od) }
             )

  apply( m 
       , 1
       , all
       )
}

filterDT    <- function(well, experiment, value, filterFuncs) {
  wells <- experiment$map$well
  ods   <- experiment$map[wells == well, value]
  filterPoints(ods, filterFuncs)
}

calculateDT <- function(well, experiment, value, mapName) {
  wells <- experiment$map$well
  df    <- experiment$map[wells == well & (experiment$map[[mapName]]),]
  time  <- df$time
  od    <- df[[value]]
  
  fitDT(od, time)
}

fitDT <- function(od, time) {
  if (length(od) > 0) {
    fit <- lm(log(od) ~ time)
    log(2) / coef(fit)["time"]
  } else {
    NA
  }
}

#' Plot a value across time for all wells in the experiment.
#' 
#' @param experiment An experiment object returned from 
#'   \code{\link{doublingTime}}
#' @param value The name of a value on experiment$map that holds optical density
#'   values.
#' @param included The name of the boolean column on experiment$map that records
#'   which time points are used for the calculation.
#'   
#' @return A ggplot2 object.
#'   
#' @export
plotDoublingTimeFacet <- function(experiment, value, included) {
  ggplot(experiment$map, aes_string(x = "time", y = value, color = included)) +
    geom_point(size = I(0.65)) +
    facet_grid(row ~ col) +
    scale_color_manual(values = c("#999999", "#00B0F6")) +  
    theme( axis.ticks.x = element_blank()
         , axis.ticks.y = element_blank()
         , axis.text.x  = element_blank()
         , axis.text.y  = element_blank()
         , legend.position = "none"
         )
}


# Filters
lagFilter <- function(od, lagWindow = 3) {
  od > (median(od[1:lagWindow]) * 2)
}

plateauFilter <- function(od, cutoff = 0.5) { od < cutoff }

bubbleFilter <- function(od, tolerance = 3, windowSize = 3) {
  
  n <- length(od)
  
  #Calculate pair-wise point-to-point value changes (deltas)
  localDelta <- abs( od[-1] - od[-n] )
  
  #Compare each pair-wise delta to the average delta in its neighborhood
  neighborhoodDelta <- sapply( 1:n
                             , function(i) {
                                 start <- max(1, i - windowSize)
                                 end   <- min(n, i + windowSize)
                                 
                                 abs(localDelta[i] - median(localDelta[start:end]))
                               }
                             )
  
  #Caclulate a neighborhood cutoff value based on the given tolerance parameter
  cutoff <- mean(neighborhoodDelta) * (1 + tolerance)
  
  #Keep only points with neighborhood deltas below the cutoff
  neighborhoodDelta < cutoff
  
}

#' Timepoint filter plugins
#' @export
dt.filters <- list( Lag              = list( id         = 'Lag'
                                           , func       =  lagFilter
                                           , lagWindow  = numericInput( 'dt.lagFilter.lagWindow'
                                                                      , "Lag window"
                                                                      , value = 3
                                                                      )
                                           )
                  , Plateau          = list( id         = 'Plateau'
                                           , func       = plateauFilter
                                           , cutoff     = numericInput( 'dt.plateauFilter.cutoff'
                                                                      , 'OD cutoff'
                                                                      , value = 0.85
                                                                      )
                                           )
                  , `Bubble reducer` = list( id         = 'Bubble reducer'
                                           , func       = bubbleFilter
                                           , tolerance  = numericInput( 'dt.bubbleFilter.tolerance'
                                                                      , 'Tolerance'
                                                                      , value = 3
                                                                      )
                                           , windowSize = numericInput( 'dt.bubbleFilter.windowSize'
                                                                      , 'Window size'
                                                                      , value = 3
                                                                      )
                                           )
                  )

#' Time conversion plugins 
#' @export
dt.timeFormats <- list( none      = list( id   = 'none'
                                        , name = "None"
                                        , func = function(v) { v }
                                        )
                      , min       = list( id   = 'min'
                                        , name = "Seconds to minutes"
                                        , func = function(v) { v / 60 }
                                        )
                      , min.round = list( id   = 'min.round'
                                        , name = "Seconds to minutes (rounded)"
                                        , func = function(v) { round(v / 60) }
                                        )
                      )


doublingTimeUI <- function(experiment) {
  tagList( fluidRow( tabBox( tabPanel( title = "Run"
                                     , p("This tool calculates a doubling time 
                                          for microbes in liquid culture using 
                                          optical density reads that include an 
                                          exponential growth phase.  After you 
                                          run an analysis you can use the", 
                                          strong(' Edit '), "tab to adjust which 
                                          points are used for the calculation."
                                         )
                                     , p("The", strong(" Save to "), " field 
                                          below gives the variable name that 
                                          will be used to add results to the 
                                          $map and $reduce data frames.  Note: 
                                          this means re-runing the analysis without 
                                          updating this value will overwrite 
                                          previous results. You can explore 
                                          the results on the 'Kinetics' and 
                                          'Factors' tabs."
                                        )
                                     , fluidRow( column( 3
                                                       , textInput( 'dt.saveName'
                                                                  , 'Save to'
                                                                  , 'doublingTime'
                                                                  )
                                                       )
                                               , column( 3
                                                       , uiOutput('dt.selectValueNames')
                                                       )
                                               , column( 3
                                                       , uiOutput('dt.conversion')
                                                       )
                                               , column( 3
                                                       , uiOutput('dt.selectFilters')
                                                       )
                                               )
                                     , fluidRow( column( 3 
                                                       , actionButton( 'dt.run'
                                                                     , 'Run'
                                                                     )
                                                       )
                                               , column( 3
                                                       , uiOutput('dt.filterOptions')
                                                       , offset = 6
                                                       )
                                               )
                                     )
                           , tabPanel( title = "Edit"
                                     , fluidRow( column( 8
                                                       , p( "Select a doubling time run to edit, then"
                                                          , strong(" click ")
                                                          , "a well to open it in the editor below. 
                                                             If the experiment includes many wells this 
                                                             plot will take a few moments to render. Blue
                                                             points represent data being used in the 
                                                             doubling time calculation."
                                                          )
                                                       )
                                                , column( 4
                                                        , uiOutput('dt.runSelection')
                                                        )
                                                )
                                     , plotOutput( 'dt.overview'
                                                 , click = 'dt.overview.click'
                                                 )
                                     , hr()
                                     , p("Click and drag to change which data points 
                                         are being used for the doubling time calculation. 
                                         The solid line represents the model fit. Don't forget to hit"
                                        , strong(" Save ")
                                        , "after you've made a change you want to keep."
                                        )
                                     , fluidRow( column( 10 
                                                       , plotOutput( 'dt.edit'
                                                                    , brush = brushOpts( id         = "dt.edit.brush"
                                                                                       , direction  = "x" 
                                                                                       , resetOnNew = TRUE
                                                                                       )
                                                                    )
                                                       )
                                               , column( 2
                                                       , selectInput( 'dtViewBox.scale'
                                                                    , 'Value scale:'
                                                                    , c('linear', 'log')
                                                                    , selected = 'linear'
                                                                    , multiple = FALSE
                                                                    )
                                                       , p("Doubling time:")
                                                       , verbatimTextOutput('dt.doublingTime')
                                                       , uiOutput('dt.saveButtonUI')
                                                       )
                                               )
                                     )
                           , title = "Doubling time"
                           , width = 12
                           , id = 'dt.tabBox'
                           )
                   )
         , fluidRow()
         )
}


doublingTimeServer <- function(experiment) {
  function(input, output, session) {
    
    output$dt.selectValueNames <- renderUI({
      selectInput( 'dt.value'
                 , label    = 'OD Channel'
                 , choices  = mapChannels(experiment)
                 , multiple = FALSE
                 )
    })
    
    output$dt.selectFilters <- renderUI({
      selectInput( 'dt.filters'
                 , label    = 'Time point filters'
                 , choices  = names(dt.filters)
                 , selected = c('Lag', 'Plateau')
                 , multiple = TRUE
                 )
    })
    
    output$dt.conversion <- renderUI({
      v        <- names(dt.timeFormats)
      names(v) <- sapply( dt.timeFormats
                        , function(a) { a$name }
                        )
      selectInput( 'dt.timeFormat'
                 , label    = 'Time formatting'
                 , choices  = v
                 , multiple = FALSE
                 )
    })

    argNames  <- function(filter) {
      b <- !(names(filter) %in% c('id', 'func'))
      names(filter)[b]
    }
    
    output$dt.filterOptions <- renderUI({
      allFilters <- dt.filters
      filters    <- allFilters[input$dt.filters]
      
      formatSection <- function(name) {
        tagList( tags$li(class = "dropdown-header", name)
               , lapply( argNames(filters[[name]])
                       , function(arg) { tags$li( filters[[name]][[arg]] ) }
                       )
               )
      }
      
      div( class = 'dropdown'
         , id    = 'dtOptionsMenuDropdown'
         , tags$script(" $(document).ready(function() {
                            $('#dtOptionsMenuDropdown .dropdown-menu').on({ 
                                'click':function(e){ e.stopPropagation(); }
                            });
                         });
                       "
                      )
         , tags$button( class = 'btn btn-default dropdown-toggle'
                      , type  = 'button'
                      , id    = 'dtOptionsMenu'
                      , `data-toggle`   = "dropdown"
                      , `aria-haspopup` ="true" 
                      , `aria-expanded` ="true"
                      , "Filter options"
                      , tags$span(class = "caret")
                      )
         , tags$ul( class = "dropdown-menu  dropdown-menu-right"
                  , `aria-labelledby` = 'dtOptionsMenu'
                  , tagList( lapply(names(filters), formatSection) )
                  )
         )      
      
    })
   
    observeEvent(input$dt.run, {
      doublingTime( experiment
                  , value      = input$dt.value
                  , reduceName = input$dt.saveName
                  , filters    = input$dt.filters
                  )
      
      if (is.null(experiment$analysis$doublingTime)) {
        experiment$analysis$doublingTime <- list()
      }

      run <- list( reduceName = input$dt.saveName
                 , mapName    = paste(input$dt.saveName, "included", sep = ".")
                 , value      = input$dt.value
                 , timeFormat = dt.timeFormats[[input$dt.timeFormat]]$func
                 )
      
      experiment$analysis$doublingTime[[input$dt.saveName]] <- run
      
      updateTabItems(session, 'dt.tabBox', 'Edit')
      #updateSelectizeInput(session, 'dt.editRun', selected = input$dt.saveName)
    
    })
    
    output$dt.runSelection <- renderUI({
      selectInput( 'dt.editRun'
                 , label    = "Edit"
                 , choices  = names(experiment$analysis$doublingTime)
                 , multiple = FALSE
                 )
    })
    
    output$dt.overview <- renderPlot({
      if (!is.null(input$dt.editRun)) {
        run <- experiment$analysis$doublingTime[[input$dt.editRun]]
        plotDoublingTimeFacet( experiment
                             , value    = run$value
                             , included = run$mapName
                             )
      }
    })
    
    edit.run <- reactive({
      if (!is.null(input$dt.editRun)) {
        experiment$analysis$doublingTime[[input$dt.editRun]]
      }
    })
    
    edit.well <- reactive({
      info  <- input$dt.overview.click
      getReduceGridWell(info, experiment)
    })
    
    # this is a bit of hack resetOnNew = FALSE seems to be polluting the DOM
    lastBrush <- NULL
    observe({
      edit.run() 
      edit.well()
      lastBrush <<- NULL
    })
    
    edit.brush <- reactive({
      
      # hackery to dirty this reactive when lastBrush changes
      edit.run() 
      edit.well()
      
      brush <- input$dt.edit.brush
      if (is.null(brush)) {
        lastBrush
      } else {
        lastBrush <<- brush
        brush
      }
    })
    
    edit.df  <- reactive({
      
      run   <- edit.run() 
      well  <- edit.well()
      brush <- edit.brush()
      
      if (!is.null(run)) {
        wells <- experiment$map$well
        rows  <- wells %in% well 
        
        if (any(rows)) {
          df    <- experiment$map[rows, c('time', run$value, run$mapName)] 
          
          if(!is.null(brush)) { 
            pdf <- df
            pdf$time <- run$timeFormat(df$time)
            pts <- brushedPoints( pdf
                                , brush
                                , xvar = 'time'
                                , yvar = run$value
                                )
            
            df[run$mapName] <- row.names(df) %in% row.names(pts)
          }
          
          df
        }
        
      }
    })
    
    edit.dt  <- reactive({
      run <- edit.run()
      df  <- edit.df()
      
      if (!is.null(df)) {
        filt  <- df[[run$mapName]]
        time  <- df$time[filt]
        od    <- df[[run$value]][filt]
        fitDT(od, time)
      }
      
    })
    
    output$dt.edit <- renderPlot({
      run <- edit.run()
      df  <- edit.df()
        
      if (!is.null(df)) {
        dt      <- edit.dt()
        startOD <- min( df[df[[run$mapName]], run$value] )
        startT  <- min( df[df[[run$mapName]], 'time'] )
        
        df$model <- startOD * ( 2^( (df$time - startT) / dt) )
         
        if (input$dtViewBox.scale == 'log') { 
          val  <- paste0("log(", run$value, ")")
          modl <- "log(model)"
          ymin <- log(min(df[[run$value]]))
          ymax <- log(max(df[[run$value]]))
        } else { 
          val  <- run$value
          modl <- "model"
          ymin <- min(df[[run$value]])
          ymax <- max(df[[run$value]])
        }
        
        ggplot(df, aes(x = run$timeFormat(time))) + 
          geom_line(aes_string(y = modl), color = "#00B0F6") +
          geom_point( aes_string( y     = val
                                , color = run$mapName
                                )
                    ) +
          scale_color_manual(values = c("#999999", "#00B0F6")) +
          theme(legend.position = "none") + 
          xlab("Time") + 
          ylim(ymin, ymax)
      }
        
      
    })
    
    output$dt.doublingTime <- renderPrint({
      run  <- edit.run()
      well <- edit.well()
      if (!is.null(run) && !is.na(well)) {
        run$timeFormat( edit.dt() )
      }

    })
    
    output$dt.saveButtonUI <- renderUI({
      well  <- edit.well()
      
      if (!is.null(well) & !is.null(edit.brush())) {
        actionButton('dt.save', 'Save', class = "btn btn-lg action-button btn-block shiny-bound-input")  
      }
    })
    
    observeEvent(input$dt.save, {
      
      run   <- edit.run() 
      well  <- edit.well()
      wells <- experiment$map$well
      rows  <- wells %in% well 
      df    <- edit.df()
      
      experiment$map[rows, run$mapName] <- df[run$mapName]
      
    })

  }
}


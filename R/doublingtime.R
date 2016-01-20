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
#'   \code{\link{dt.filters}} for available filters. 
#'   Alternatively, use filterFuncs to pass in a list of functions directly.
#' @param filterFuncs A list of OD filter functions.  Each should take an od
#'   vector as the sole argument.  Alternatively, specify filters.
#' @param timeFormat A character vector with the name of a time formatting 
#'   function.  See \code{\link{dt.timeFormats}} for options. 
#'   Alternatively, use formatFunc to pass in a formatting function directly.
#' @param formatFunc A time formatting function.  Should take the doubling time
#'   value as the sole argument.  Alternatively, specify a timeFormat.
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
                                           , formatFunc = formatFunc
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

calculateDT <- function(well, experiment, value, mapName, formatFunc) {
  wells <- experiment$map$well
  df    <- experiment$map[wells == well & experiment$map[[mapName]],]
  time  <- df$time
  od    <- df[[value]]
  
  if (length(od) > 0) {
    fit <- lm(log(od) ~ time)
    dt  <- log(2) / coef(fit)["time"]
    formatFunc(dt)
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

plateauFilter <- function(od, cutoff = 0.7) { od < cutoff }

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
                                     , plotOutput('dt.Overview')
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
                  , timeFormat = input$dt.timeFormat
                  )
      
      if (is.null(experiment$analysis$doublingTime)) {
        experiment$analysis$doublingTime <- list()
      }

      run <- list( reduceName = input$dt.saveName
                 , mapName    = paste(input$dt.saveName, "included", sep = ".")
                 , value      = input$dt.value
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
    
    output$dt.Overview <- renderPlot({
      if (!is.null(input$dt.editRun)) {
        run <- experiment$analysis$doublingTime[[input$dt.editRun]]
        plotDoublingTimeFacet( experiment
                             , value    = run$value
                             , included = run$mapName
                             )
      }
    })

  }
}


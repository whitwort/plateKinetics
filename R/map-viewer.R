#' Plot a value across time for all wells in the experiment.
#' 
#' @param experiment An experiment object, usually created by 
#'   \code{\link{loadExperiment}}.
#' @param value The name of a value on experiment$map to plot.
#'   
#' @return A ggplot2 object.
#'   
#' @export
plotMapFacet <- function(experiment, value) {
  ggplot(experiment$map, aes_string(x = "time", y = value)) +
    geom_point(size = I(0.85)) +
    facet_grid(row ~ col) +
    theme_bw() + 
    theme( axis.ticks.x = element_blank()
         , axis.ticks.y = element_blank()
         , axis.text.x  = element_blank()
         , axis.text.y  = element_blank()
         )
}

mapValueNames <- function(experiment) {
  setdiff( colnames(experiment$map)
         , c('well', 'time', colnames(experiment$reduce))
         )
}


mapUI <- function(experiment) {
  tagList( fluidRow( uiOutput('mapOverviewBox'))
         , fluidRow( box( uiOutput('mapViewBox.controls')
                        , hr()
                        , plotOutput('mapViewBox.plot')
                        , title  = "Viewer"
                        , status = "success"
                        , width  = 12
                        )
                   )
         )
}

getReduceGridWell <- function(info, experiment) {
  col.n  <- info$panelvar1
  row.n  <- info$panelvar2
  
  rows   <- experiment$reduce$row
  cols   <- experiment$reduce$col
  
  df <-experiment$reduce[rows == row.n & cols == col.n, ]
  
  if (nrow(df) != 0) {
    row.names(df)
  } else {
    NA
  }
  
}

mapServer <- function(experiment) {
  function(input, output, session) {
    
    pushSelection <- function(id, values, append = TRUE, unique = TRUE) {
      if (id %in% names(input)) {
        
        if (append) {
          isolate({
            vals <- c(input[[id]], values)
          })
        } else {
          vals <- values
        }
        
        if (unique) {
          vals <- unique(vals)
        }

        updateSelectInput(session, id, selected = vals)
      }
    }
    
    output$mapOverviewBox <- renderUI({
      
      info <- p("This overview shows the change in values over time for each 
                well in your experiment.  If there are many wells this plot 
                will take a while to render.", strong(" Click "), "to add a 
                channel to the current view below or ", strong(" double click "),
                "on a well to reset the viewer."
               )
      
      tabs <- lapply( mapValueNames(experiment)
                    , function(name) {
                        box.id <- paste0('mapOverviewBox.', name)
                        
                        output[[box.id]] <- renderPlot({ 
                          plotMapFacet(experiment, name) 
                        })
                        
                        observe({
                          info <- input[[paste0(box.id, ".click")]]
                          well <- getReduceGridWell(info, experiment)
                          if (!is.na(well)) {
                            pushSelection("mapViewBox.values", name)
                            pushSelection("mapViewBox.wells", well)  
                          }
                        })

                        observe({
                          info <- input[[paste0(box.id, ".dblclick")]]
                          well <- getReduceGridWell(info, experiment)
                          if (!is.na(well)) {
                            pushSelection("mapViewBox.values", name, append = FALSE)
                            pushSelection("mapViewBox.wells", well, append = FALSE)
                          }
                        })
                        
                        output[[paste0(box.id, '.hover.table')]] <- renderUI({
                          info <- input[[paste0(box.id, ".hover")]]
                          well <- getReduceGridWell(info, experiment)
                          df   <- experiment$reduce[well,]
                          
                          if (!is.na(well)) {
                            #hackerific
                            s <- renderTable(df)()
                            HTML(gsub( 'table-condensed\">'
                                     , 'table-condensed\", style=\"width: 100%;\">'
                                     , s
                                     , fixed = TRUE
                                     )
                                 )
                            
                          } else {
                            p("Hover over a well to see experimental labels.", class = "text-center")
                          }
                          
                        })
                        
                        tabPanel( title = name
                                , info
                                , plotOutput( box.id
                                            , click    = paste0(box.id, ".click")
                                            , dblclick = paste0(box.id, ".dblclick")
                                            , hover    = paste0(box.id, ".hover")
                                            )
                                , fluidRow( column( 10
                                                  , uiOutput(paste0(box.id, ".hover.table"))
                                                  , offset = 1
                                                  )
                                          )
                                ) 
                      }
                    )
      
      tabs$title = tagList(icon("th"), 'Overview')
      tabs$width = 12
      
      do.call(tabBox, tabs)
      
    })
    
    output$mapViewBox.controls <- renderUI({
      
      times <- c(min(experiment$map$time), max(experiment$map$time))
      
      allNames <- colnames(experiment$map)
      valNames <- mapValueNames(experiment)
      catNames <- allNames[ !sapply(experiment$map, is.numeric) &
                            !(allNames %in% valNames)
                          ]
      colNames <- allNames[!(allNames %in% valNames)]
      
      div( fluidRow( column( 4
                           , selectInput( 'mapViewBox.values'
                                        , 'Plot:'
                                        , valNames
                                        , multiple = TRUE
                                        )
                           )
                   , column( 4
                           , selectInput( 'mapViewBox.wells'
                                        , 'For well(s):'
                                        , experiment$design$wells
                                        , multiple = TRUE
                                        )
                           )
                   , column( 4
                           , sliderInput( 'mapViewBox.times'
                                        , 'Time range:'
                                        , min   = times[1]
                                        , max   = times[2]
                                        , value = times
                                        )
                           )
                   )
         , fluidRow( column( 4
                           , selectInput( 'mapViewBox.color'
                                        , 'Color by:'
                                        , c(colNames, 'channel')
                                        , selected = 'well'
                                        , multiple = FALSE
                                        )
                           )
                   , column( 4
                           , selectInput( 'mapViewBox.shape'
                                        , "Point shape:"
                                        , c(catNames, 'channel')
                                        , selected = 'channel'
                                        , multiple = FALSE
                                        )
                           )
                   , column( 4
                           , selectInput( 'mapViewBox.scale'
                                        , 'Value scale:'
                                        , c('linear', 'log')
                                        , selected = 'linear'
                                        , multiple = FALSE
                                        )
                           )
                   )
         )
      
    })
    
    output$mapViewBox.plot <- renderPlot({
      wells   <- experiment$map$well
      values  <- input$mapViewBox.values
      times   <- experiment$map$time
      trange  <- input$mapViewBox.times
      
      rows    <- (wells %in% input$mapViewBox.wells) &
        (times > trange[1] & times < trange[2])
      
      if (any(rows)) {
        df <- experiment$map[rows, ]
        df <- tidyr::gather_(df, 'channel', 'value', values)
        
        val <- if (input$mapViewBox.scale == 'log') "log(value)" else "value"
        
        ggplot(df) + 
          geom_point( aes_string( x     = "time"
                                , y     = val
                                , color = input$mapViewBox.color
                                , shape = input$mapViewBox.shape
                                )
                    )
      }
      
    })
    
  }
}

addPlugin( 'Kinetics'
         , ui     = mapUI
         , server = mapServer
         , id     = 'mapview'
         , icon   = icon("clock-o")
         )

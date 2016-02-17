#' @import shiny 
#' @import shinydashboard
NULL

#' View an experiment in an interactive shiny app
#' 
#' The experiment viewer is an interactive dashboard style shiny app that is 
#' dynamically created when this function is called.
#' 
#' @param experiment An experiment object, usually created by 
#'   \code{\link{loadExperiment}}.
#' @param plugins A set of plugins.  Defaults to the package wide set.
#'   
#' @return An experiment object, with any updates made using the interactive 
#'   app.
#'   
#' @export
viewExperiment <- function(experiment, plugins = default.plugins) {
  
  experiment$varName <- substitute(experiment)
  
  app <- shinyApp( ui     = viewUI(experiment, plugins)
                 , server = viewServer(experiment, plugins)
                 )

  runGadget( app
           , viewer = dialogViewer("View Experiment", width = 1250, height = 1000)
           )
  
}

# Generate a shiny UI and server for the current plugins
viewUI <- function(experiment, plugins) {
  
  homeMenu <- menuItem('Overview', tabName = 'overview', icon = icon("dashboard"))
  
  design <- experiment$design
  total  <- length(design$channels) * nrow(experiment$data) * length(design$wells)
  homeItem <- tabItem( tabName = 'overview'
                     , fluidRow( valueBox( length(design$channels)
                                         , "Channels"
                                         , icon  = icon("table")
                                         , color = "aqua"
                                         )
                               , valueBox( length(design$wells)
                                         , "Wells"
                                         , icon  = icon("th")
                                         , color = "purple"
                                         )
 
                               , valueBox( length(design$factors)
                                         , "Factors"
                                         , icon  = icon("tags")
                                         , color = "yellow"
                                         )
                               , valueBox( nrow(experiment$data)
                                         , "Time points"
                                         , icon  = icon("clock-o")
                                         , color = "green"
                                         )
                               , valueBox( total
                                         , "Data points"
                                         , icon  = icon("plus")
                                         , color = "maroon"
                                         )
                               , valueBox( format(object.size(experiment), units = "auto")
                                         , 'Size'
                                         , icon  = icon("file")
                                         , color = "blue"
                                         )
                               )
                     , br()
                     , p( "Currently loaded experiment:"
                        , strong(experiment$varName)
                        )
                     )
  
  pluginsMenu <- lapply( plugins
                       , function(p) {
                           menuItem( text    = p$name
                                   , tabName = p$id
                                   , icon    = p$icon
                                   )
                         }
                       )
  pluginItems <- lapply( plugins
                       , function(p) { 
                           tabItem(tabName = p$id, p$ui(experiment)) 
                         }
                       )
  names(pluginItems) <- NULL
  
  dashboardPage( dashboardHeader(title = "plateKinetics")
               , dashboardSidebar( sidebarMenu( homeMenu
                                              , pluginsMenu
                                              )
                                 , hr()
                                 , p("Closing the viewer will return the updated experiment object.")
                                 , fluidRow( column( 10
                                                   , actionButton( 'close'
                                                                 , "Close"
                                                                 , class = "btn-block"
                                                                 )
                                                   , offset = 1
                                                   )
                                   
                                           )
                                 , uiOutput('heartbeat')
                                 )
               , dashboardBody(do.call(tabItems, c(list(homeItem), pluginItems)))
               )
}

viewServer <- function(experiment, plugins) {
  function(input, output, session) {
    
    #reactiveExperiment <- makeReactiveBinding(experiment)
    
    reactExp <- reactiveValues( design  = experiment$design
                              , data    = experiment$data
                              , factors = experiment$factors
                              , map     = experiment$map
                              , reduce  = experiment$reduce
                              , varName = experiment$varName
                              )
    
    for (plugin in plugins) {
      plugin$server(reactExp)(input = input, output = output, session = session)
    }
    
    # hack: for some reason app disconnects after 60s of inactivity when launched
    # in Rstudio server
    output$heartbeat <- renderUI({
      invalidateLater(58 * 1000, session)
      p(Sys.time(), style = "visibility: hidden;")
    })
    
    observeEvent( input$close
                , stopApp(returnValue = invisible(reactiveValuesToList(reactExp)))
                )
 
  }
}



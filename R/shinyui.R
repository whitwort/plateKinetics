#' @import shiny 
#' @import shinydashboard
NULL

newPluginList <- function(plugins = list()) {
  list( add = function(name, ui, server, id, icon) {
                plugins[[id]] <<- list( name   = name
                                      , ui     = ui
                                      , server = server
                                      , id     = id
                                      , icon   = icon
                                      )
        }
      , get = function(id = names(plugins)) { plugins[id] }
      )
}
default.plugins <- newPluginList()

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
  
  plugins <- listPlugins(plugins)
  
  shinyApp( ui     = viewUI(experiment, substitute(experiment), plugins)
          , server = viewServer(experiment, plugins)
          )
  
}

# Generate a shiny UI and server for the current plugins
viewUI <- function(experiment, name, plugins) {
  
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
  
  dashboardPage( dashboardHeader(title = paste("View", name))
                 , dashboardSidebar(sidebarMenu(pluginsMenu))
                 , dashboardBody(do.call(tabItems, pluginItems))
  )
}

viewServer <- function(experiment, plugins) {
  function(input, output, session) {
    for (plugin in plugins) {
      plugin$server(experiment)(input = input, output = output, session = session)
    }
  }
}

#' Add a plugin to the experiment viewer
#' 
#' @param name A character name to use for the associated dashboard tab.
#' @param ui A function to call with the current experiment that returns a shiny
#'   UI for the plugin tab.  The function should take the experiment object as 
#'   the only argument.
#' @param server A function that takes an experiment object as the only argument
#'   and returns a shiny server function to handle the plugin.  The returned 
#'   shiny server function should take input, output and session arguments (in 
#'   that order).
#' @param id String to use for element id.
#' @param icon Icon to use in the sidebar menu.
#' @param plugins A plugin set.  Defaults to the package wide plugin set.
#'   
#' @details See factors.R, doublingtime.R and apoindex.R in this package for
#'   example plugin implementations.
#'   
#' @export
addPlugin <- function( name
                     , ui
                     , server  
                     , id      = gsub("\\s", "", tolower(name))
                     , icon    = shiny::icon("th")
                     , plugins = default.plugins
                     ) { 
  
  plugins$add(name, ui, server, id, icon)
}

#' List installed experiment viewer plugins
#'
#' @param plugins A plugin set.  Defaults to the package wide set.
#'
#' @return A list of plugins
#' 
#' @export
listPlugins <- function(plugins = default.plugins) { plugins$get() }


#' @import ggplot2
#' 
NULL

#' Create a raster plot summarizing the levels/values for a factor in an
#' experiment.
#' 
#' @param experiment An experiment object, usually created by 
#'   \code{\link{loadExperiment}}.
#' @param factorName A character name giving the factor to be visulaized.
#'   
#' @return A ggplot2 object.
#'
#' @export
plotFactor <- function(experiment, factorName) {
  
  df     <- experiment$reduce
  df$col <- factor(df$col, levels = sort(unique(df$col)))
  df$row <- factor(df$row, levels = sort(unique(df$row), decreasing = TRUE))
  
  ggplot(df, aes(col, row)) + 
    geom_raster( aes_string(fill = factorName)
               , hjust = 1
               , vjust = 0
               , alpha = 0.8
               ) + 
    theme(panel.grid.major = element_line(size = 1.5, color = "black")) + 
    labs(x = "column", y = "row")
    
}

factorUI <- function(experiment) {
  tagList( h1("Plate setup")
         , p("This page provides a set of visualizations summarizing the design 
              of this experiment by ploting factor levels/values across the 
              wells of your plate.  It is a useful tool to use as a sanity check 
              to make sure your actual design matches the current set of 
              annotations."
            )
         , uiOutput('setup.factors')
         )
}

factorServer <- function(experiment) {
  function(input, output, session) {
    
    output$setup.factors <- renderUI({

      factors <- experiment$design$factors
      lapply( names(factors)
            , function(factorName) {
                id <- paste('setup.factors', factorName, sep = ".")
                output[[id]] <- renderPlot({ plotFactor(experiment, factorName) })
                tagList(h2(factorName), plotOutput(id))
              }  
            )
      
    })
  }
}


# Register a new plugin for the experimental viewer
addPlugin(name = 'Plate setup', ui = factorUI, server = factorServer, icon = icon("tags"))

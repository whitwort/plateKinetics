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
plotFactor <- function(experiment, factorName) {
  
  design   <- experiment$design
  platform <- design$platform
  
  wells    <- c(platform)
  rows     <- rep(1:nrow(platform), times = ncol(platform))
  cols     <- rep(1:ncol(platform), each = nrow(platform))
  value    <- design$factors[[factorName]][wells]
  
  df <- data.frame(wells, rows, cols, value)
  
  ggplot( df
        , aes( factor(cols)
             , factor(rows, levels = sort(unique(rows), decreasing = TRUE))
             , fill  = value
             )
        )                                                                + 
    geom_raster(hjust = 1, vjust = 0, alpha = 0.8)                       + 
    theme(panel.grid.major = element_line(size = 1.5, color = "black"))  + 
    labs(x = "column", y = "row")

}

factorUI <- function(experiment) {
  tagList( h1("Experimental design")
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
addPlugin(name = 'Plate setup', ui = factorUI, server = factorServer, icon = icon("wrench"))

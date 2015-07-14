
reduceUI <- function(experiment, id) {
  
}

reduceServer <- function(experiment, id) {
  function(input, output, session) {
    
  }
}

id <- 'reduceview'
addPlugin( 'Factor summary'
         , ui     = function(experiment) { reduceUI(experimeent, id)    }
         , server = function(exerpiment) { reduceServer(experiment, id) }
         , id     = id
         , icon   = icon("bar-chart")
         )

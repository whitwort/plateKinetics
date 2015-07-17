
reduceUI <- function(experiment, id) {
  
}

reduceServer <- function(experiment, id) {
  function(input, output, session) {
    
  }
}

id <- 'reduceview'
addPlugin( id     = id
         , name   = 'Factors'
         , ui     = function(experiment) { reduceUI(experimeent, id)    }
         , server = function(exerpiment) { reduceServer(experiment, id) }
         , icon   = icon("bar-chart")
         )

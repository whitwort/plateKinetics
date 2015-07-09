
mapUI <- function(experiment, id) {
  
}

mapServer <- function(experiment, id) {
  function(input, output, session) {
    
  }
}

id <- 'mapview'
addPlugin( 'Factor summary (map)'
           , ui     = function(experiment) { mapUI(experimeent, id)    }
           , server = function(exerpiment) { mapServer(experiment, id) }
           , id     = id
)

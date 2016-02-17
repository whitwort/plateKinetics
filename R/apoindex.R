apoIndex <- function(experiment, growthChannel = "OD600", rosChannel = "DHE", indexName = 'apoIndex') {
  
  experiment$map[[indexName]] <- experiment$map[[rosChannel]] / experiment$map[[growthChannel]]
  experiment
  
}

apoindexUI <- function(experiment) {
  h2("apoIndex")
  p("Comming soon.")
}

apoindexServer <- function(experiment) {
  function(input, output, session) {
    
  }
}


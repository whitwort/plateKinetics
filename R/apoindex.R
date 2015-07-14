apoIndex <- function(experiment, growthChannel = "OD600", rosChannel = "DHE", indexName = 'apoIndex') {
  
  experiment$map[[indexName]] <- experiment$map[[rosChannel]] / experiment$map[[growthChannel]]
  experiment
  
}

apoindexUI <- function(experiment) {
  h2("apoIndex")
}

apoindexServer <- function(experiment) {
  function(input, output, session) {
    
  }
}

addPlugin(name = 'apoIndex', ui = apoindexUI, server = apoindexServer, icon = icon("line-chart"))

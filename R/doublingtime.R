

dtUI <- function(experiment) {
  h2("doubling times")
}


dtServer <- function(experiment) {
  function(input, output, session) {
    
  }
}

addPlugin(name = 'Doubling time', ui = dtUI, server = dtServer)

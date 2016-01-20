#' Default viewer app plugins
#' @export
default.plugins <- list( factors    = list( id     = 'factors'
                                          , name   = 'Plate setup'
                                          , ui     = factorUI
                                          , server = factorServer
                                          , icon   = icon("tags")
                                          )
                       , apoindex   = list( id     = 'apoindex'
                                          , name   = 'apoIndex'
                                          , ui     = apoindexUI
                                          , server = apoindexServer
                                          , icon   = icon("line-chart")
                                          )
                       , doubling   = list( id     = 'doublingtime'
                                          , ui     = doublingTimeUI
                                          , server = doublingTimeServer
                                          , name   = 'Doubling time'
                                          , icon   = icon('signal')
                                          )
                       , mapviewer  = list( id     = 'mapview' 
                                          , name   = 'Kinetics'
                                          , ui     = mapUI
                                          , server = mapServer
                                          , icon   = icon("clock-o")
                                          )
                       , reduceview = list( id     = 'reduceview'
                                          , name   = 'Factors'
                                          , ui     = function(experiment) { reduceUI(experimeent, id)    }
                                          , server = function(exerpiment) { reduceServer(experiment, id) }
                                          , icon   = icon("bar-chart")
                                          )
                       , resultabs  = list( id     = 'resulttables'
                                          , ui     = resultUI
                                          , server = resultServer
                                          , name   = 'Result tables'
                                          , icon   = icon("th")
                                          )
                       )

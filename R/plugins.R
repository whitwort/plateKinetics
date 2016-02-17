#' Default viewer app plugins
#' @export
default.plugins <- list( factors    = list( id     = 'factors'
                                          , name   = 'Plate setup'
                                          , ui     = factorUI
                                          , server = factorServer
                                          , icon   = icon("tags")
                                          )
                       , doubling   = list( id     = 'doublingtime'
                                          , ui     = doublingTimeUI
                                          , server = doublingTimeServer
                                          , name   = 'Doubling time'
                                          , icon   = icon('signal')
                                          )
                       , apoindex   = list( id     = 'apoindex'
                                          , name   = 'apoIndex'
                                          , ui     = apoindexUI
                                          , server = apoindexServer
                                          , icon   = icon("line-chart")
                                          )
                       , mapviewer  = list( id     = 'mapview' 
                                          , name   = 'Kinetics (map)'
                                          , ui     = mapUI
                                          , server = mapServer
                                          , icon   = icon("clock-o")
                                          )
                       , reduceview = list( id     = 'reduceview'
                                          , name   = 'Factors (reduce)'
                                          , ui     = reduceUI
                                          , server = reduceServer
                                          , icon   = icon("bar-chart")
                                          )
                       , resultabs  = list( id     = 'resulttables'
                                          , ui     = resultUI
                                          , server = resultServer
                                          , name   = 'Result tables'
                                          , icon   = icon("th")
                                          )
                       )

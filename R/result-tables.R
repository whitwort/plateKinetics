resultUI <- function(experiment) {
  
  mapText <- p("The results from analysis procedures which produce one new value 
                for each time point are stored in the `", experiment$varName, 
                "$map` data.frame.  You can explore these data on the 'Kinetics' 
                tab."
              )
  redText <- p("The results from analysis procedures which produce one value per 
                well are stored in the `", experiment$varName, 
                "$reduce` data.frame.  You can explore these data on the 
                'Factors' tab."
              )
  
  tagList( fluidRow( box( fluidRow( column( 9, mapText)
                                  , column( 3
                                          , downloadButton( 'rt.dowload.map'
                                                          , "Download map table"
                                                          )
                                          )
                                  )
                        , hr()
                        , fluidRow(column(12, DT::dataTableOutput('rt.map')))
                        , title  = "Kinetic analysis (map)"
                        , width  = 12
                        , status = "success"
                        )
                   )
         , fluidRow( box( fluidRow( column( 9, redText)
                                  , column( 3
                                          , downloadButton( 'rt.dowload.reduce'
                                                          , "Download reduce table"
                                                          , class = "btn-block"
                                                          )
                                          )
                                  )
                        , hr()
                        , fluidRow(column(12, DT::dataTableOutput('rt.reduce')))
                        , title  = "Factor analysis (reduce)"
                        , width  = 12
                        , status = "warning"
                        )
                   )
         )
}

resultServer <- function(experiment) {
  function(input, output, session) {
  
    output$rt.download.map <- downloadHandler( paste( experiment$varName
                                                    , "map"
                                                    , "txt"
                                                    , sep = "."
                                                    )
                                             , function(path) { 
                                                 write.csv(experiment$map, path) 
                                               }
                                             , 'text/csv'
                                             )
    
    output$rt.download.reduce <- downloadHandler( paste( experiment$varName
                                                       , "reduce"
                                                       , "txt"
                                                       , sep = "."
                                                       )
                                                , function(path) { 
                                                    write.csv(experiment$reduce, path) 
                                                  }
                                                , 'text/csv'
                                                )
    
    output$rt.map <- DT::renderDataTable({
      DT::datatable( experiment$map
                   , filter  = 'top'
                   , options = list(pageLength = 10)
                   )
    })
    
    output$rt.reduce <- DT::renderDataTable({
      DT::datatable( experiment$reduce
                   , filter  = 'top'
                   , options = list(pageLength = 10)
                   )
    })
    
  }
}


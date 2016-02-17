reduceNumericFactors <- function(experiment) {
  names <- colnames(experiment$factors)
  names[ sapply( names
               , function(n) { is.numeric(experiment$reduce[[n]]) }
               )
       ]
}

reduceCategoricalFactors <- function(experiment) {
  names <- colnames(experiment$factors)
  setdiff(names, reduceNumericFactors(experiment))
}

reduceValues <- function(experiment) {
  setdiff(colnames(experiment$reduce), colnames(experiment$factors))
}

reduceUI <- function(experiment) {
  fluidRow( tabBox( tabPanel( title = 'Categorical'
                            , uiOutput('reduceCat.controls')
                            , hr()
                            , plotOutput('reduceCat.plot')
                            )
                  , tabPanel( title = 'Quantitative'
                            , uiOutput('reduceQuant.controls')
                            , hr()
                            , plotOutput('reduceQuant.plot')
                            )
                  , width = 12
                  )
    
          )
}

reduceServer <- function(experiment) {
  function(input, output, session) {
   
    output$reduceCat.controls   <- renderUI({
      fluidRow( column( 3
                      , selectInput( 'reduceCat.value'
                                   , 'Plot:'
                                   , reduceValues(experiment)
                                   )
                      )
              , column( 3
                      , selectInput( 'reduceCat.factor'
                                   , 'By:'
                                   , reduceCategoricalFactors(experiment)
                                   )
                      )
              , column( 3
                      , selectInput( 'reduceCat.color'
                                   , 'Color:'
                                   , reduceCategoricalFactors(experiment)
                                   )
                      )
              , column( 3
                      , selectInput( 'reduceCat.scale'
                                   , 'Value scale:'
                                   , c('linear', 'log')
                                   , selected = 'linear'
                                   )
                      )
              )
    })
    
    output$reduceCat.plot       <- renderPlot({
      ggplot( experiment$reduce
            , aes_string( input$reduceCat.factor
                        , if (input$reduceCat.scale == 'linear') {  
                            input$reduceCat.value
                          } else {
                            paste0("log(", input$reduceCat.value, ")")
                          }
                        , fill = input$reduceCat.color
                        )
            ) +
        geom_boxplot()
    })
    
    output$reduceQuant.controls <- renderUI({
      fluidRow( column( 3
                      , selectInput( 'reduceQuant.value'
                                   , 'Plot:'
                                   , reduceValues(experiment)
                                   )
                      )
              , column( 3
                      , selectInput( 'reduceQuant.var'
                                   , 'Versus:'
                                   , reduceNumericFactors(experiment)
                                   )
                      )
              , column( 3
                      , selectInput( 'reduceQuant.color'
                                   , 'Color:'
                                   , reduceCategoricalFactors(experiment)
                                   )
                      )
              , column( 3
                      , selectInput( 'reduceQuant.scale'
                                   , 'Value scale:'
                                   , c('linear', 'log')
                                   , selected = 'linear'
                                   )
                      )
              )
    })
    
    output$reduceQuant.plot     <- renderPlot({
      ggplot( experiment$reduce
            , aes_string( input$reduceQuant.var
                        , if (input$reduceQuant.scale == 'linear') {
                            input$reduceQuant.value
                          } else {
                            paste0("log(", input$reduceQuant.value, ")")
                          }
                        , color = input$reduceQuant.color
                        )
            ) + 
        geom_point() +
        geom_smooth()
    })
    
     
  }
}

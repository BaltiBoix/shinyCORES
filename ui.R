shinyUI(navbarPage(h4("Shiny CORES Consumo de Productos petrolíferos en España"),
                   
                  windowTitle = "Shiny CORES",
                   
                  position = "static-top",
                  
                  tabPanel("por CCAA y Provincia 1",

                      fluidRow(
                              column(3,
                                   wellPanel(
                                         checkboxGroupInput("prod.sel", 
                                                      label = h4("Producto"), 
                                                      choices = products.list,
                                                      selected = products.list[c(2,4)]),
                                         
                                         br(),
                                         
                                         checkboxInput("norm.sel", label=h5("normaliza?"), value = FALSE),
                                         
                                         checkboxInput("onefacet2.sel", label=h5("todo en uno?"), value = FALSE)
                                   ),
                                         
                                   wellPanel(
                                         selectInput("CCAA.sel", 
                                                     label = h4("CCAA"), 
                                                     choices = CCAA.list,
                                                     selected = CCAA.list[length(CCAA.list)],
                                                     multiple = FALSE),

                                         uiOutput("provincia.uisel")

                                   ),
                                   wellPanel(
                                         sliderInput("fechas.sel", label = h4("Fechas"), 
                                                     min = fechas.rango[1], 
                                                     max = fechas.rango[2], 
                                                     value = fechas.rango)

                                   )
                              ),
                            column(9,
                                   wellPanel(
                                         plotlyOutput("plot", height = "750px")
                                   )
                            )
                      )

                  ),
                  tabPanel("por CCAA y Provincia 2",
                           
                           fluidRow(
                                 column(3,
                                        wellPanel(
                                              selectInput("prod1.sel", 
                                                      label = h4("Producto"), 
                                                      choices = products.list,
                                                      selected = products.list[c(2,4)],
                                                      multiple = FALSE),
                                              
                                              br(),
                                              
                                              checkboxInput("norm1.sel", label=h5("normaliza?"), value = FALSE),

                                              checkboxInput("onefacet.sel", label=h5("todo en uno?"), value = FALSE)
                                        ),
                                        
                                        wellPanel(
                                              selectInput("CCAA1.sel", 
                                                      label = h4("CCAA"), 
                                                      choices = CCAA.list,
                                                      selected = CCAA.list[length(CCAA.list)],
                                                      selectize = TRUE,
                                                      multiple = TRUE),

                                              uiOutput("provincia1.uisel")
                                              
                                        ),
                                        wellPanel(
                                              sliderInput("fechas1.sel", label = h4("Fechas"), 
                                                          min = fechas.rango[1], 
                                                          max = fechas.rango[2], 
                                                          value = fechas.rango)
                                              
                                        )
                                 ),
                                 column(9,
                                        wellPanel(
                                              plotOutput("plot1")
                                        )
                                 )
                           )
                           
                           
                           ),
                  tabPanel("Consumos por producto",
                           
                           fluidRow(
                                 column(3,
                                        wellPanel(
                                              checkboxGroupInput("familia.sel", 
                                                                 label = h4("Familia"), 
                                                                 choices = familia.list,
                                                                 selected = familia.list[c(2,4)]),
                                              
                                              br(),
                                              
                                              checkboxInput("norm2.sel", label=h5("normaliza?"), value = FALSE),
                                              
                                              checkboxInput("onefacet1.sel", label=h5("todo en uno?"), value = FALSE)
                                        ),
                                        
                                        uiOutput("producto.uisel"),
                                              
                                        wellPanel(
                                              sliderInput("fechas2.sel", label = h4("Fechas"), 
                                                          min = fechas1.rango[1], 
                                                          max = fechas1.rango[2], 
                                                          value = fechas1.rango)
                                              
                                        )
                                     
                                 ),
                                 
                                 column(9,
                                        wellPanel(
                                              plotOutput("plot2")
                                        )
                                 )
                           )
                           
                  ),
                  
                  tabPanel("Análisis de estacionalidad",
                           
                           fluidRow(
                                 column(3,
                                        wellPanel(
                                              selectInput("familia1.sel", 
                                                           label = h4("Familia"), 
                                                           choices = familia.list,
                                                           selected = familia.list[4],
                                                           multiple = FALSE),
                                              
                                              br(),
                                              
                                              uiOutput("producto1.uisel")
                                        ),

                                        wellPanel(
                                              sliderInput("fechas3.sel", label = h4("Fechas"), 
                                                          min = fechas1.rango[1], 
                                                          max = fechas1.rango[2] + 3, 
                                                          value = c(fechas1.rango[1], fechas1.rango[2] + 1))
                                              
                                        )
                                        
                                 ),
                                 
                                 column(9,
                                        wellPanel(
                                              plotOutput("plot3")
                                        )
                                 )
                           )
                           
                  ),
                  
                  tabPanel("README",

                           fluidRow(
                                 column(10, offset = 1,
                                        includeMarkdown("README.md")
                                 )
                           )
                  )
                           
))
shinyUI(navbarPage("Shiny CORES",
                  tabPanel("Datos Brutos por CCAA y Provincia",

                      fluidRow(
                              column(3,
                                   wellPanel(
                                         checkboxGroupInput("prod.sel", 
                                                      label = h4("Producto"), 
                                                      choices = products.list,
                                                      selected = products.list[c(2,4)]),
                                         
                                         br(),
                                         
                                         checkboxInput("norm.sel", label=h5("normaliza?"), value = FALSE)
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
                                         plotOutput("plot")
                                   )
                            )
                      )

                  ),
                  tabPanel("Datos Brutos por producto",
                           
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
                  tabPanel("Proyecciones")
))
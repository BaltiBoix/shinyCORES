shinyUI(navbarPage("Shiny CORES",
                  tabPanel("Datos Brutos por CCAA y Provincia",

                      fluidRow(
                            #tags$style(type='text/css', ".selectize-input { font-size: 12px; line-height: 12px;} .selectize-dropdown { font-size: 12px; line-height: 12px; }"),    
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
                                         plotOutput("plot1")
                                   )
                            )
                      )

                  ),
                  tabPanel("Datos Brutos por producto"),
                  tabPanel("Proyecciones")
))
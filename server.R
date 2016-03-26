shinyServer(function(input, output) {
      
      output$provincia.uisel<-renderUI({
            provincia.list<-df %>% filter(CCAA==input$CCAA.sel) %>% select(Provincia) %>% distinct()
            provincia.list<-as.character(provincia.list[,1])
            selectInput("provincia.sel", 
                        label = h4("Provincia"), 
                        choices = provincia.list,
                        selected = 'Total',
                        multiple = FALSE)
      })
      
      output$plot <- renderPlot({
            norm.Sel<-input$norm.sel
            CCAA.Sel<-input$CCAA.sel
            if(is.null(input$provincia.sel)){
                  Provincia.Sel<-'Total'
            }else{
                  Provincia.Sel<-input$provincia.sel
            }
            prod.Sel<-input$prod.sel
            xlim.Sel<-c(as.yearmon(paste0(input$fechas.sel[1],"-01-01")), 
                        as.yearmon(paste0(input$fechas.sel[2],"-01-01")))
            if(fechas.rango[2] == input$fechas.sel[2]){xlim.Sel[2]<-df$fecha[nrow(df)]}
            iprod.Sel<-which(names(df) %in% prod.Sel, arr.ind=T)
            if(length(iprod.Sel)>0){
                  z<-df %>% filter(CCAA==CCAA.Sel & Provincia==Provincia.Sel) %>% select(1, iprod.Sel)
                  if(NROW(z)>0){
                        z<-zoo(x=z[,2:NCOL(z)], order.by = z[,1])
                        if(norm.Sel){
                              nor<-sapply(window(z, start = xlim.Sel[1], end = xlim.Sel[1]+11/12),function(x) 100/mean(x))
                              if(NCOL(z)>1) nor<-matrix(rep(nor,each=NROW(z)),ncol=NCOL(z))
                              z<-z * nor
                        }
                        g<-autoplot(z, na.rm = TRUE)
                        if(NCOL(z)>1) g<-g + facet_free()
                        g<-g + scale_x_yearmon(limits=xlim.Sel)
                        g<-g + geom_line(size=0.5, na.rm = TRUE)
                        g<-g + geom_smooth(se=F, size=1, na.rm = TRUE)
                        g<-g + xlab("Fecha")+ggtitle(paste0(CCAA.Sel, "-", Provincia.Sel))
                        if(norm.Sel) {
                              g<-g + ylab("%")
                        }else{
                              g<-g + ylab("kt/mes")
                        }
                        g<-g + theme(axis.text = element_text(size = 12),
                                     plot.title = element_text(size = 16, face='bold'),
                                     strip.text = element_text(size = 16, face='bold'),
                                     axis.title.x = element_text(size = 14),
                                     axis.title.y = element_text(size = 14),
                                     panel.border = element_rect(linetype = 'solid', color = 'red', fill = NA),
                                     strip.background = element_rect(linetype = 'solid', color = 'darkred', fill = 'gray'),
                                     panel.grid.major = element_line(colour = "grey"),
                                     panel.grid.minor = element_blank())
                        g
                  }
            }
      }, height = 800, width = 'auto')

      output$provincia1.uisel<-renderUI({
            provincia1.list<-df %>% filter(CCAA %in% input$CCAA1.sel) %>% select(Provincia) %>% distinct()
            provincia1.list<-as.character(provincia1.list[,1])
            selectInput("provincia1.sel", 
                        label = h4("Provincia"), 
                        choices = provincia1.list,
                        selected = 'Total',
                        multiple = TRUE)
      })
      
      output$plot1 <- renderPlot({
            
            norm.Sel<-input$norm1.sel
            CCAA.Sel<-input$CCAA1.sel
            onefacet.Sel<-input$onefacet.sel
            xlim.Sel<-c(as.yearmon(paste0(input$fechas1.sel[1],"-01-01")), 
                        as.yearmon(paste0(input$fechas1.sel[2],"-01-01")))
            if(fechas.rango[2] == input$fechas1.sel[2]){xlim.Sel[2]<-df$fecha[nrow(df)]}
            if(is.null(input$provincia1.sel)){
                  Provincia.Sel<-'Total'
            }else{
                  Provincia.Sel<-input$provincia1.sel
            }
            prod.Sel<-input$prod1.sel
            
            iprod.Sel<-which(names(df) == prod.Sel)            
            z<-df %>% 
                  filter(CCAA %in% CCAA.Sel) %>% 
                  filter(Provincia %in% Provincia.Sel) %>% 
                  select(1:3, psel=iprod.Sel) %>%
                  mutate(CCAA.Prov=paste0(CCAA,'.',Provincia)) %>%
                  select(-CCAA, -Provincia) %>%
                  spread(key=CCAA.Prov, value=psel)
            
            if(NROW(z)>0) {
                  z<-zoo(x=z[,2:NCOL(z)], order.by = z[,1])
                  
                  if(norm.Sel){
                        nor<-sapply(window(z, start = xlim.Sel[1], end = xlim.Sel[1]+11/12),function(x) 100/mean(x))
                        if(NCOL(z)>1) nor<-matrix(rep(nor,each=NROW(z)),ncol=NCOL(z))
                        z<-z * nor
                  }
                  
                  if(onefacet.Sel){
                        g<-autoplot(z, na.rm = TRUE, facets = NULL)
                  }else{
                        g<-autoplot(z, na.rm = TRUE)
                        if(NCOL(z)>1) g<-g + facet_free()
                  }
                  g<-g + scale_x_yearmon(limits=xlim.Sel)
                  g<-g + geom_line(size=0.5, na.rm = TRUE)
                  g<-g + geom_smooth(se=F, size=1, na.rm = TRUE)
                  g<-g + xlab("Fecha")
                  if(norm.Sel){ 
                        g<-g + ylab("%")
                  }else{
                        g<-g + ylab("kt/mes")
                  }
                  g<-g + ggtitle(prod.Sel)
                  g<-g + theme(axis.text = element_text(size = 12),
                               plot.title = element_text(size = 16, face='bold'),
                               strip.text = element_text(size = 14, face='bold'),
                               axis.title.x = element_text(size = 14),
                               axis.title.y = element_text(size = 14),
                               panel.border = element_rect(linetype = 'solid', color = 'red', fill = NA),
                               strip.background = element_rect(linetype = 'solid', color = 'darkred', fill = 'gray'),
                               panel.grid.major = element_line(colour = "grey"),
                               panel.grid.minor = element_blank(),
                               legend.position = 'bottom',
                               legend.text = element_text(size = 14),
                               legend.title=element_blank())
                  g
            }
      }, height = 800, width = 'auto')
      
 })

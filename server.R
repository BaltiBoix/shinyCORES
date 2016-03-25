shinyServer(function(input, output) {
      
      #CCAA.Sel<-"Total Nacional"
      #Provincia.Sel<-"Total"
      #xlim.Sel<-c(as.yearmon("Enero 2000"), as.yearmon("Enero 2016"))
      #norm.Sel<-TRUE
      #prod.Sel<-c('GS95', 'GOA')
      #iprod.Sel<-which(names(df) %in% prod.Sel, arr.ind=T)
      
      output$provincia.uisel<-renderUI({
            provincia.list<-df %>% filter(CCAA==input$CCAA.sel) %>% select(Provincia) %>% distinct()
            selectInput("provincia.sel", 
                        label = h4("Provincia"), 
                        choices = provincia.list,
                        selected = 'Total',
                        multiple = FALSE)
      })
      
      output$plot1 <- renderPlot({
            norm.Sel<-input$norm.sel
            CCAA.Sel<-input$CCAA.sel
            Provincia.Sel<-"Total"
            if(!is.null(input$provincia.sel)){
                  Provincia.Sel<-input$provincia.sel
            }
            prod.Sel<-input$prod.sel
            xlim.Sel<-c(as.yearmon(paste("Enero",input$fechas.sel[1])), as.yearmon(paste("Enero",input$fechas.sel[2])))
            if(fechas.rango[2] == input$fechas.sel[2]){xlim.Sel[2]<-df$fecha[nrow(df)]}
            iprod.Sel<-which(names(df) %in% prod.Sel, arr.ind=T)
            z<-df %>% filter(CCAA==CCAA.Sel & Provincia==Provincia.Sel) %>% select(1, iprod.Sel)
            z<-zoo(x=z[,2:ncol(z)], order.by = z[,1])
            if(norm.Sel){
                  nor<-sapply(window(z, start = xlim.Sel[1], end = xlim.Sel[1]+11/12),function(x) 100/mean(x))
                  nor<-matrix(rep(nor,each=nrow(z)),ncol=ncol(z))
                  z<-z * nor
            }
            g<-autoplot(z)+ facet_free()+scale_x_yearmon(limits=xlim.Sel)
            g<-g + geom_line(color='blue', size=0.5)
            g<-g + stat_smooth(se=F, size=1)
            g<-g + xlab("Fecha")+ggtitle(paste0(CCAA.Sel, "-", Provincia.Sel))
            if(norm.Sel) {
                  g<-g + ylab("%")
            }else{
                  g<-g + ylab("kt")
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
      }, height = 800, width = 'auto')
      
 })

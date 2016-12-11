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
      
      output$plot <- renderPlotly({
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
                        z<-zoo(x=z[,2:NCOL(z)], order.by = as.Date(z[,1]))
                        names(z)[1]<-prod.Sel[1]
                        ytit<-"kt/mes"
                        if(norm.Sel){
                              nor<-sapply(window(z, start = as.Date(xlim.Sel[1]), end = as.Date(xlim.Sel[1]+11/12)),function(x) 100/mean(x))
                              if(NCOL(z)>1) nor<-matrix(rep(nor,each=NROW(z)),ncol=NCOL(z))
                              z<-z * nor
                              ytit<-"%"
                        }

                        zdf <- fortify.zoo(z) %>% filter(Index >= as.Date(xlim.Sel[1]) & Index <= as.Date(xlim.Sel[2]))
                        names(zdf)[2]<-prod.Sel[1]
                        if(input$onefacet2.sel){
                              p <- plot_ly()
                              for(i in 2:length(zdf)){
                                    tsi<-ts(zdf[,i], frequency=12, start =c(as.numeric(format(index(z[1,1]), "%Y")), as.numeric(format(index(z[1,1]), "%m"))))
                                    trend<-stl(tsi, "per")$time.series[, 2]
                                    p <- p %>%
                                          add_lines(x=zdf[,1], y=zdf[,i], type="scatter", mode = "lines", name=names(zdf[i]), 
                                                    line=list(color=jBrewColors[i-1], width=1.0)) %>%
                                          add_lines(x=zdf[,1], y=coredata(trend), type="scatter", mode = "lines", 
                                                    showlegend=FALSE, hoverinfo = "none", line=list(color=jBrewColors[i-1], width=2.0))
                              }
                              p<-p %>% layout(xaxis=list(title="Fecha", type="date", showline=TRUE, showgrid=TRUE,
                                                         tickwidth=1, mirror = TRUE,
                                                         gridwidth=0.5, gridcolor = toRGB("red", alpha=0.5)),
                                              yaxis=list(title=ytit, type="linear", showline=TRUE, showgrid=TRUE,
                                                         exponentformat="SI",
                                                         tickwidth=1, mirror = TRUE,
                                                         gridwidth=0.25, gridcolor = toRGB("red", alpha=0.25)),
                                              title=paste0("<b>",CCAA.Sel, "-", Provincia.Sel,"</b>"), margin=list(t = 40, b=50),
                                              font = list(size=16))
                              p
                        }else{
                              l<-list()
                              for(i in 2:length(zdf)){
                                    tsi<-ts(zdf[,i], frequency=12, start =c(as.numeric(format(index(z[1,1]), "%Y")), as.numeric(format(index(z[1,1]), "%m"))))
                                    trend<-stl(tsi, s.window = 15)$time.series[, 2]
                                    p1 <- plot_ly() %>%
                                          add_lines(x=zdf[,1], y=zdf[,i], type="scatter", mode = "lines", name=names(zdf[i]),
                                                    line=list(color=jBrewColors[i-1], width=1.0)) %>%
                                          add_lines(x=zdf[,1], y=coredata(trend), type="scatter", mode = "lines", 
                                                    name=paste("trend",names(zdf[i])), showlegend=FALSE, hoverinfo = "none",
                                                    line=list(color=jBrewColors[i-1], width=2.0)) %>%
                                          layout(xaxis=list(title="Fecha", type="date", showline=TRUE, showgrid=TRUE,
                                                            tickwidth=1, mirror = TRUE,
                                                            gridwidth=0.5, gridcolor = toRGB("red", alpha=0.5)),
                                                 yaxis=list(title=ytit, type="linear", showline=TRUE, showgrid=TRUE,
                                                            exponentformat="SI",
                                                            tickwidth=1, mirror = TRUE,
                                                            gridwidth=0.5, gridcolor = toRGB("red", alpha=0.5)))
                                    p2 <- plot_ly() %>%
                                          add_bars(x=zdf[,1], y=c(NA, diff(trend,1)), name=paste("Var",names(zdf[i])),
                                                   marker=list(color=jBrewColors[i-1])) %>%
                                          layout(xaxis=list(title="Fecha", type="date", showline=TRUE, showgrid=TRUE,
                                                            tickwidth=1, mirror = TRUE,
                                                            gridwidth=0.5, gridcolor = toRGB("red", alpha=0.5)),
                                                 yaxis=list(title=ytit, type="linear", showline=TRUE, showgrid=TRUE,
                                                            exponentformat="SI",
                                                            tickwidth=1, mirror = TRUE,
                                                            gridwidth=0.5, gridcolor = toRGB("red", alpha=0.5))) 
                                    
                                    
                                    l[[i-1]]<-subplot(p1, p2, nrows = 2, shareX = TRUE, heights = c(0.75,0.25))
                                    
                              }
                              subplot(l, nrows = ncol(zdf)-1, shareX = TRUE) %>%
                                    layout(xaxis=list(title="Fecha", type="date", showline=TRUE, showgrid=TRUE,
                                                      tickwidth=1, mirror = TRUE,
                                                      gridwidth=0.5, gridcolor = toRGB("red", alpha=0.5)),
                                           yaxis=list(title=ytit, type="linear", showline=TRUE, showgrid=TRUE,
                                                      exponentformat="SI",
                                                      tickwidth=1, mirror = TRUE,
                                                      gridwidth=0.5, gridcolor = toRGB("red", alpha=0.5)),
                                           title=paste0("<b>",CCAA.Sel, "-", Provincia.Sel,"</b>"), margin=list(t = 40, b=50),
                                           font = list(size=16))
                        }
                  }
            }
      }) 

      output$provincia1.uisel<-renderUI({
            provincia1.list<-df %>% filter(CCAA %in% input$CCAA1.sel) %>% select(Provincia) %>% distinct()
            provincia1.list<-as.character(provincia1.list[,1])
            selectInput("provincia1.sel", 
                        label = h4("Provincia"), 
                        choices = provincia1.list,
                        selected = 'Total',
                        multiple = TRUE)
      })
      
      output$plot1 <- renderPlotly({
            
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
                  z<-zoo(x=z[,2:NCOL(z)], order.by = as.Date(z[,1]))
                  ytit<-"kt/mes"
                  if(norm.Sel){
                        nor<-sapply(window(z, start = as.Date(xlim.Sel[1]), end = as.Date(xlim.Sel[1]+11/12)),function(x) 100/mean(x))
                        if(NCOL(z)>1) nor<-matrix(rep(nor,each=NROW(z)),ncol=NCOL(z))
                        z<-z * nor
                        ytit<-"%"
                  }
                  
                  zdf <- fortify.zoo(z) %>% filter(Index >= as.Date(xlim.Sel[1]) & Index <= as.Date(xlim.Sel[2]))
                  if(ncol(zdf) == 2) names(zdf)[2]<-paste0(CCAA.Sel[1],'.',Provincia.Sel[1])
                  
                  if(onefacet.Sel){
                        p <- plot_ly()
                        for(i in 2:length(zdf)){
                              tsi<-ts(zdf[,i], frequency=12, start =c(as.numeric(format(index(z[1,1]), "%Y")), as.numeric(format(index(z[1,1]), "%m"))))
                              trend<-stl(tsi, "per")$time.series[, 2]
                              p <- p %>%
                                    add_lines(x=zdf[,1], y=zdf[,i], type="scatter", mode = "lines", name=names(zdf[i]), 
                                              line=list(color=jBrewColors[i-1], width=1.0)) %>%
                                    add_lines(x=zdf[,1], y=coredata(trend), type="scatter", mode = "lines", 
                                              showlegend=FALSE, hoverinfo = "none", line=list(color=jBrewColors[i-1], width=2.0))
                        }
                        p<-p %>% layout(xaxis=list(title="Fecha", type="date", showline=TRUE, showgrid=TRUE,
                                                   tickwidth=1, mirror = TRUE,
                                                   gridwidth=0.5, gridcolor = toRGB("red", alpha=0.5)),
                                        yaxis=list(title=ytit, type="linear", showline=TRUE, showgrid=TRUE,
                                                   exponentformat="SI",
                                                   tickwidth=1, mirror = TRUE,
                                                   gridwidth=0.25, gridcolor = toRGB("red", alpha=0.25)),
                                        title=paste0("<b>",prod.Sel,"</b>"), margin=list(t = 40, b=50),
                                        font = list(size=16))
                        p
                        
                  }else{
                        l<-list()
                        for(i in 2:length(zdf)){
                              tsi<-ts(zdf[,i], frequency=12, start =c(as.numeric(format(index(z[1,1]), "%Y")), as.numeric(format(index(z[1,1]), "%m"))))
                              trend<-stl(tsi, s.window = 15)$time.series[, 2]
                              p1 <- plot_ly() %>%
                                    add_lines(x=zdf[,1], y=zdf[,i], type="scatter", mode = "lines", name=names(zdf[i]),
                                              line=list(color=jBrewColors[i-1], width=1.0)) %>%
                                    add_lines(x=zdf[,1], y=coredata(trend), type="scatter", mode = "lines", 
                                              name=paste("trend",names(zdf[i])), showlegend=FALSE, hoverinfo = "none",
                                              line=list(color=jBrewColors[i-1], width=2.0)) %>%
                                    layout(xaxis=list(title="Fecha", type="date", showline=TRUE, showgrid=TRUE,
                                                      tickwidth=1, mirror = TRUE,
                                                      gridwidth=0.5, gridcolor = toRGB("red", alpha=0.5)),
                                           yaxis=list(title=ytit, type="linear", showline=TRUE, showgrid=TRUE,
                                                      exponentformat="SI",
                                                      tickwidth=1, mirror = TRUE,
                                                      gridwidth=0.5, gridcolor = toRGB("red", alpha=0.5)))
                              p2 <- plot_ly() %>%
                                    add_bars(x=zdf[,1], y=c(NA, diff(trend,1)), name=paste("Var",names(zdf[i])),
                                             marker=list(color=jBrewColors[i-1])) %>%
                                    layout(xaxis=list(title="Fecha", type="date", showline=TRUE, showgrid=TRUE,
                                                      tickwidth=1, mirror = TRUE,
                                                      gridwidth=0.5, gridcolor = toRGB("red", alpha=0.5)),
                                           yaxis=list(title=ytit, type="linear", showline=TRUE, showgrid=TRUE,
                                                      exponentformat="SI",
                                                      tickwidth=1, mirror = TRUE,
                                                      gridwidth=0.5, gridcolor = toRGB("red", alpha=0.5))) 
                              
                              
                              l[[i-1]]<-subplot(p1, p2, nrows = 2, shareX = TRUE, heights = c(0.75,0.25))
                              
                        }
                        subplot(l, nrows = ncol(zdf)-1, shareX = TRUE) %>%
                              layout(xaxis=list(title="Fecha", type="date", showline=TRUE, showgrid=TRUE,
                                                tickwidth=1, mirror = TRUE,
                                                gridwidth=0.5, gridcolor = toRGB("red", alpha=0.5)),
                                     yaxis=list(title=ytit, type="linear", showline=TRUE, showgrid=TRUE,
                                                exponentformat="SI",
                                                tickwidth=1, mirror = TRUE,
                                                gridwidth=0.5, gridcolor = toRGB("red", alpha=0.5)),
                                     title=paste0("<b>",prod.Sel,"</b>"), margin=list(t = 40, b=50),
                                     font = list(size=16))
                        
                  }
            }
      }) #, height = 750, width = 'auto')

      output$producto.uisel<-renderUI({
            product1.list<-NULL
            for(f in input$familia.sel){
                  product1.list<-c(product1.list, products.pp[grep(paste0("^",f), products.pp)])
            }
            selectInput("product1.sel", 
                        label = h4("Producto"), 
                        choices = product1.list,
                        selected = c('GOIL.total', 'GSNA.total'),
                        multiple = TRUE)
      })
      
      output$plot2 <- renderPlotly({
            
            if(!is.null(input$product1.sel)){
                  norm.Sel<-input$norm2.sel
                  xlim.Sel<-c(as.yearmon(paste0(input$fechas2.sel[1],"-01-01")), 
                              as.yearmon(paste0(input$fechas2.sel[2],"-01-01")))
                  if(fechas1.rango[2] == input$fechas2.sel[2]){xlim.Sel[2]<-pp.df$fecha[nrow(pp.df)]}
                  
                  zpp<-pp.df %>% select(fecha, -anyo, -mes, one_of(input$product1.sel))
                  
                  zpp<-zoo(x = select(zpp, -fecha), order.by=zpp$fecha)
                  
                  if(norm.Sel){
                        nor<-sapply(window(zpp, start = xlim.Sel[1], end = xlim.Sel[1]+11/12),
                                    function(x) ifelse(is.na(100/mean(x)), 1, 100/mean(x)))
                        if(NCOL(zpp)>1) nor<-matrix(rep(nor,each=NROW(zpp)),ncol=NCOL(zpp))
                        zpp<-zpp * nor
                  }
                  #breaks.zpp = xlim.Sel[1]+seq.int(0,(xlim.Sel[2]-xlim.Sel[1])*12, length.out = 12)/12

                  if(input$onefacet1.sel){
                        g<-autoplot(zpp, na.rm = TRUE, facets = NULL)
                  }else{
                        g<-autoplot(zpp, na.rm = TRUE)
                        if(NCOL(zpp)>1) g<-g + facet_free()
                  }
                  g<-g + scale_x_yearmon(limits=xlim.Sel, format = "%b %Y") #,breaks=breaks.zpp
                  g<-g + geom_line(size=0.5, na.rm = TRUE)
                  g<-g + geom_smooth(se=F, size=1, na.rm = TRUE)
                  g<-g + xlab("Fecha")+ggtitle("Consumo mensual")
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
                               panel.grid.major= element_line(size = 0.25, colour = "red", linetype = "dotted"),
                               panel.grid.minor = element_blank(),
                               legend.position = 'bottom',
                               legend.text = element_text(size = 14),
                               legend.title=element_blank())
                  
                  ggplotly(g)
                  
            }
      })   #, height = 750, width = 'auto')
      
      output$producto1.uisel<-renderUI({
            product2.list<-NULL
            for(f in input$familia1.sel){
                  product2.list<-c(product2.list, products.pp[grep(paste0("^",f), products.pp)])
            }
            selectInput("product2.sel", 
                        label = h4("Producto"), 
                        choices = product2.list,
                        selected = 'GOIL.total',
                        multiple = FALSE)
      })
      
      output$plot3 <- renderPlot({
            
            if(!is.null(input$product2.sel)){
                  xlim.Sel<-c(as.yearmon(paste0(input$fechas3.sel[1],"-01-01")), 
                              as.yearmon(paste0(input$fechas3.sel[2],"-01-01")))
                  if(fechas1.rango[2] == input$fechas3.sel[2]){xlim.Sel[2]<-pp.df$fecha[nrow(pp.df)]}
                  
                  z<-pp.df %>% select(fecha, -anyo, -mes, one_of(input$product2.sel))
                  
                  z<-zoo(x = select(z, -fecha), order.by=z$fecha)
                  
                  z<-na.locf(z)
                  
                  fit<-seas(as.ts(z), forecast.save = "fct", forecast.probability = 0.95)
                  
                  zz0<-data.frame(fecha=as.character(as.yearmon(time(original(fit)))),
                                  original=drop(coredata(original(fit))),
                                  stringsAsFactors = FALSE) 
                  
                  zz<-data.frame(fecha=as.character(as.yearmon(time(final(fit)))),
                                 outlier=coredata(outlier(fit)),
                                 final=coredata(final(fit)), 
                                 trend=coredata(trend(fit)),
                                 stringsAsFactors = FALSE) 

                  zz<- left_join(zz, zz0, by='fecha')
                  
                  zzf<-data.frame(fecha=as.character(as.yearmon(time(series(fit, 'forecast.forecasts')))), 
                                  series(fit, 'forecast.forecasts'),
                                  stringsAsFactors = FALSE)
                  
                  zz<- full_join(zz, zzf, by='fecha') %>% mutate(fecha=as.yearmon(fecha))

                  g<-ggplot(zz, na.rm = TRUE)
                  g<-g + geom_line(aes(x=fecha, y=original, color='original'), size=0.5, na.rm = TRUE)
                  g<-g + geom_text(aes(x=fecha, y=original, label=outlier), na.rm = TRUE)
                  g<-g + geom_label(aes(x=fecha, y=original, label=outlier, color='outlier'), na.rm = TRUE)
                  g<-g + geom_line(aes(x=fecha, y=final, color='final'), size=1, na.rm = TRUE)
                  g<-g + geom_line(aes(x=fecha, y=trend, color='trend'), size=1.5, na.rm = TRUE)
                  g<-g + geom_line(aes(x=fecha, y=forecast, color='forecast'), size=1, na.rm = TRUE)
                  g<-g + geom_ribbon(aes(x=fecha, ymin = lowerci, ymax = upperci), fill = "grey70", alpha=0.5)
                  g<-g + scale_color_manual(values=c('original'='chartreuse4', 'final'='black', 'trend'='blue', 'forecast'='red', 'outlier'='orange1'),
                                            breaks=c('original', 'final', 'trend', 'forecast', 'outlier')) 
                  g<-g + ylab("kt")+ggtitle("Consumo mensual")
                  g<-g + scale_x_yearmon(limits=xlim.Sel)
                  g<-g + theme(panel.background = element_rect(colour = "red"),
                               plot.title = element_text(size = 16, face='bold', color='blue'),
                               panel.grid.major= element_line(size = 0.25, colour = "red", linetype = "dotted"),
                               axis.text.x = element_text(size = 14),
                               axis.title.x = element_blank(),
                               axis.title.y = element_text(size = 14),
                               legend.position = 'bottom',
                               legend.text = element_text(size = 14),
                               legend.title=element_blank())
                  g
                  
            }
      }, height = 750, width = 'auto')

})

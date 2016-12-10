require(dplyr, quietly = TRUE)
require(zoo, quietly = TRUE)
require(ggplot2, quietly = TRUE)
require(tidyr, quietly = TRUE)
require(ggseas, quietly=TRUE)
require(scales, quietly=TRUE)
require(markdown, quietly = TRUE)
require(plotly, quietly = TRUE)
require(RColorBrewer, quietly = TRUE)

jBrewColors <- brewer.pal(n = 9, name = "Set1")

df<-readRDS('data/consumos-pp-ccaa-provincias.RDS')
pp.df<-readRDS('data/consumos-pp.RDS')

#products.list<-eval(parse(text=paste0('list(',paste(names(df)[4:ncol(df)], '=', 4:ncol(df), collapse = ', '),')')))
products.list<-names(df)[4:ncol(df)]
CCAA.list<-unique(df$CCAA)
fechas.rango<-c(floor(as.numeric(df$fecha[1])), ceiling(as.numeric(df$fecha[NROW(df)])))

familia.list<-unique(unlist(strsplit(names(pp.df)[4:NCOL(pp.df)],"[.]"))[ c(TRUE,FALSE) ])
products.pp<-names(pp.df)[4:NCOL(pp.df)]
fechas1.rango<-c(floor(as.numeric(pp.df$fecha[1])), ceiling(as.numeric(pp.df$fecha[NROW(pp.df)])))

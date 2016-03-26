require(dplyr, quietly = TRUE)
require(zoo, quietly = TRUE)
require(ggplot2, quietly = TRUE)
require(tidyr, quietly = TRUE)

df<-readRDS('data/consumos-pp-ccaa-provincias.RDS')

#products.list<-eval(parse(text=paste0('list(',paste(names(df)[4:ncol(df)], '=', 4:ncol(df), collapse = ', '),')')))
products.list<-names(df)[4:ncol(df)]
CCAA.list<-unique(df$CCAA)
fechas.rango<-c(as.numeric(df$fecha[1]), as.numeric(df$fecha[NROW(df)]))

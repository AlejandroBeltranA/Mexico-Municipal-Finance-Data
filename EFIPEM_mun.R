### Dataset for Municipal Finance 
library(plyr)
library(dplyr)
library(rowr)
library(car)
library(tidyr)
library(stringr)
setwd("G:/My Drive/Dissertation/Data/finanzas_muni")

nombres <- read.csv("G:/My Drive/Dissertation/Data/finanzas_muni/efipem_municipal_csv/catalogos/tc_municipio.csv" , stringsAsFactors = TRUE, encoding = "UTF-8")

specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))
names(nombres)

nombres$X.U.FEFF.ID_ENTIDAD <- nombres$X.U.FEFF.ID_ENTIDAD/100
nombres$ID_MUNICIPIO <- nombres$ID_MUNICIPIO/100000
nombres$CVE_INEGI <- specify_decimal((nombres$X.U.FEFF.ID_ENTIDAD+nombres$ID_MUNICIPIO), 5)

nombres$CVE_INEGI <- as.character(nombres$CVE_INEGI)
nombres$CVE_INEGI <- str_remove(nombres$CVE_INEGI , "0.")

keep <- c("NOM_MUN", "CVE_INEGI")
nombres <- nombres[keep]


## Pull the data in
setwd("G:/My Drive/Dissertation/Data/finanzas_muni/efipem_municipal_csv/conjunto_de_datos")

files = list.files(pattern="*.csv")
files

muni_1989 <- as.data.frame(read.csv("efipem_municipal_anual_tr_cifra_1989.csv" , stringsAsFactors = TRUE,  encoding = "UTF-8"))

muni_1989$ID_ENTIDAD <- (muni_1989$ID_ENTIDAD/100)
muni_1989$ID_MUNICIPIO <- muni_1989$ID_MUNICIPIO/100000
muni_1989$CVE_INEGI <- specify_decimal((muni_1989$ID_ENTIDAD+muni_1989$ID_MUNICIPIO), 5)
muni_1989$CVE_INEGI <- as.character(muni_1989$CVE_INEGI)
muni_1989$CVE_INEGI <- str_remove(muni_1989$CVE_INEGI , "0.")


muni_1989 <-  select(muni_1989, CVE_INEGI, DESCRIPCION_CATEGORIA, VALOR)



muni_1989_2 <- filter(muni_1989, DESCRIPCION_CATEGORIA == c( "Impuestos"))

muni_1989_2 <- spread(muni_1989_2, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_1989_3 <- filter(muni_1989, DESCRIPCION_CATEGORIA == c( "Participaciones federales"))

muni_1989_3 <- spread(muni_1989_3, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_1989_4 <- filter(muni_1989, DESCRIPCION_CATEGORIA == c( "Impuesto predial"))

muni_1989_4 <- spread(muni_1989_4, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_1989_5 <- filter(muni_1989, DESCRIPCION_CATEGORIA == c( "Aportaciones federales y estatales"))

muni_1989_5 <- spread(muni_1989_5, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_1989_6 <- filter(muni_1989, DESCRIPCION_CATEGORIA == c( "Total de egresos"))

muni_1989_6 <- spread(muni_1989_6, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_1989_7 <- filter(muni_1989, DESCRIPCION_CATEGORIA == c( "Total de ingresos"))

muni_1989_7 <- spread(muni_1989_7, DESCRIPCION_CATEGORIA, VALOR)  

efipem_1989 <- full_join(muni_1989_2, muni_1989_3)
efipem_1989 <- full_join(efipem_1989, muni_1989_4)
efipem_1989 <- full_join(efipem_1989, muni_1989_5)
efipem_1989 <- full_join(efipem_1989, muni_1989_6)
efipem_1989 <- full_join(efipem_1989, muni_1989_7)
efipem_1989 <- full_join(efipem_1989, nombres)
efipem_1989$year <- rep(1989, length(efipem_1989$CVE_INEGI))


### 1990
files

muni_1990 <- as.data.frame(read.csv("efipem_municipal_anual_tr_cifra_1990.csv" , stringsAsFactors = TRUE,  encoding = "UTF-8"))

muni_1990$ID_ENTIDAD <- (muni_1990$ID_ENTIDAD/100)
muni_1990$ID_MUNICIPIO <- muni_1990$ID_MUNICIPIO/100000
muni_1990$CVE_INEGI <- specify_decimal((muni_1990$ID_ENTIDAD+muni_1990$ID_MUNICIPIO), 5)
muni_1990$CVE_INEGI <- as.character(muni_1990$CVE_INEGI)
muni_1990$CVE_INEGI <- str_remove(muni_1990$CVE_INEGI , "0.")


muni_1990 <-  select(muni_1990, CVE_INEGI, DESCRIPCION_CATEGORIA, VALOR)



muni_1990_2 <- filter(muni_1990, DESCRIPCION_CATEGORIA == c( "Impuestos"))

muni_1990_2 <- spread(muni_1990_2, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_1990_3 <- filter(muni_1990, DESCRIPCION_CATEGORIA == c( "Participaciones federales"))

muni_1990_3 <- spread(muni_1990_3, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_1990_4 <- filter(muni_1990, DESCRIPCION_CATEGORIA == c( "Impuesto predial"))

muni_1990_4 <- spread(muni_1990_4, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_1990_5 <- filter(muni_1990, DESCRIPCION_CATEGORIA == c( "Aportaciones federales y estatales"))

muni_1990_5 <- spread(muni_1990_5, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_1990_6 <- filter(muni_1990, DESCRIPCION_CATEGORIA == c( "Total de egresos"))

muni_1990_6 <- spread(muni_1990_6, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_1990_7 <- filter(muni_1990, DESCRIPCION_CATEGORIA == c( "Total de ingresos"))

muni_1990_7 <- spread(muni_1990_7, DESCRIPCION_CATEGORIA, VALOR)  

efipem_1990 <- full_join(muni_1990_2, muni_1990_3)
efipem_1990 <- full_join(efipem_1990, muni_1990_4)
efipem_1990 <- full_join(efipem_1990, muni_1990_5)
efipem_1990 <- full_join(efipem_1990, muni_1990_6)
efipem_1990 <- full_join(efipem_1990, muni_1990_7)
efipem_1990 <- full_join(efipem_1990, nombres)
efipem_1990$year <- rep(1990, length(efipem_1990$CVE_INEGI))

### 1991
muni_1991 <- as.data.frame(read.csv("efipem_municipal_anual_tr_cifra_1991.csv" , stringsAsFactors = TRUE,  encoding = "UTF-8"))

muni_1991$ID_ENTIDAD <- (muni_1991$ID_ENTIDAD/100)
muni_1991$ID_MUNICIPIO <- muni_1991$ID_MUNICIPIO/100000
muni_1991$CVE_INEGI <- specify_decimal((muni_1991$ID_ENTIDAD+muni_1991$ID_MUNICIPIO), 5)
muni_1991$CVE_INEGI <- as.character(muni_1991$CVE_INEGI)
muni_1991$CVE_INEGI <- str_remove(muni_1991$CVE_INEGI , "0.")


muni_1991 <-  select(muni_1991, CVE_INEGI, DESCRIPCION_CATEGORIA, VALOR)



muni_1991_2 <- filter(muni_1991, DESCRIPCION_CATEGORIA == c( "Impuestos"))

muni_1991_2 <- spread(muni_1991_2, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_1991_3 <- filter(muni_1991, DESCRIPCION_CATEGORIA == c( "Participaciones federales"))

muni_1991_3 <- spread(muni_1991_3, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_1991_4 <- filter(muni_1991, DESCRIPCION_CATEGORIA == c( "Impuesto predial"))

muni_1991_4 <- spread(muni_1991_4, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_1991_5 <- filter(muni_1991, DESCRIPCION_CATEGORIA == c( "Aportaciones federales y estatales"))

muni_1991_5 <- spread(muni_1991_5, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_1991_6 <- filter(muni_1991, DESCRIPCION_CATEGORIA == c( "Total de egresos"))

muni_1991_6 <- spread(muni_1991_6, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_1991_7 <- filter(muni_1991, DESCRIPCION_CATEGORIA == c( "Total de ingresos"))

muni_1991_7 <- spread(muni_1991_7, DESCRIPCION_CATEGORIA, VALOR)  

efipem_1991 <- full_join(muni_1991_2, muni_1991_3)
efipem_1991 <- full_join(efipem_1991, muni_1991_4)
efipem_1991 <- full_join(efipem_1991, muni_1991_5)
efipem_1991 <- full_join(efipem_1991, muni_1991_6)
efipem_1991 <- full_join(efipem_1991, muni_1991_7)
efipem_1991 <- full_join(efipem_1991, nombres)
efipem_1991$year <- rep(1991, length(efipem_1991$CVE_INEGI))


### 1992
muni_1992 <- as.data.frame(read.csv("efipem_municipal_anual_tr_cifra_1992.csv" , stringsAsFactors = TRUE,  encoding = "UTF-8"))

muni_1992$ID_ENTIDAD <- (muni_1992$ID_ENTIDAD/100)
muni_1992$ID_MUNICIPIO <- muni_1992$ID_MUNICIPIO/100000
muni_1992$CVE_INEGI <- specify_decimal((muni_1992$ID_ENTIDAD+muni_1992$ID_MUNICIPIO), 5)
muni_1992$CVE_INEGI <- as.character(muni_1992$CVE_INEGI)
muni_1992$CVE_INEGI <- str_remove(muni_1992$CVE_INEGI , "0.")


muni_1992 <-  select(muni_1992, CVE_INEGI, DESCRIPCION_CATEGORIA, VALOR)



muni_1992_2 <- filter(muni_1992, DESCRIPCION_CATEGORIA == c( "Impuestos"))

muni_1992_2 <- spread(muni_1992_2, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_1992_3 <- filter(muni_1992, DESCRIPCION_CATEGORIA == c( "Participaciones federales"))

muni_1992_3 <- spread(muni_1992_3, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_1992_4 <- filter(muni_1992, DESCRIPCION_CATEGORIA == c( "Impuesto predial"))

muni_1992_4 <- spread(muni_1992_4, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_1992_5 <- filter(muni_1992, DESCRIPCION_CATEGORIA == c( "Aportaciones federales y estatales"))

muni_1992_5 <- spread(muni_1992_5, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_1992_6 <- filter(muni_1992, DESCRIPCION_CATEGORIA == c( "Total de egresos"))

muni_1992_6 <- spread(muni_1992_6, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_1992_7 <- filter(muni_1992, DESCRIPCION_CATEGORIA == c( "Total de ingresos"))

muni_1992_7 <- spread(muni_1992_7, DESCRIPCION_CATEGORIA, VALOR)  

efipem_1992 <- full_join(muni_1992_2, muni_1992_3)
efipem_1992 <- full_join(efipem_1992, muni_1992_4)
efipem_1992 <- full_join(efipem_1992, muni_1992_5)
efipem_1992 <- full_join(efipem_1992, muni_1992_6)
efipem_1992 <- full_join(efipem_1992, muni_1992_7)
efipem_1992 <- full_join(efipem_1992, nombres)
efipem_1992$year <- rep(1992, length(efipem_1992$CVE_INEGI))

### 1993
muni_1993 <- as.data.frame(read.csv("efipem_municipal_anual_tr_cifra_1993.csv" , stringsAsFactors = TRUE,  encoding = "UTF-8"))

muni_1993$ID_ENTIDAD <- (muni_1993$ID_ENTIDAD/100)
muni_1993$ID_MUNICIPIO <- muni_1993$ID_MUNICIPIO/100000
muni_1993$CVE_INEGI <- specify_decimal((muni_1993$ID_ENTIDAD+muni_1993$ID_MUNICIPIO), 5)
muni_1993$CVE_INEGI <- as.character(muni_1993$CVE_INEGI)
muni_1993$CVE_INEGI <- str_remove(muni_1993$CVE_INEGI , "0.")


muni_1993 <-  select(muni_1993, CVE_INEGI, DESCRIPCION_CATEGORIA, VALOR)



muni_1993_2 <- filter(muni_1993, DESCRIPCION_CATEGORIA == c( "Impuestos"))

muni_1993_2 <- spread(muni_1993_2, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_1993_3 <- filter(muni_1993, DESCRIPCION_CATEGORIA == c( "Participaciones federales"))

muni_1993_3 <- spread(muni_1993_3, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_1993_4 <- filter(muni_1993, DESCRIPCION_CATEGORIA == c( "Impuesto predial"))

muni_1993_4 <- spread(muni_1993_4, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_1993_5 <- filter(muni_1993, DESCRIPCION_CATEGORIA == c( "Aportaciones federales y estatales"))

muni_1993_5 <- spread(muni_1993_5, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_1993_6 <- filter(muni_1993, DESCRIPCION_CATEGORIA == c( "Total de egresos"))

muni_1993_6 <- spread(muni_1993_6, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_1993_7 <- filter(muni_1993, DESCRIPCION_CATEGORIA == c( "Total de ingresos"))

muni_1993_7 <- spread(muni_1993_7, DESCRIPCION_CATEGORIA, VALOR)  

efipem_1993 <- full_join(muni_1993_2, muni_1993_3)
efipem_1993 <- full_join(efipem_1993, muni_1993_4)
efipem_1993 <- full_join(efipem_1993, muni_1993_5)
efipem_1993 <- full_join(efipem_1993, muni_1993_6)
efipem_1993 <- full_join(efipem_1993, muni_1993_7)
efipem_1993 <- full_join(efipem_1993, nombres)
efipem_1993$year <- rep(1993, length(efipem_1993$CVE_INEGI))

### 1994
muni_1994 <- as.data.frame(read.csv("efipem_municipal_anual_tr_cifra_1994.csv" , stringsAsFactors = TRUE,  encoding = "UTF-8"))

muni_1994$ID_ENTIDAD <- (muni_1994$ID_ENTIDAD/100)
muni_1994$ID_MUNICIPIO <- muni_1994$ID_MUNICIPIO/100000
muni_1994$CVE_INEGI <- specify_decimal((muni_1994$ID_ENTIDAD+muni_1994$ID_MUNICIPIO), 5)
muni_1994$CVE_INEGI <- as.character(muni_1994$CVE_INEGI)
muni_1994$CVE_INEGI <- str_remove(muni_1994$CVE_INEGI , "0.")


muni_1994 <-  select(muni_1994, CVE_INEGI, DESCRIPCION_CATEGORIA, VALOR)



muni_1994_2 <- filter(muni_1994, DESCRIPCION_CATEGORIA == c( "Impuestos"))

muni_1994_2 <- spread(muni_1994_2, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_1994_3 <- filter(muni_1994, DESCRIPCION_CATEGORIA == c( "Participaciones federales"))

muni_1994_3 <- spread(muni_1994_3, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_1994_4 <- filter(muni_1994, DESCRIPCION_CATEGORIA == c( "Impuesto predial"))

muni_1994_4 <- spread(muni_1994_4, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_1994_5 <- filter(muni_1994, DESCRIPCION_CATEGORIA == c( "Aportaciones federales y estatales"))

muni_1994_5 <- spread(muni_1994_5, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_1994_6 <- filter(muni_1994, DESCRIPCION_CATEGORIA == c( "Total de egresos"))

muni_1994_6 <- spread(muni_1994_6, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_1994_7 <- filter(muni_1994, DESCRIPCION_CATEGORIA == c( "Total de ingresos"))

muni_1994_7 <- spread(muni_1994_7, DESCRIPCION_CATEGORIA, VALOR)  

efipem_1994 <- full_join(muni_1994_2, muni_1994_3)
efipem_1994 <- full_join(efipem_1994, muni_1994_4)
efipem_1994 <- full_join(efipem_1994, muni_1994_5)
efipem_1994 <- full_join(efipem_1994, muni_1994_6)
efipem_1994 <- full_join(efipem_1994, muni_1994_7)
efipem_1994 <- full_join(efipem_1994, nombres)
efipem_1994$year <- rep(1994, length(efipem_1994$CVE_INEGI))

###1995
muni_1995 <- as.data.frame(read.csv("efipem_municipal_anual_tr_cifra_1995.csv" , stringsAsFactors = TRUE,  encoding = "UTF-8"))

muni_1995$ID_ENTIDAD <- (muni_1995$ID_ENTIDAD/100)
muni_1995$ID_MUNICIPIO <- muni_1995$ID_MUNICIPIO/100000
muni_1995$CVE_INEGI <- specify_decimal((muni_1995$ID_ENTIDAD+muni_1995$ID_MUNICIPIO), 5)
muni_1995$CVE_INEGI <- as.character(muni_1995$CVE_INEGI)
muni_1995$CVE_INEGI <- str_remove(muni_1995$CVE_INEGI , "0.")


muni_1995 <-  select(muni_1995, CVE_INEGI, DESCRIPCION_CATEGORIA, VALOR)



muni_1995_2 <- filter(muni_1995, DESCRIPCION_CATEGORIA == c( "Impuestos"))

muni_1995_2 <- spread(muni_1995_2, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_1995_3 <- filter(muni_1995, DESCRIPCION_CATEGORIA == c( "Participaciones federales"))

muni_1995_3 <- spread(muni_1995_3, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_1995_4 <- filter(muni_1995, DESCRIPCION_CATEGORIA == c( "Impuesto predial"))

muni_1995_4 <- spread(muni_1995_4, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_1995_5 <- filter(muni_1995, DESCRIPCION_CATEGORIA == c( "Aportaciones federales y estatales"))

muni_1995_5 <- spread(muni_1995_5, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_1995_6 <- filter(muni_1995, DESCRIPCION_CATEGORIA == c( "Total de egresos"))

muni_1995_6 <- spread(muni_1995_6, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_1995_7 <- filter(muni_1995, DESCRIPCION_CATEGORIA == c( "Total de ingresos"))

muni_1995_7 <- spread(muni_1995_7, DESCRIPCION_CATEGORIA, VALOR)  

efipem_1995 <- full_join(muni_1995_2, muni_1995_3)
efipem_1995 <- full_join(efipem_1995, muni_1995_4)
efipem_1995 <- full_join(efipem_1995, muni_1995_5)
efipem_1995 <- full_join(efipem_1995, muni_1995_6)
efipem_1995 <- full_join(efipem_1995, muni_1995_7)
efipem_1995 <- full_join(efipem_1995, nombres)
efipem_1995$year <- rep(1995, length(efipem_1995$CVE_INEGI))


### 1996

muni_1996 <- as.data.frame(read.csv("efipem_municipal_anual_tr_cifra_1996.csv" , stringsAsFactors = TRUE,  encoding = "UTF-8"))

muni_1996$ID_ENTIDAD <- (muni_1996$ID_ENTIDAD/100)
muni_1996$ID_MUNICIPIO <- muni_1996$ID_MUNICIPIO/100000
muni_1996$CVE_INEGI <- specify_decimal((muni_1996$ID_ENTIDAD+muni_1996$ID_MUNICIPIO), 5)
muni_1996$CVE_INEGI <- as.character(muni_1996$CVE_INEGI)
muni_1996$CVE_INEGI <- str_remove(muni_1996$CVE_INEGI , "0.")


muni_1996 <-  select(muni_1996, CVE_INEGI, DESCRIPCION_CATEGORIA, VALOR)



muni_1996_2 <- filter(muni_1996, DESCRIPCION_CATEGORIA == c( "Impuestos"))

muni_1996_2 <- spread(muni_1996_2, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_1996_3 <- filter(muni_1996, DESCRIPCION_CATEGORIA == c( "Participaciones federales"))

muni_1996_3 <- spread(muni_1996_3, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_1996_4 <- filter(muni_1996, DESCRIPCION_CATEGORIA == c( "Impuesto predial"))

muni_1996_4 <- spread(muni_1996_4, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_1996_5 <- filter(muni_1996, DESCRIPCION_CATEGORIA == c( "Aportaciones federales y estatales"))

muni_1996_5 <- spread(muni_1996_5, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_1996_6 <- filter(muni_1996, DESCRIPCION_CATEGORIA == c( "Total de egresos"))

muni_1996_6 <- spread(muni_1996_6, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_1996_7 <- filter(muni_1996, DESCRIPCION_CATEGORIA == c( "Total de ingresos"))

muni_1996_7 <- spread(muni_1996_7, DESCRIPCION_CATEGORIA, VALOR)  

efipem_1996 <- full_join(muni_1996_2, muni_1996_3)
efipem_1996 <- full_join(efipem_1996, muni_1996_4)
efipem_1996 <- full_join(efipem_1996, muni_1996_5)
efipem_1996 <- full_join(efipem_1996, muni_1996_6)
efipem_1996 <- full_join(efipem_1996, muni_1996_7)
efipem_1996 <- full_join(efipem_1996, nombres)
efipem_1996$year <- rep(1996, length(efipem_1996$CVE_INEGI))

### 1997
muni_1997 <- as.data.frame(read.csv("efipem_municipal_anual_tr_cifra_1997.csv" , stringsAsFactors = TRUE,  encoding = "UTF-8"))

muni_1997$ID_ENTIDAD <- (muni_1997$ID_ENTIDAD/100)
muni_1997$ID_MUNICIPIO <- muni_1997$ID_MUNICIPIO/100000
muni_1997$CVE_INEGI <- specify_decimal((muni_1997$ID_ENTIDAD+muni_1997$ID_MUNICIPIO), 5)
muni_1997$CVE_INEGI <- as.character(muni_1997$CVE_INEGI)
muni_1997$CVE_INEGI <- str_remove(muni_1997$CVE_INEGI , "0.")


muni_1997 <-  select(muni_1997, CVE_INEGI, DESCRIPCION_CATEGORIA, VALOR)



muni_1997_2 <- filter(muni_1997, DESCRIPCION_CATEGORIA == c( "Impuestos"))

muni_1997_2 <- spread(muni_1997_2, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_1997_3 <- filter(muni_1997, DESCRIPCION_CATEGORIA == c( "Participaciones federales"))

muni_1997_3 <- spread(muni_1997_3, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_1997_4 <- filter(muni_1997, DESCRIPCION_CATEGORIA == c( "Impuesto predial"))

muni_1997_4 <- spread(muni_1997_4, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_1997_5 <- filter(muni_1997, DESCRIPCION_CATEGORIA == c( "Aportaciones federales y estatales"))

muni_1997_5 <- spread(muni_1997_5, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_1997_6 <- filter(muni_1997, DESCRIPCION_CATEGORIA == c( "Total de egresos"))

muni_1997_6 <- spread(muni_1997_6, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_1997_7 <- filter(muni_1997, DESCRIPCION_CATEGORIA == c( "Total de ingresos"))

muni_1997_7 <- spread(muni_1997_7, DESCRIPCION_CATEGORIA, VALOR)  

efipem_1997 <- full_join(muni_1997_2, muni_1997_3)
efipem_1997 <- full_join(efipem_1997, muni_1997_4)
efipem_1997 <- full_join(efipem_1997, muni_1997_5)
efipem_1997 <- full_join(efipem_1997, muni_1997_6)
efipem_1997 <- full_join(efipem_1997, muni_1997_7)
efipem_1997 <- full_join(efipem_1997, nombres)
efipem_1997$year <- rep(1997, length(efipem_1997$CVE_INEGI))

## 1998
muni_1998 <- as.data.frame(read.csv("efipem_municipal_anual_tr_cifra_1998.csv" , stringsAsFactors = TRUE,  encoding = "UTF-8"))

muni_1998$ID_ENTIDAD <- (muni_1998$ID_ENTIDAD/100)
muni_1998$ID_MUNICIPIO <- muni_1998$ID_MUNICIPIO/100000
muni_1998$CVE_INEGI <- specify_decimal((muni_1998$ID_ENTIDAD+muni_1998$ID_MUNICIPIO), 5)
muni_1998$CVE_INEGI <- as.character(muni_1998$CVE_INEGI)
muni_1998$CVE_INEGI <- str_remove(muni_1998$CVE_INEGI , "0.")


muni_1998 <-  select(muni_1998, CVE_INEGI, DESCRIPCION_CATEGORIA, VALOR)



muni_1998_2 <- filter(muni_1998, DESCRIPCION_CATEGORIA == c( "Impuestos"))

muni_1998_2 <- spread(muni_1998_2, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_1998_3 <- filter(muni_1998, DESCRIPCION_CATEGORIA == c( "Participaciones federales"))

muni_1998_3 <- spread(muni_1998_3, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_1998_4 <- filter(muni_1998, DESCRIPCION_CATEGORIA == c( "Impuesto predial"))

muni_1998_4 <- spread(muni_1998_4, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_1998_5 <- filter(muni_1998, DESCRIPCION_CATEGORIA == c( "Aportaciones federales y estatales"))

muni_1998_5 <- spread(muni_1998_5, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_1998_6 <- filter(muni_1998, DESCRIPCION_CATEGORIA == c( "Total de egresos"))

muni_1998_6 <- spread(muni_1998_6, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_1998_7 <- filter(muni_1998, DESCRIPCION_CATEGORIA == c( "Total de ingresos"))

muni_1998_7 <- spread(muni_1998_7, DESCRIPCION_CATEGORIA, VALOR)  

efipem_1998 <- full_join(muni_1998_2, muni_1998_3)
efipem_1998 <- full_join(efipem_1998, muni_1998_4)
efipem_1998 <- full_join(efipem_1998, muni_1998_5)
efipem_1998 <- full_join(efipem_1998, muni_1998_6)
efipem_1998 <- full_join(efipem_1998, muni_1998_7)
efipem_1998 <- full_join(efipem_1998, nombres)
efipem_1998$year <- rep(1998, length(efipem_1998$CVE_INEGI))

### 1999
muni_1999 <- as.data.frame(read.csv("efipem_municipal_anual_tr_cifra_1999.csv" , stringsAsFactors = TRUE,  encoding = "UTF-8"))

muni_1999$ID_ENTIDAD <- (muni_1999$ID_ENTIDAD/100)
muni_1999$ID_MUNICIPIO <- muni_1999$ID_MUNICIPIO/100000
muni_1999$CVE_INEGI <- specify_decimal((muni_1999$ID_ENTIDAD+muni_1999$ID_MUNICIPIO), 5)
muni_1999$CVE_INEGI <- as.character(muni_1999$CVE_INEGI)
muni_1999$CVE_INEGI <- str_remove(muni_1999$CVE_INEGI , "0.")


muni_1999 <-  select(muni_1999, CVE_INEGI, DESCRIPCION_CATEGORIA, VALOR)



muni_1999_2 <- filter(muni_1999, DESCRIPCION_CATEGORIA == c( "Impuestos"))

muni_1999_2 <- spread(muni_1999_2, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_1999_3 <- filter(muni_1999, DESCRIPCION_CATEGORIA == c( "Participaciones federales"))

muni_1999_3 <- spread(muni_1999_3, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_1999_4 <- filter(muni_1999, DESCRIPCION_CATEGORIA == c( "Impuesto predial"))

muni_1999_4 <- spread(muni_1999_4, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_1999_5 <- filter(muni_1999, DESCRIPCION_CATEGORIA == c( "Aportaciones federales y estatales"))

muni_1999_5 <- spread(muni_1999_5, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_1999_6 <- filter(muni_1999, DESCRIPCION_CATEGORIA == c( "Total de egresos"))

muni_1999_6 <- spread(muni_1999_6, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_1999_7 <- filter(muni_1999, DESCRIPCION_CATEGORIA == c( "Total de ingresos"))

muni_1999_7 <- spread(muni_1999_7, DESCRIPCION_CATEGORIA, VALOR)  

efipem_1999 <- full_join(muni_1999_2, muni_1999_3)
efipem_1999 <- full_join(efipem_1999, muni_1999_4)
efipem_1999 <- full_join(efipem_1999, muni_1999_5)
efipem_1999 <- full_join(efipem_1999, muni_1999_6)
efipem_1999 <- full_join(efipem_1999, muni_1999_7)
efipem_1999 <- full_join(efipem_1999, nombres)
efipem_1999$year <- rep(1999, length(efipem_1999$CVE_INEGI))

### 2000
muni_2000 <- as.data.frame(read.csv("efipem_municipal_anual_tr_cifra_2000.csv" , stringsAsFactors = TRUE,  encoding = "UTF-8"))

muni_2000$ID_ENTIDAD <- (muni_2000$ID_ENTIDAD/100)
muni_2000$ID_MUNICIPIO <- muni_2000$ID_MUNICIPIO/100000
muni_2000$CVE_INEGI <- specify_decimal((muni_2000$ID_ENTIDAD+muni_2000$ID_MUNICIPIO), 5)
muni_2000$CVE_INEGI <- as.character(muni_2000$CVE_INEGI)
muni_2000$CVE_INEGI <- str_remove(muni_2000$CVE_INEGI , "0.")


muni_2000 <-  select(muni_2000, CVE_INEGI, DESCRIPCION_CATEGORIA, VALOR)



muni_2000_2 <- filter(muni_2000, DESCRIPCION_CATEGORIA == c( "Impuestos"))

muni_2000_2 <- spread(muni_2000_2, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2000_3 <- filter(muni_2000, DESCRIPCION_CATEGORIA == c( "Participaciones federales"))

muni_2000_3 <- spread(muni_2000_3, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2000_4 <- filter(muni_2000, DESCRIPCION_CATEGORIA == c( "Impuesto predial"))

muni_2000_4 <- spread(muni_2000_4, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2000_5 <- filter(muni_2000, DESCRIPCION_CATEGORIA == c( "Aportaciones federales y estatales"))

muni_2000_5 <- spread(muni_2000_5, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2000_6 <- filter(muni_2000, DESCRIPCION_CATEGORIA == c( "Total de egresos"))

muni_2000_6 <- spread(muni_2000_6, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2000_7 <- filter(muni_2000, DESCRIPCION_CATEGORIA == c( "Total de ingresos"))

muni_2000_7 <- spread(muni_2000_7, DESCRIPCION_CATEGORIA, VALOR)  

efipem_2000 <- full_join(muni_2000_2, muni_2000_3)
efipem_2000 <- full_join(efipem_2000, muni_2000_4)
efipem_2000 <- full_join(efipem_2000, muni_2000_5)
efipem_2000 <- full_join(efipem_2000, muni_2000_6)
efipem_2000 <- full_join(efipem_2000, muni_2000_7)
efipem_2000 <- full_join(efipem_2000, nombres)
efipem_2000$year <- rep(2000, length(efipem_2000$CVE_INEGI))

### 2001
muni_2001 <- as.data.frame(read.csv("efipem_municipal_anual_tr_cifra_2001.csv" , stringsAsFactors = TRUE,  encoding = "UTF-8"))

muni_2001$ID_ENTIDAD <- (muni_2001$ID_ENTIDAD/100)
muni_2001$ID_MUNICIPIO <- muni_2001$ID_MUNICIPIO/100000
muni_2001$CVE_INEGI <- specify_decimal((muni_2001$ID_ENTIDAD+muni_2001$ID_MUNICIPIO), 5)
muni_2001$CVE_INEGI <- as.character(muni_2001$CVE_INEGI)
muni_2001$CVE_INEGI <- str_remove(muni_2001$CVE_INEGI , "0.")


muni_2001 <-  select(muni_2001, CVE_INEGI, DESCRIPCION_CATEGORIA, VALOR)



muni_2001_2 <- filter(muni_2001, DESCRIPCION_CATEGORIA == c( "Impuestos"))

muni_2001_2 <- spread(muni_2001_2, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2001_3 <- filter(muni_2001, DESCRIPCION_CATEGORIA == c( "Participaciones federales"))

muni_2001_3 <- spread(muni_2001_3, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2001_4 <- filter(muni_2001, DESCRIPCION_CATEGORIA == c( "Impuesto predial"))

muni_2001_4 <- spread(muni_2001_4, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2001_5 <- filter(muni_2001, DESCRIPCION_CATEGORIA == c( "Aportaciones federales y estatales"))

muni_2001_5 <- spread(muni_2001_5, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2001_6 <- filter(muni_2001, DESCRIPCION_CATEGORIA == c( "Total de egresos"))

muni_2001_6 <- spread(muni_2001_6, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2001_7 <- filter(muni_2001, DESCRIPCION_CATEGORIA == c( "Total de ingresos"))

muni_2001_7 <- spread(muni_2001_7, DESCRIPCION_CATEGORIA, VALOR)  

efipem_2001 <- full_join(muni_2001_2, muni_2001_3)
efipem_2001 <- full_join(efipem_2001, muni_2001_4)
efipem_2001 <- full_join(efipem_2001, muni_2001_5)
efipem_2001 <- full_join(efipem_2001, muni_2001_6)
efipem_2001 <- full_join(efipem_2001, muni_2001_7)
efipem_2001 <- full_join(efipem_2001, nombres)
efipem_2001$year <- rep(2001, length(efipem_2001$CVE_INEGI))

### 2002
muni_2002 <- as.data.frame(read.csv("efipem_municipal_anual_tr_cifra_2002.csv" , stringsAsFactors = TRUE,  encoding = "UTF-8"))

muni_2002$ID_ENTIDAD <- (muni_2002$ID_ENTIDAD/100)
muni_2002$ID_MUNICIPIO <- muni_2002$ID_MUNICIPIO/100000
muni_2002$CVE_INEGI <- specify_decimal((muni_2002$ID_ENTIDAD+muni_2002$ID_MUNICIPIO), 5)
muni_2002$CVE_INEGI <- as.character(muni_2002$CVE_INEGI)
muni_2002$CVE_INEGI <- str_remove(muni_2002$CVE_INEGI , "0.")


muni_2002 <-  select(muni_2002, CVE_INEGI, DESCRIPCION_CATEGORIA, VALOR)



muni_2002_2 <- filter(muni_2002, DESCRIPCION_CATEGORIA == c( "Impuestos"))

muni_2002_2 <- spread(muni_2002_2, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2002_3 <- filter(muni_2002, DESCRIPCION_CATEGORIA == c( "Participaciones federales"))

muni_2002_3 <- spread(muni_2002_3, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2002_4 <- filter(muni_2002, DESCRIPCION_CATEGORIA == c( "Impuesto predial"))

muni_2002_4 <- spread(muni_2002_4, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2002_5 <- filter(muni_2002, DESCRIPCION_CATEGORIA == c( "Aportaciones federales y estatales"))

muni_2002_5 <- spread(muni_2002_5, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2002_6 <- filter(muni_2002, DESCRIPCION_CATEGORIA == c( "Total de egresos"))

muni_2002_6 <- spread(muni_2002_6, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2002_7 <- filter(muni_2002, DESCRIPCION_CATEGORIA == c( "Total de ingresos"))

muni_2002_7 <- spread(muni_2002_7, DESCRIPCION_CATEGORIA, VALOR)  

efipem_2002 <- full_join(muni_2002_2, muni_2002_3)
efipem_2002 <- full_join(efipem_2002, muni_2002_4)
efipem_2002 <- full_join(efipem_2002, muni_2002_5)
efipem_2002 <- full_join(efipem_2002, muni_2002_6)
efipem_2002 <- full_join(efipem_2002, muni_2002_7)
efipem_2002 <- full_join(efipem_2002, nombres)
efipem_2002$year <- rep(2002, length(efipem_2002$CVE_INEGI))

### 2003
muni_2003 <- as.data.frame(read.csv("efipem_municipal_anual_tr_cifra_2003.csv" , stringsAsFactors = TRUE,  encoding = "UTF-8"))

muni_2003$ID_ENTIDAD <- (muni_2003$ID_ENTIDAD/100)
muni_2003$ID_MUNICIPIO <- muni_2003$ID_MUNICIPIO/100000
muni_2003$CVE_INEGI <- specify_decimal((muni_2003$ID_ENTIDAD+muni_2003$ID_MUNICIPIO), 5)
muni_2003$CVE_INEGI <- as.character(muni_2003$CVE_INEGI)
muni_2003$CVE_INEGI <- str_remove(muni_2003$CVE_INEGI , "0.")


muni_2003 <-  select(muni_2003, CVE_INEGI, DESCRIPCION_CATEGORIA, VALOR)



muni_2003_2 <- filter(muni_2003, DESCRIPCION_CATEGORIA == c( "Impuestos"))

muni_2003_2 <- spread(muni_2003_2, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2003_3 <- filter(muni_2003, DESCRIPCION_CATEGORIA == c( "Participaciones federales"))

muni_2003_3 <- spread(muni_2003_3, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2003_4 <- filter(muni_2003, DESCRIPCION_CATEGORIA == c( "Impuesto predial"))

muni_2003_4 <- spread(muni_2003_4, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2003_5 <- filter(muni_2003, DESCRIPCION_CATEGORIA == c( "Aportaciones federales y estatales"))

muni_2003_5 <- spread(muni_2003_5, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2003_6 <- filter(muni_2003, DESCRIPCION_CATEGORIA == c( "Total de egresos"))

muni_2003_6 <- spread(muni_2003_6, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2003_7 <- filter(muni_2003, DESCRIPCION_CATEGORIA == c( "Total de ingresos"))

muni_2003_7 <- spread(muni_2003_7, DESCRIPCION_CATEGORIA, VALOR)  

efipem_2003 <- full_join(muni_2003_2, muni_2003_3)
efipem_2003 <- full_join(efipem_2003, muni_2003_4)
efipem_2003 <- full_join(efipem_2003, muni_2003_5)
efipem_2003 <- full_join(efipem_2003, muni_2003_6)
efipem_2003 <- full_join(efipem_2003, muni_2003_7)
efipem_2003 <- full_join(efipem_2003, nombres)
efipem_2003$year <- rep(2003, length(efipem_2003$CVE_INEGI))

### 2004
muni_2004 <- as.data.frame(read.csv("efipem_municipal_anual_tr_cifra_2004.csv" , stringsAsFactors = TRUE,  encoding = "UTF-8"))

muni_2004$ID_ENTIDAD <- (muni_2004$ID_ENTIDAD/100)
muni_2004$ID_MUNICIPIO <- muni_2004$ID_MUNICIPIO/100000
muni_2004$CVE_INEGI <- specify_decimal((muni_2004$ID_ENTIDAD+muni_2004$ID_MUNICIPIO), 5)
muni_2004$CVE_INEGI <- as.character(muni_2004$CVE_INEGI)
muni_2004$CVE_INEGI <- str_remove(muni_2004$CVE_INEGI , "0.")


muni_2004 <-  select(muni_2004, CVE_INEGI, DESCRIPCION_CATEGORIA, VALOR)



muni_2004_2 <- filter(muni_2004, DESCRIPCION_CATEGORIA == c( "Impuestos"))

muni_2004_2 <- spread(muni_2004_2, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2004_3 <- filter(muni_2004, DESCRIPCION_CATEGORIA == c( "Participaciones federales"))

muni_2004_3 <- spread(muni_2004_3, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2004_4 <- filter(muni_2004, DESCRIPCION_CATEGORIA == c( "Impuesto predial"))

muni_2004_4 <- spread(muni_2004_4, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2004_5 <- filter(muni_2004, DESCRIPCION_CATEGORIA == c( "Aportaciones federales y estatales"))

muni_2004_5 <- spread(muni_2004_5, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2004_6 <- filter(muni_2004, DESCRIPCION_CATEGORIA == c( "Total de egresos"))

muni_2004_6 <- spread(muni_2004_6, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2004_7 <- filter(muni_2004, DESCRIPCION_CATEGORIA == c( "Total de ingresos"))

muni_2004_7 <- spread(muni_2004_7, DESCRIPCION_CATEGORIA, VALOR)  

efipem_2004 <- full_join(muni_2004_2, muni_2004_3)
efipem_2004 <- full_join(efipem_2004, muni_2004_4)
efipem_2004 <- full_join(efipem_2004, muni_2004_5)
efipem_2004 <- full_join(efipem_2004, muni_2004_6)
efipem_2004 <- full_join(efipem_2004, muni_2004_7)
efipem_2004 <- full_join(efipem_2004, nombres)
efipem_2004$year <- rep(2004, length(efipem_2004$CVE_INEGI))

### 2005
muni_2005 <- as.data.frame(read.csv("efipem_municipal_anual_tr_cifra_2005.csv" , stringsAsFactors = TRUE,  encoding = "UTF-8"))

muni_2005$ID_ENTIDAD <- (muni_2005$ID_ENTIDAD/100)
muni_2005$ID_MUNICIPIO <- muni_2005$ID_MUNICIPIO/100000
muni_2005$CVE_INEGI <- specify_decimal((muni_2005$ID_ENTIDAD+muni_2005$ID_MUNICIPIO), 5)
muni_2005$CVE_INEGI <- as.character(muni_2005$CVE_INEGI)
muni_2005$CVE_INEGI <- str_remove(muni_2005$CVE_INEGI , "0.")


muni_2005 <-  select(muni_2005, CVE_INEGI, DESCRIPCION_CATEGORIA, VALOR)



muni_2005_2 <- filter(muni_2005, DESCRIPCION_CATEGORIA == c( "Impuestos"))

muni_2005_2 <- spread(muni_2005_2, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2005_3 <- filter(muni_2005, DESCRIPCION_CATEGORIA == c( "Participaciones federales"))

muni_2005_3 <- spread(muni_2005_3, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2005_4 <- filter(muni_2005, DESCRIPCION_CATEGORIA == c( "Impuesto predial"))

muni_2005_4 <- spread(muni_2005_4, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2005_5 <- filter(muni_2005, DESCRIPCION_CATEGORIA == c( "Aportaciones federales y estatales"))

muni_2005_5 <- spread(muni_2005_5, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2005_6 <- filter(muni_2005, DESCRIPCION_CATEGORIA == c( "Total de egresos"))

muni_2005_6 <- spread(muni_2005_6, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2005_7 <- filter(muni_2005, DESCRIPCION_CATEGORIA == c( "Total de ingresos"))

muni_2005_7 <- spread(muni_2005_7, DESCRIPCION_CATEGORIA, VALOR)  

efipem_2005 <- full_join(muni_2005_2, muni_2005_3)
efipem_2005 <- full_join(efipem_2005, muni_2005_4)
efipem_2005 <- full_join(efipem_2005, muni_2005_5)
efipem_2005 <- full_join(efipem_2005, muni_2005_6)
efipem_2005 <- full_join(efipem_2005, muni_2005_7)
efipem_2005 <- full_join(efipem_2005, nombres)
efipem_2005$year <- rep(2005, length(efipem_2005$CVE_INEGI))

### 2006
muni_2006 <- as.data.frame(read.csv("efipem_municipal_anual_tr_cifra_2006.csv" , stringsAsFactors = TRUE,  encoding = "UTF-8"))

muni_2006$ID_ENTIDAD <- (muni_2006$ID_ENTIDAD/100)
muni_2006$ID_MUNICIPIO <- muni_2006$ID_MUNICIPIO/100000
muni_2006$CVE_INEGI <- specify_decimal((muni_2006$ID_ENTIDAD+muni_2006$ID_MUNICIPIO), 5)
muni_2006$CVE_INEGI <- as.character(muni_2006$CVE_INEGI)
muni_2006$CVE_INEGI <- str_remove(muni_2006$CVE_INEGI , "0.")


muni_2006 <-  select(muni_2006, CVE_INEGI, DESCRIPCION_CATEGORIA, VALOR)



muni_2006_2 <- filter(muni_2006, DESCRIPCION_CATEGORIA == c( "Impuestos"))

muni_2006_2 <- spread(muni_2006_2, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2006_3 <- filter(muni_2006, DESCRIPCION_CATEGORIA == c( "Participaciones federales"))

muni_2006_3 <- spread(muni_2006_3, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2006_4 <- filter(muni_2006, DESCRIPCION_CATEGORIA == c( "Impuesto predial"))

muni_2006_4 <- spread(muni_2006_4, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2006_5 <- filter(muni_2006, DESCRIPCION_CATEGORIA == c( "Aportaciones federales y estatales"))

muni_2006_5 <- spread(muni_2006_5, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2006_6 <- filter(muni_2006, DESCRIPCION_CATEGORIA == c( "Total de egresos"))

muni_2006_6 <- spread(muni_2006_6, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2006_7 <- filter(muni_2006, DESCRIPCION_CATEGORIA == c( "Total de ingresos"))

muni_2006_7 <- spread(muni_2006_7, DESCRIPCION_CATEGORIA, VALOR)  

efipem_2006 <- full_join(muni_2006_2, muni_2006_3)
efipem_2006 <- full_join(efipem_2006, muni_2006_4)
efipem_2006 <- full_join(efipem_2006, muni_2006_5)
efipem_2006 <- full_join(efipem_2006, muni_2006_6)
efipem_2006 <- full_join(efipem_2006, muni_2006_7)
efipem_2006 <- full_join(efipem_2006, nombres)
efipem_2006$year <- rep(2006, length(efipem_2006$CVE_INEGI))

###2007
muni_2007 <- as.data.frame(read.csv("efipem_municipal_anual_tr_cifra_2007.csv" , stringsAsFactors = TRUE,  encoding = "UTF-8"))

muni_2007$ID_ENTIDAD <- (muni_2007$ID_ENTIDAD/100)
muni_2007$ID_MUNICIPIO <- muni_2007$ID_MUNICIPIO/100000
muni_2007$CVE_INEGI <- specify_decimal((muni_2007$ID_ENTIDAD+muni_2007$ID_MUNICIPIO), 5)
muni_2007$CVE_INEGI <- as.character(muni_2007$CVE_INEGI)
muni_2007$CVE_INEGI <- str_remove(muni_2007$CVE_INEGI , "0.")


muni_2007 <-  select(muni_2007, CVE_INEGI, DESCRIPCION_CATEGORIA, VALOR)



muni_2007_2 <- filter(muni_2007, DESCRIPCION_CATEGORIA == c( "Impuestos"))

muni_2007_2 <- spread(muni_2007_2, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2007_3 <- filter(muni_2007, DESCRIPCION_CATEGORIA == c( "Participaciones federales"))

muni_2007_3 <- spread(muni_2007_3, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2007_4 <- filter(muni_2007, DESCRIPCION_CATEGORIA == c( "Impuesto predial"))

muni_2007_4 <- spread(muni_2007_4, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2007_5 <- filter(muni_2007, DESCRIPCION_CATEGORIA == c( "Aportaciones federales y estatales"))

muni_2007_5 <- spread(muni_2007_5, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2007_6 <- filter(muni_2007, DESCRIPCION_CATEGORIA == c( "Total de egresos"))

muni_2007_6 <- spread(muni_2007_6, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2007_7 <- filter(muni_2007, DESCRIPCION_CATEGORIA == c( "Total de ingresos"))

muni_2007_7 <- spread(muni_2007_7, DESCRIPCION_CATEGORIA, VALOR)  

efipem_2007 <- full_join(muni_2007_2, muni_2007_3)
efipem_2007 <- full_join(efipem_2007, muni_2007_4)
efipem_2007 <- full_join(efipem_2007, muni_2007_5)
efipem_2007 <- full_join(efipem_2007, muni_2007_6)
efipem_2007 <- full_join(efipem_2007, muni_2007_7)
efipem_2007 <- full_join(efipem_2007, nombres)
efipem_2007$year <- rep(2007, length(efipem_2007$CVE_INEGI))

### 2008
muni_2008 <- as.data.frame(read.csv("efipem_municipal_anual_tr_cifra_2008.csv" , stringsAsFactors = TRUE,  encoding = "UTF-8"))

muni_2008$ID_ENTIDAD <- (muni_2008$ID_ENTIDAD/100)
muni_2008$ID_MUNICIPIO <- muni_2008$ID_MUNICIPIO/100000
muni_2008$CVE_INEGI <- specify_decimal((muni_2008$ID_ENTIDAD+muni_2008$ID_MUNICIPIO), 5)
muni_2008$CVE_INEGI <- as.character(muni_2008$CVE_INEGI)
muni_2008$CVE_INEGI <- str_remove(muni_2008$CVE_INEGI , "0.")


muni_2008 <-  select(muni_2008, CVE_INEGI, DESCRIPCION_CATEGORIA, VALOR)



muni_2008_2 <- filter(muni_2008, DESCRIPCION_CATEGORIA == c( "Impuestos"))

muni_2008_2 <- spread(muni_2008_2, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2008_3 <- filter(muni_2008, DESCRIPCION_CATEGORIA == c( "Participaciones federales"))

muni_2008_3 <- spread(muni_2008_3, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2008_4 <- filter(muni_2008, DESCRIPCION_CATEGORIA == c( "Impuesto predial"))

muni_2008_4 <- spread(muni_2008_4, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2008_5 <- filter(muni_2008, DESCRIPCION_CATEGORIA == c( "Aportaciones federales y estatales"))

muni_2008_5 <- spread(muni_2008_5, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2008_6 <- filter(muni_2008, DESCRIPCION_CATEGORIA == c( "Total de egresos"))

muni_2008_6 <- spread(muni_2008_6, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2008_7 <- filter(muni_2008, DESCRIPCION_CATEGORIA == c( "Total de ingresos"))

muni_2008_7 <- spread(muni_2008_7, DESCRIPCION_CATEGORIA, VALOR)  

efipem_2008 <- full_join(muni_2008_2, muni_2008_3)
efipem_2008 <- full_join(efipem_2008, muni_2008_4)
efipem_2008 <- full_join(efipem_2008, muni_2008_5)
efipem_2008 <- full_join(efipem_2008, muni_2008_6)
efipem_2008 <- full_join(efipem_2008, muni_2008_7)
efipem_2008 <- full_join(efipem_2008, nombres)
efipem_2008$year <- rep(2008, length(efipem_2008$CVE_INEGI))

### 2009
muni_2009 <- as.data.frame(read.csv("efipem_municipal_anual_tr_cifra_2009.csv" , stringsAsFactors = TRUE,  encoding = "UTF-8"))

muni_2009$ID_ENTIDAD <- (muni_2009$ID_ENTIDAD/100)
muni_2009$ID_MUNICIPIO <- muni_2009$ID_MUNICIPIO/100000
muni_2009$CVE_INEGI <- specify_decimal((muni_2009$ID_ENTIDAD+muni_2009$ID_MUNICIPIO), 5)
muni_2009$CVE_INEGI <- as.character(muni_2009$CVE_INEGI)
muni_2009$CVE_INEGI <- str_remove(muni_2009$CVE_INEGI , "0.")


muni_2009 <-  select(muni_2009, CVE_INEGI, DESCRIPCION_CATEGORIA, VALOR)



muni_2009_2 <- filter(muni_2009, DESCRIPCION_CATEGORIA == c( "Impuestos"))

muni_2009_2 <- spread(muni_2009_2, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2009_3 <- filter(muni_2009, DESCRIPCION_CATEGORIA == c( "Participaciones federales"))

muni_2009_3 <- spread(muni_2009_3, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2009_4 <- filter(muni_2009, DESCRIPCION_CATEGORIA == c( "Impuesto predial"))

muni_2009_4 <- spread(muni_2009_4, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2009_5 <- filter(muni_2009, DESCRIPCION_CATEGORIA == c( "Aportaciones federales y estatales"))

muni_2009_5 <- spread(muni_2009_5, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2009_6 <- filter(muni_2009, DESCRIPCION_CATEGORIA == c( "Total de egresos"))

muni_2009_6 <- spread(muni_2009_6, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2009_7 <- filter(muni_2009, DESCRIPCION_CATEGORIA == c( "Total de ingresos"))

muni_2009_7 <- spread(muni_2009_7, DESCRIPCION_CATEGORIA, VALOR)  

efipem_2009 <- full_join(muni_2009_2, muni_2009_3)
efipem_2009 <- full_join(efipem_2009, muni_2009_4)
efipem_2009 <- full_join(efipem_2009, muni_2009_5)
efipem_2009 <- full_join(efipem_2009, muni_2009_6)
efipem_2009 <- full_join(efipem_2009, muni_2009_7)
efipem_2009 <- full_join(efipem_2009, nombres)
efipem_2009$year <- rep(2009, length(efipem_2009$CVE_INEGI))

### 2010
muni_2010 <- as.data.frame(read.csv("efipem_municipal_anual_tr_cifra_2010.csv" , stringsAsFactors = TRUE,  encoding = "UTF-8"))

muni_2010$ID_ENTIDAD <- (muni_2010$ID_ENTIDAD/100)
muni_2010$ID_MUNICIPIO <- muni_2010$ID_MUNICIPIO/100000
muni_2010$CVE_INEGI <- specify_decimal((muni_2010$ID_ENTIDAD+muni_2010$ID_MUNICIPIO), 5)
muni_2010$CVE_INEGI <- as.character(muni_2010$CVE_INEGI)
muni_2010$CVE_INEGI <- str_remove(muni_2010$CVE_INEGI , "0.")


muni_2010 <-  select(muni_2010, CVE_INEGI, DESCRIPCION_CATEGORIA, VALOR)



muni_2010_2 <- filter(muni_2010, DESCRIPCION_CATEGORIA == c( "Impuestos"))

muni_2010_2 <- spread(muni_2010_2, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2010_3 <- filter(muni_2010, DESCRIPCION_CATEGORIA == c( "Participaciones federales"))

muni_2010_3 <- spread(muni_2010_3, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2010_4 <- filter(muni_2010, DESCRIPCION_CATEGORIA == c( "Impuesto predial"))

muni_2010_4 <- spread(muni_2010_4, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2010_5 <- filter(muni_2010, DESCRIPCION_CATEGORIA == c( "Aportaciones federales y estatales"))

muni_2010_5 <- spread(muni_2010_5, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2010_6 <- filter(muni_2010, DESCRIPCION_CATEGORIA == c( "Total de egresos"))

muni_2010_6 <- spread(muni_2010_6, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2010_7 <- filter(muni_2010, DESCRIPCION_CATEGORIA == c( "Total de ingresos"))

muni_2010_7 <- spread(muni_2010_7, DESCRIPCION_CATEGORIA, VALOR)  

efipem_2010 <- full_join(muni_2010_2, muni_2010_3)
efipem_2010 <- full_join(efipem_2010, muni_2010_4)
efipem_2010 <- full_join(efipem_2010, muni_2010_5)
efipem_2010 <- full_join(efipem_2010, muni_2010_6)
efipem_2010 <- full_join(efipem_2010, muni_2010_7)
efipem_2010 <- full_join(efipem_2010, nombres)
efipem_2010$year <- rep(2010, length(efipem_2010$CVE_INEGI))

### 2011
muni_2011 <- as.data.frame(read.csv("efipem_municipal_anual_tr_cifra_2011.csv" , stringsAsFactors = TRUE,  encoding = "UTF-8"))

muni_2011$ID_ENTIDAD <- (muni_2011$ID_ENTIDAD/100)
muni_2011$ID_MUNICIPIO <- muni_2011$ID_MUNICIPIO/100000
muni_2011$CVE_INEGI <- specify_decimal((muni_2011$ID_ENTIDAD+muni_2011$ID_MUNICIPIO), 5)
muni_2011$CVE_INEGI <- as.character(muni_2011$CVE_INEGI)
muni_2011$CVE_INEGI <- str_remove(muni_2011$CVE_INEGI , "0.")


muni_2011 <-  select(muni_2011, CVE_INEGI, DESCRIPCION_CATEGORIA, VALOR)



muni_2011_2 <- filter(muni_2011, DESCRIPCION_CATEGORIA == c( "Impuestos"))

muni_2011_2 <- spread(muni_2011_2, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2011_3 <- filter(muni_2011, DESCRIPCION_CATEGORIA == c( "Participaciones federales"))

muni_2011_3 <- spread(muni_2011_3, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2011_4 <- filter(muni_2011, DESCRIPCION_CATEGORIA == c( "Impuesto predial"))

muni_2011_4 <- spread(muni_2011_4, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2011_5 <- filter(muni_2011, DESCRIPCION_CATEGORIA == c( "Aportaciones federales y estatales"))

muni_2011_5 <- spread(muni_2011_5, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2011_6 <- filter(muni_2011, DESCRIPCION_CATEGORIA == c( "Total de egresos"))

muni_2011_6 <- spread(muni_2011_6, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2011_7 <- filter(muni_2011, DESCRIPCION_CATEGORIA == c( "Total de ingresos"))

muni_2011_7 <- spread(muni_2011_7, DESCRIPCION_CATEGORIA, VALOR)  

efipem_2011 <- full_join(muni_2011_2, muni_2011_3)
efipem_2011 <- full_join(efipem_2011, muni_2011_4)
efipem_2011 <- full_join(efipem_2011, muni_2011_5)
efipem_2011 <- full_join(efipem_2011, muni_2011_6)
efipem_2011 <- full_join(efipem_2011, muni_2011_7)
efipem_2011 <- full_join(efipem_2011, nombres)
efipem_2011$year <- rep(2011, length(efipem_2011$CVE_INEGI))

### 2012
muni_2012 <- as.data.frame(read.csv("efipem_municipal_anual_tr_cifra_2012.csv" , stringsAsFactors = TRUE,  encoding = "UTF-8"))

muni_2012$ID_ENTIDAD <- (muni_2012$ID_ENTIDAD/100)
muni_2012$ID_MUNICIPIO <- muni_2012$ID_MUNICIPIO/100000
muni_2012$CVE_INEGI <- specify_decimal((muni_2012$ID_ENTIDAD+muni_2012$ID_MUNICIPIO), 5)
muni_2012$CVE_INEGI <- as.character(muni_2012$CVE_INEGI)
muni_2012$CVE_INEGI <- str_remove(muni_2012$CVE_INEGI , "0.")


muni_2012 <-  select(muni_2012, CVE_INEGI, DESCRIPCION_CATEGORIA, VALOR)



muni_2012_2 <- filter(muni_2012, DESCRIPCION_CATEGORIA == c( "Impuestos"))

muni_2012_2 <- spread(muni_2012_2, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2012_3 <- filter(muni_2012, DESCRIPCION_CATEGORIA == c( "Participaciones federales"))

muni_2012_3 <- spread(muni_2012_3, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2012_4 <- filter(muni_2012, DESCRIPCION_CATEGORIA == c( "Impuesto predial"))

muni_2012_4 <- spread(muni_2012_4, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2012_5 <- filter(muni_2012, DESCRIPCION_CATEGORIA == c( "Aportaciones federales y estatales"))

muni_2012_5 <- spread(muni_2012_5, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2012_6 <- filter(muni_2012, DESCRIPCION_CATEGORIA == c( "Total de egresos"))

muni_2012_6 <- spread(muni_2012_6, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2012_7 <- filter(muni_2012, DESCRIPCION_CATEGORIA == c( "Total de ingresos"))

muni_2012_7 <- spread(muni_2012_7, DESCRIPCION_CATEGORIA, VALOR)  

efipem_2012 <- full_join(muni_2012_2, muni_2012_3)
efipem_2012 <- full_join(efipem_2012, muni_2012_4)
efipem_2012 <- full_join(efipem_2012, muni_2012_5)
efipem_2012 <- full_join(efipem_2012, muni_2012_6)
efipem_2012 <- full_join(efipem_2012, muni_2012_7)
efipem_2012 <- full_join(efipem_2012, nombres)
efipem_2012$year <- rep(2012, length(efipem_2012$CVE_INEGI))

###2013
muni_2013 <- as.data.frame(read.csv("efipem_municipal_anual_tr_cifra_2013.csv" , stringsAsFactors = TRUE,  encoding = "UTF-8"))

muni_2013$ID_ENTIDAD <- (muni_2013$ID_ENTIDAD/100)
muni_2013$ID_MUNICIPIO <- muni_2013$ID_MUNICIPIO/100000
muni_2013$CVE_INEGI <- specify_decimal((muni_2013$ID_ENTIDAD+muni_2013$ID_MUNICIPIO), 5)
muni_2013$CVE_INEGI <- as.character(muni_2013$CVE_INEGI)
muni_2013$CVE_INEGI <- str_remove(muni_2013$CVE_INEGI , "0.")


muni_2013 <-  select(muni_2013, CVE_INEGI, DESCRIPCION_CATEGORIA, VALOR)



muni_2013_2 <- filter(muni_2013, DESCRIPCION_CATEGORIA == c( "Impuestos"))

muni_2013_2 <- spread(muni_2013_2, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2013_3 <- filter(muni_2013, DESCRIPCION_CATEGORIA == c( "Participaciones federales"))

muni_2013_3 <- spread(muni_2013_3, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2013_4 <- filter(muni_2013, DESCRIPCION_CATEGORIA == c( "Impuesto predial"))

muni_2013_4 <- spread(muni_2013_4, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2013_5 <- filter(muni_2013, DESCRIPCION_CATEGORIA == c( "Aportaciones federales y estatales"))

muni_2013_5 <- spread(muni_2013_5, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2013_6 <- filter(muni_2013, DESCRIPCION_CATEGORIA == c( "Total de egresos"))

muni_2013_6 <- spread(muni_2013_6, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2013_7 <- filter(muni_2013, DESCRIPCION_CATEGORIA == c( "Total de ingresos"))

muni_2013_7 <- spread(muni_2013_7, DESCRIPCION_CATEGORIA, VALOR)  

efipem_2013 <- full_join(muni_2013_2, muni_2013_3)
efipem_2013 <- full_join(efipem_2013, muni_2013_4)
efipem_2013 <- full_join(efipem_2013, muni_2013_5)
efipem_2013 <- full_join(efipem_2013, muni_2013_6)
efipem_2013 <- full_join(efipem_2013, muni_2013_7)
efipem_2013 <- full_join(efipem_2013, nombres)
efipem_2013$year <- rep(2013, length(efipem_2013$CVE_INEGI))

### 2014
muni_2014 <- as.data.frame(read.csv("efipem_municipal_anual_tr_cifra_2014.csv" , stringsAsFactors = TRUE,  encoding = "UTF-8"))

muni_2014$ID_ENTIDAD <- (muni_2014$ID_ENTIDAD/100)
muni_2014$ID_MUNICIPIO <- muni_2014$ID_MUNICIPIO/100000
muni_2014$CVE_INEGI <- specify_decimal((muni_2014$ID_ENTIDAD+muni_2014$ID_MUNICIPIO), 5)
muni_2014$CVE_INEGI <- as.character(muni_2014$CVE_INEGI)
muni_2014$CVE_INEGI <- str_remove(muni_2014$CVE_INEGI , "0.")


muni_2014 <-  select(muni_2014, CVE_INEGI, DESCRIPCION_CATEGORIA, VALOR)



muni_2014_2 <- filter(muni_2014, DESCRIPCION_CATEGORIA == c( "Impuestos"))

muni_2014_2 <- spread(muni_2014_2, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2014_3 <- filter(muni_2014, DESCRIPCION_CATEGORIA == c( "Participaciones federales"))

muni_2014_3 <- spread(muni_2014_3, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2014_4 <- filter(muni_2014, DESCRIPCION_CATEGORIA == c( "Impuesto predial"))

muni_2014_4 <- spread(muni_2014_4, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2014_5 <- filter(muni_2014, DESCRIPCION_CATEGORIA == c( "Aportaciones federales y estatales"))

muni_2014_5 <- spread(muni_2014_5, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2014_6 <- filter(muni_2014, DESCRIPCION_CATEGORIA == c( "Total de egresos"))

muni_2014_6 <- spread(muni_2014_6, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2014_7 <- filter(muni_2014, DESCRIPCION_CATEGORIA == c( "Total de ingresos"))

muni_2014_7 <- spread(muni_2014_7, DESCRIPCION_CATEGORIA, VALOR)  

efipem_2014 <- full_join(muni_2014_2, muni_2014_3)
efipem_2014 <- full_join(efipem_2014, muni_2014_4)
efipem_2014 <- full_join(efipem_2014, muni_2014_5)
efipem_2014 <- full_join(efipem_2014, muni_2014_6)
efipem_2014 <- full_join(efipem_2014, muni_2014_7)
efipem_2014 <- full_join(efipem_2014, nombres)
efipem_2014$year <- rep(2014, length(efipem_2014$CVE_INEGI))

### 2015
muni_2015 <- as.data.frame(read.csv("efipem_municipal_anual_tr_cifra_2015.csv" , stringsAsFactors = TRUE,  encoding = "UTF-8"))

muni_2015$ID_ENTIDAD <- (muni_2015$ID_ENTIDAD/100)
muni_2015$ID_MUNICIPIO <- muni_2015$ID_MUNICIPIO/100000
muni_2015$CVE_INEGI <- specify_decimal((muni_2015$ID_ENTIDAD+muni_2015$ID_MUNICIPIO), 5)
muni_2015$CVE_INEGI <- as.character(muni_2015$CVE_INEGI)
muni_2015$CVE_INEGI <- str_remove(muni_2015$CVE_INEGI , "0.")


muni_2015 <-  select(muni_2015, CVE_INEGI, DESCRIPCION_CATEGORIA, VALOR)



muni_2015_2 <- filter(muni_2015, DESCRIPCION_CATEGORIA == c( "Impuestos"))

muni_2015_2 <- spread(muni_2015_2, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2015_3 <- filter(muni_2015, DESCRIPCION_CATEGORIA == c( "Participaciones federales"))

muni_2015_3 <- spread(muni_2015_3, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2015_4 <- filter(muni_2015, DESCRIPCION_CATEGORIA == c( "Impuesto predial"))

muni_2015_4 <- spread(muni_2015_4, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2015_5 <- filter(muni_2015, DESCRIPCION_CATEGORIA == c( "Aportaciones federales y estatales"))

muni_2015_5 <- spread(muni_2015_5, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2015_6 <- filter(muni_2015, DESCRIPCION_CATEGORIA == c( "Total de egresos"))

muni_2015_6 <- spread(muni_2015_6, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2015_7 <- filter(muni_2015, DESCRIPCION_CATEGORIA == c( "Total de ingresos"))

muni_2015_7 <- spread(muni_2015_7, DESCRIPCION_CATEGORIA, VALOR)  

efipem_2015 <- full_join(muni_2015_2, muni_2015_3)
efipem_2015 <- full_join(efipem_2015, muni_2015_4)
efipem_2015 <- full_join(efipem_2015, muni_2015_5)
efipem_2015 <- full_join(efipem_2015, muni_2015_6)
efipem_2015 <- full_join(efipem_2015, muni_2015_7)
efipem_2015 <- full_join(efipem_2015, nombres)
efipem_2015$year <- rep(2015, length(efipem_2015$CVE_INEGI))

### 2016
muni_2016 <- as.data.frame(read.csv("efipem_municipal_anual_tr_cifra_2016.csv" , stringsAsFactors = TRUE,  encoding = "UTF-8"))

muni_2016$ID_ENTIDAD <- (muni_2016$ID_ENTIDAD/100)
muni_2016$ID_MUNICIPIO <- muni_2016$ID_MUNICIPIO/100000
muni_2016$CVE_INEGI <- specify_decimal((muni_2016$ID_ENTIDAD+muni_2016$ID_MUNICIPIO), 5)
muni_2016$CVE_INEGI <- as.character(muni_2016$CVE_INEGI)
muni_2016$CVE_INEGI <- str_remove(muni_2016$CVE_INEGI , "0.")


muni_2016 <-  select(muni_2016, CVE_INEGI, DESCRIPCION_CATEGORIA, VALOR)



muni_2016_2 <- filter(muni_2016, DESCRIPCION_CATEGORIA == c( "Impuestos"))

muni_2016_2 <- spread(muni_2016_2, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2016_3 <- filter(muni_2016, DESCRIPCION_CATEGORIA == c( "Participaciones federales"))

muni_2016_3 <- spread(muni_2016_3, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2016_4 <- filter(muni_2016, DESCRIPCION_CATEGORIA == c( "Impuesto predial"))

muni_2016_4 <- spread(muni_2016_4, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2016_5 <- filter(muni_2016, DESCRIPCION_CATEGORIA == c( "Aportaciones federales y estatales"))

muni_2016_5 <- spread(muni_2016_5, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2016_6 <- filter(muni_2016, DESCRIPCION_CATEGORIA == c( "Total de egresos"))

muni_2016_6 <- spread(muni_2016_6, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2016_7 <- filter(muni_2016, DESCRIPCION_CATEGORIA == c( "Total de ingresos"))

muni_2016_7 <- spread(muni_2016_7, DESCRIPCION_CATEGORIA, VALOR)  

efipem_2016 <- full_join(muni_2016_2, muni_2016_3)
efipem_2016 <- full_join(efipem_2016, muni_2016_4)
efipem_2016 <- full_join(efipem_2016, muni_2016_5)
efipem_2016 <- full_join(efipem_2016, muni_2016_6)
efipem_2016 <- full_join(efipem_2016, muni_2016_7)
efipem_2016 <- full_join(efipem_2016, nombres)
efipem_2016$year <- rep(2016, length(efipem_2016$CVE_INEGI))

### 2017
muni_2017 <- as.data.frame(read.csv("efipem_municipal_anual_tr_cifra_2017.csv" , stringsAsFactors = TRUE,  encoding = "UTF-8"))

muni_2017$ID_ENTIDAD <- (muni_2017$ID_ENTIDAD/100)
muni_2017$ID_MUNICIPIO <- muni_2017$ID_MUNICIPIO/100000
muni_2017$CVE_INEGI <- specify_decimal((muni_2017$ID_ENTIDAD+muni_2017$ID_MUNICIPIO), 5)
muni_2017$CVE_INEGI <- as.character(muni_2017$CVE_INEGI)
muni_2017$CVE_INEGI <- str_remove(muni_2017$CVE_INEGI , "0.")


muni_2017 <-  select(muni_2017, CVE_INEGI, DESCRIPCION_CATEGORIA, VALOR)



muni_2017_2 <- filter(muni_2017, DESCRIPCION_CATEGORIA == c( "Impuestos"))

muni_2017_2 <- spread(muni_2017_2, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2017_3 <- filter(muni_2017, DESCRIPCION_CATEGORIA == c( "Participaciones federales"))

muni_2017_3 <- spread(muni_2017_3, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2017_4 <- filter(muni_2017, DESCRIPCION_CATEGORIA == c( "Impuesto predial"))

muni_2017_4 <- spread(muni_2017_4, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2017_5 <- filter(muni_2017, DESCRIPCION_CATEGORIA == c( "Aportaciones federales y estatales"))

muni_2017_5 <- spread(muni_2017_5, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2017_6 <- filter(muni_2017, DESCRIPCION_CATEGORIA == c( "Total de egresos"))

muni_2017_6 <- spread(muni_2017_6, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2017_7 <- filter(muni_2017, DESCRIPCION_CATEGORIA == c( "Total de ingresos"))

muni_2017_7 <- spread(muni_2017_7, DESCRIPCION_CATEGORIA, VALOR)  

efipem_2017 <- full_join(muni_2017_2, muni_2017_3)
efipem_2017 <- full_join(efipem_2017, muni_2017_4)
efipem_2017 <- full_join(efipem_2017, muni_2017_5)
efipem_2017 <- full_join(efipem_2017, muni_2017_6)
efipem_2017 <- full_join(efipem_2017, muni_2017_7)
efipem_2017 <- full_join(efipem_2017, nombres)
efipem_2017$year <- rep(2017, length(efipem_2017$CVE_INEGI))


### 2018



muni_2018 <- as.data.frame(read.csv("efipem_municipal_anual_tr_cifra_2018.csv" , stringsAsFactors = TRUE,  encoding = "UTF-8"))

muni_2018$ID_ENTIDAD <- (muni_2018$ID_ENTIDAD/100)
muni_2018$ID_MUNICIPIO <- muni_2018$ID_MUNICIPIO/100000
muni_2018$CVE_INEGI <- specify_decimal((muni_2018$ID_ENTIDAD+muni_2018$ID_MUNICIPIO), 5)
muni_2018$CVE_INEGI <- as.character(muni_2018$CVE_INEGI)
muni_2018$CVE_INEGI <- str_remove(muni_2018$CVE_INEGI , "0.")


muni_2018 <-  select(muni_2018, CVE_INEGI, DESCRIPCION_CATEGORIA, VALOR)



muni_2018_2 <- filter(muni_2018, DESCRIPCION_CATEGORIA == c( "Impuestos"))

muni_2018_2 <- spread(muni_2018_2, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2018_3 <- filter(muni_2018, DESCRIPCION_CATEGORIA == c( "Participaciones federales"))

muni_2018_3 <- spread(muni_2018_3, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2018_4 <- filter(muni_2018, DESCRIPCION_CATEGORIA == c( "Impuesto predial"))

muni_2018_4 <- spread(muni_2018_4, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2018_5 <- filter(muni_2018, DESCRIPCION_CATEGORIA == c( "Aportaciones federales y estatales"))

muni_2018_5 <- spread(muni_2018_5, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2018_6 <- filter(muni_2018, DESCRIPCION_CATEGORIA == c( "Total de egresos"))

muni_2018_6 <- spread(muni_2018_6, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2018_7 <- filter(muni_2018, DESCRIPCION_CATEGORIA == c( "Total de ingresos"))

muni_2018_7 <- spread(muni_2018_7, DESCRIPCION_CATEGORIA, VALOR)  

efipem_2018 <- full_join(muni_2018_2, muni_2018_3)
efipem_2018 <- full_join(efipem_2018, muni_2018_4)
efipem_2018 <- full_join(efipem_2018, muni_2018_5)
efipem_2018 <- full_join(efipem_2018, muni_2018_6)
efipem_2018 <- full_join(efipem_2018, muni_2018_7)
efipem_2018 <- full_join(efipem_2018, nombres)
efipem_2018$year <- rep(2018, length(efipem_2018$CVE_INEGI))


#2019





muni_2019 <- as.data.frame(read.csv("efipem_municipal_anual_tr_cifra_2019.csv" , stringsAsFactors = TRUE,  encoding = "UTF-8"))

muni_2019$ID_ENTIDAD <- (muni_2019$ID_ENTIDAD/100)
muni_2019$ID_MUNICIPIO <- muni_2019$ID_MUNICIPIO/100000
muni_2019$CVE_INEGI <- specify_decimal((muni_2019$ID_ENTIDAD+muni_2019$ID_MUNICIPIO), 5)
muni_2019$CVE_INEGI <- as.character(muni_2019$CVE_INEGI)
muni_2019$CVE_INEGI <- str_remove(muni_2019$CVE_INEGI , "0.")


muni_2019 <-  select(muni_2019, CVE_INEGI, DESCRIPCION_CATEGORIA, VALOR)



muni_2019_2 <- filter(muni_2019, DESCRIPCION_CATEGORIA == c( "Impuestos"))

muni_2019_2 <- spread(muni_2019_2, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2019_3 <- filter(muni_2019, DESCRIPCION_CATEGORIA == c( "Participaciones federales"))

muni_2019_3 <- spread(muni_2019_3, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2019_4 <- filter(muni_2019, DESCRIPCION_CATEGORIA == c( "Impuesto predial"))

muni_2019_4 <- spread(muni_2019_4, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2019_5 <- filter(muni_2019, DESCRIPCION_CATEGORIA == c( "Aportaciones federales y estatales"))

muni_2019_5 <- spread(muni_2019_5, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2019_6 <- filter(muni_2019, DESCRIPCION_CATEGORIA == c( "Total de egresos"))

muni_2019_6 <- spread(muni_2019_6, DESCRIPCION_CATEGORIA, VALOR)  
#
muni_2019_7 <- filter(muni_2019, DESCRIPCION_CATEGORIA == c( "Total de ingresos"))

muni_2019_7 <- spread(muni_2019_7, DESCRIPCION_CATEGORIA, VALOR)  

efipem_2019 <- full_join(muni_2019_2, muni_2019_3)
efipem_2019 <- full_join(efipem_2019, muni_2019_4)
efipem_2019 <- full_join(efipem_2019, muni_2019_5)
efipem_2019 <- full_join(efipem_2019, muni_2019_6)
efipem_2019 <- full_join(efipem_2019, muni_2019_7)
efipem_2019 <- full_join(efipem_2019, nombres)
efipem_2019$year <- rep(2019, length(efipem_2019$CVE_INEGI))

#### Let's throw these puppies together.

efipem_mun <- full_join(efipem_1989, efipem_1990)
efipem_mun <- full_join(efipem_mun, efipem_1991)
efipem_mun <- full_join(efipem_mun, efipem_1992)
efipem_mun <- full_join(efipem_mun, efipem_1993)
efipem_mun <- full_join(efipem_mun, efipem_1994)
efipem_mun <- full_join(efipem_mun, efipem_1995)
efipem_mun <- full_join(efipem_mun, efipem_1996)
efipem_mun <- full_join(efipem_mun, efipem_1997)
efipem_mun <- full_join(efipem_mun, efipem_1998)
efipem_mun <- full_join(efipem_mun, efipem_1999)
efipem_mun <- full_join(efipem_mun, efipem_2000)
efipem_mun <- full_join(efipem_mun, efipem_2001)
efipem_mun <- full_join(efipem_mun, efipem_2002)
efipem_mun <- full_join(efipem_mun, efipem_2003)
efipem_mun <- full_join(efipem_mun, efipem_2004)
efipem_mun <- full_join(efipem_mun, efipem_2005)
efipem_mun <- full_join(efipem_mun, efipem_2006)
efipem_mun <- full_join(efipem_mun, efipem_2007)
efipem_mun <- full_join(efipem_mun, efipem_2008)
efipem_mun <- full_join(efipem_mun, efipem_2009)
efipem_mun <- full_join(efipem_mun, efipem_2010)
efipem_mun <- full_join(efipem_mun, efipem_2011)
efipem_mun <- full_join(efipem_mun, efipem_2012)
efipem_mun <- full_join(efipem_mun, efipem_2013)
efipem_mun <- full_join(efipem_mun, efipem_2014)
efipem_mun <- full_join(efipem_mun, efipem_2015)
efipem_mun <- full_join(efipem_mun, efipem_2016)
efipem_mun <- full_join(efipem_mun, efipem_2017)
efipem_mun <- full_join(efipem_mun, efipem_2018)
efipem_mun <- full_join(efipem_mun, efipem_2019)


efipem_mun <- efipem_mun[order(efipem_mun$CVE_INEGI , efipem_mun$year),]

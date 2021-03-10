### Dataset for alcaldia Finance 
library(plyr)
library(dplyr)
library(rowr)
library(car)
library(tidyr)
setwd("G:/My Drive/Dissertation/Data/finanzas_muni")

nombres <- read.csv("G:/My Drive/Dissertation/Data/finanzas_muni/efipem_alcaldias_csv/catalogos/tc_alcaldia.csv" , stringsAsFactors = TRUE, encoding = "UTF-8")

specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))
names(nombres)

nombres$X.U.FEFF.ID_ENTIDAD <- nombres$X.U.FEFF.ID_ENTIDAD/100
nombres$ID_ALCALDIA <- nombres$ID_ALCALDIA/100000
nombres$CVE_INEGI <- specify_decimal((nombres$X.U.FEFF.ID_ENTIDAD+nombres$ID_ALCALDIA), 5)

nombres$CVE_INEGI <- as.character(nombres$CVE_INEGI)
nombres$CVE_INEGI <- str_remove(nombres$CVE_INEGI , "0.")

keep <- c("NOM_ALC", "CVE_INEGI")
nombres <- nombres[keep]


## Pull the data in
setwd("G:/My Drive/Dissertation/Data/finanzas_muni/efipem_alcaldias_csv/conjunto_de_datos")

files = list.files(pattern="*.csv")
files

### 1999
muni_1999 <- as.data.frame(read.csv("efipem_alcaldia_anual_tr_cifra_1999.csv" , stringsAsFactors = TRUE,  encoding = "UTF-8"))

muni_1999$ID_ENTIDAD <- (muni_1999$ID_ENTIDAD/100)
muni_1999$ID_ALCALDIA <- muni_1999$ID_ALCALDIA/100000
muni_1999$CVE_INEGI <- specify_decimal((muni_1999$ID_ENTIDAD+muni_1999$ID_ALCALDIA), 5)
muni_1999$CVE_INEGI <- as.character(muni_1999$CVE_INEGI)
muni_1999$CVE_INEGI <- str_remove(muni_1999$CVE_INEGI , "0.")


muni_1999 <-  select(muni_1999, CVE_INEGI, DESCRIPCION_CATEGORIA, VALOR)



muni_1999_2 <- filter(muni_1999, DESCRIPCION_CATEGORIA == c("Impuestos y derechos"))

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
muni_2000 <- as.data.frame(read.csv("efipem_alcaldia_anual_tr_cifra_2000.csv" , stringsAsFactors = TRUE,  encoding = "UTF-8"))

muni_2000$ID_ENTIDAD <- (muni_2000$ID_ENTIDAD/100)
muni_2000$ID_ALCALDIA <- muni_2000$ID_ALCALDIA/100000
muni_2000$CVE_INEGI <- specify_decimal((muni_2000$ID_ENTIDAD+muni_2000$ID_ALCALDIA), 5)
muni_2000$CVE_INEGI <- as.character(muni_2000$CVE_INEGI)
muni_2000$CVE_INEGI <- str_remove(muni_2000$CVE_INEGI , "0.")


muni_2000 <-  select(muni_2000, CVE_INEGI, DESCRIPCION_CATEGORIA, VALOR)



muni_2000_2 <- filter(muni_2000, DESCRIPCION_CATEGORIA == c( "Impuestos y derechos"))

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
muni_2001 <- as.data.frame(read.csv("efipem_alcaldia_anual_tr_cifra_2001.csv" , stringsAsFactors = TRUE,  encoding = "UTF-8"))

muni_2001$ID_ENTIDAD <- (muni_2001$ID_ENTIDAD/100)
muni_2001$ID_ALCALDIA <- muni_2001$ID_ALCALDIA/100000
muni_2001$CVE_INEGI <- specify_decimal((muni_2001$ID_ENTIDAD+muni_2001$ID_ALCALDIA), 5)
muni_2001$CVE_INEGI <- as.character(muni_2001$CVE_INEGI)
muni_2001$CVE_INEGI <- str_remove(muni_2001$CVE_INEGI , "0.")


muni_2001 <-  select(muni_2001, CVE_INEGI, DESCRIPCION_CATEGORIA, VALOR)



muni_2001_2 <- filter(muni_2001, DESCRIPCION_CATEGORIA == c( "Impuestos y derechos"))

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
muni_2002 <- as.data.frame(read.csv("efipem_alcaldia_anual_tr_cifra_2002.csv" , stringsAsFactors = TRUE,  encoding = "UTF-8"))

muni_2002$ID_ENTIDAD <- (muni_2002$ID_ENTIDAD/100)
muni_2002$ID_ALCALDIA <- muni_2002$ID_ALCALDIA/100000
muni_2002$CVE_INEGI <- specify_decimal((muni_2002$ID_ENTIDAD+muni_2002$ID_ALCALDIA), 5)
muni_2002$CVE_INEGI <- as.character(muni_2002$CVE_INEGI)
muni_2002$CVE_INEGI <- str_remove(muni_2002$CVE_INEGI , "0.")


muni_2002 <-  select(muni_2002, CVE_INEGI, DESCRIPCION_CATEGORIA, VALOR)



muni_2002_2 <- filter(muni_2002, DESCRIPCION_CATEGORIA == c( "Impuestos y derechos"))

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
muni_2003 <- as.data.frame(read.csv("efipem_alcaldia_anual_tr_cifra_2003.csv" , stringsAsFactors = TRUE,  encoding = "UTF-8"))

muni_2003$ID_ENTIDAD <- (muni_2003$ID_ENTIDAD/100)
muni_2003$ID_ALCALDIA <- muni_2003$ID_ALCALDIA/100000
muni_2003$CVE_INEGI <- specify_decimal((muni_2003$ID_ENTIDAD+muni_2003$ID_ALCALDIA), 5)
muni_2003$CVE_INEGI <- as.character(muni_2003$CVE_INEGI)
muni_2003$CVE_INEGI <- str_remove(muni_2003$CVE_INEGI , "0.")


muni_2003 <-  select(muni_2003, CVE_INEGI, DESCRIPCION_CATEGORIA, VALOR)



muni_2003_2 <- filter(muni_2003, DESCRIPCION_CATEGORIA == c( "Impuestos y derechos"))

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
muni_2004 <- as.data.frame(read.csv("efipem_alcaldia_anual_tr_cifra_2004.csv" , stringsAsFactors = TRUE,  encoding = "UTF-8"))

muni_2004$ID_ENTIDAD <- (muni_2004$ID_ENTIDAD/100)
muni_2004$ID_ALCALDIA <- muni_2004$ID_ALCALDIA/100000
muni_2004$CVE_INEGI <- specify_decimal((muni_2004$ID_ENTIDAD+muni_2004$ID_ALCALDIA), 5)
muni_2004$CVE_INEGI <- as.character(muni_2004$CVE_INEGI)
muni_2004$CVE_INEGI <- str_remove(muni_2004$CVE_INEGI , "0.")


muni_2004 <-  select(muni_2004, CVE_INEGI, DESCRIPCION_CATEGORIA, VALOR)



muni_2004_2 <- filter(muni_2004, DESCRIPCION_CATEGORIA == c( "Impuestos y derechos"))

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
muni_2005 <- as.data.frame(read.csv("efipem_alcaldia_anual_tr_cifra_2005.csv" , stringsAsFactors = TRUE,  encoding = "UTF-8"))

muni_2005$ID_ENTIDAD <- (muni_2005$ID_ENTIDAD/100)
muni_2005$ID_ALCALDIA <- muni_2005$ID_ALCALDIA/100000
muni_2005$CVE_INEGI <- specify_decimal((muni_2005$ID_ENTIDAD+muni_2005$ID_ALCALDIA), 5)
muni_2005$CVE_INEGI <- as.character(muni_2005$CVE_INEGI)
muni_2005$CVE_INEGI <- str_remove(muni_2005$CVE_INEGI , "0.")


muni_2005 <-  select(muni_2005, CVE_INEGI, DESCRIPCION_CATEGORIA, VALOR)



muni_2005_2 <- filter(muni_2005, DESCRIPCION_CATEGORIA == c( "Impuestos y derechos"))

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
muni_2006 <- as.data.frame(read.csv("efipem_alcaldia_anual_tr_cifra_2006.csv" , stringsAsFactors = TRUE,  encoding = "UTF-8"))

muni_2006$ID_ENTIDAD <- (muni_2006$ID_ENTIDAD/100)
muni_2006$ID_ALCALDIA <- muni_2006$ID_ALCALDIA/100000
muni_2006$CVE_INEGI <- specify_decimal((muni_2006$ID_ENTIDAD+muni_2006$ID_ALCALDIA), 5)
muni_2006$CVE_INEGI <- as.character(muni_2006$CVE_INEGI)
muni_2006$CVE_INEGI <- str_remove(muni_2006$CVE_INEGI , "0.")


muni_2006 <-  select(muni_2006, CVE_INEGI, DESCRIPCION_CATEGORIA, VALOR)



muni_2006_2 <- filter(muni_2006, DESCRIPCION_CATEGORIA == c( "Impuestos y derechos"))

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
muni_2007 <- as.data.frame(read.csv("efipem_alcaldia_anual_tr_cifra_2007.csv" , stringsAsFactors = TRUE,  encoding = "UTF-8"))

muni_2007$ID_ENTIDAD <- (muni_2007$ID_ENTIDAD/100)
muni_2007$ID_ALCALDIA <- muni_2007$ID_ALCALDIA/100000
muni_2007$CVE_INEGI <- specify_decimal((muni_2007$ID_ENTIDAD+muni_2007$ID_ALCALDIA), 5)
muni_2007$CVE_INEGI <- as.character(muni_2007$CVE_INEGI)
muni_2007$CVE_INEGI <- str_remove(muni_2007$CVE_INEGI , "0.")


muni_2007 <-  select(muni_2007, CVE_INEGI, DESCRIPCION_CATEGORIA, VALOR)



muni_2007_2 <- filter(muni_2007, DESCRIPCION_CATEGORIA == c( "Impuestos y derechos"))

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
muni_2008 <- as.data.frame(read.csv("efipem_alcaldia_anual_tr_cifra_2008.csv" , stringsAsFactors = TRUE,  encoding = "UTF-8"))

muni_2008$ID_ENTIDAD <- (muni_2008$ID_ENTIDAD/100)
muni_2008$ID_ALCALDIA <- muni_2008$ID_ALCALDIA/100000
muni_2008$CVE_INEGI <- specify_decimal((muni_2008$ID_ENTIDAD+muni_2008$ID_ALCALDIA), 5)
muni_2008$CVE_INEGI <- as.character(muni_2008$CVE_INEGI)
muni_2008$CVE_INEGI <- str_remove(muni_2008$CVE_INEGI , "0.")


muni_2008 <-  select(muni_2008, CVE_INEGI, DESCRIPCION_CATEGORIA, VALOR)



muni_2008_2 <- filter(muni_2008, DESCRIPCION_CATEGORIA == c( "Impuestos y derechos"))

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
muni_2009 <- as.data.frame(read.csv("efipem_alcaldia_anual_tr_cifra_2009.csv" , stringsAsFactors = TRUE,  encoding = "UTF-8"))

muni_2009$ID_ENTIDAD <- (muni_2009$ID_ENTIDAD/100)
muni_2009$ID_ALCALDIA <- muni_2009$ID_ALCALDIA/100000
muni_2009$CVE_INEGI <- specify_decimal((muni_2009$ID_ENTIDAD+muni_2009$ID_ALCALDIA), 5)
muni_2009$CVE_INEGI <- as.character(muni_2009$CVE_INEGI)
muni_2009$CVE_INEGI <- str_remove(muni_2009$CVE_INEGI , "0.")


muni_2009 <-  select(muni_2009, CVE_INEGI, DESCRIPCION_CATEGORIA, VALOR)



muni_2009_2 <- filter(muni_2009, DESCRIPCION_CATEGORIA == c( "Impuestos y derechos"))

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
muni_2010 <- as.data.frame(read.csv("efipem_alcaldia_anual_tr_cifra_2010.csv" , stringsAsFactors = TRUE,  encoding = "UTF-8"))

muni_2010$ID_ENTIDAD <- (muni_2010$ID_ENTIDAD/100)
muni_2010$ID_ALCALDIA <- muni_2010$ID_ALCALDIA/100000
muni_2010$CVE_INEGI <- specify_decimal((muni_2010$ID_ENTIDAD+muni_2010$ID_ALCALDIA), 5)
muni_2010$CVE_INEGI <- as.character(muni_2010$CVE_INEGI)
muni_2010$CVE_INEGI <- str_remove(muni_2010$CVE_INEGI , "0.")


muni_2010 <-  select(muni_2010, CVE_INEGI, DESCRIPCION_CATEGORIA, VALOR)



muni_2010_2 <- filter(muni_2010, DESCRIPCION_CATEGORIA == c( "Impuestos y derechos"))

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
muni_2011 <- as.data.frame(read.csv("efipem_alcaldia_anual_tr_cifra_2011.csv" , stringsAsFactors = TRUE,  encoding = "UTF-8"))

muni_2011$ID_ENTIDAD <- (muni_2011$ID_ENTIDAD/100)
muni_2011$ID_ALCALDIA <- muni_2011$ID_ALCALDIA/100000
muni_2011$CVE_INEGI <- specify_decimal((muni_2011$ID_ENTIDAD+muni_2011$ID_ALCALDIA), 5)
muni_2011$CVE_INEGI <- as.character(muni_2011$CVE_INEGI)
muni_2011$CVE_INEGI <- str_remove(muni_2011$CVE_INEGI , "0.")


muni_2011 <-  select(muni_2011, CVE_INEGI, DESCRIPCION_CATEGORIA, VALOR)



muni_2011_2 <- filter(muni_2011, DESCRIPCION_CATEGORIA == c( "Impuestos y derechos"))

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
muni_2012 <- as.data.frame(read.csv("efipem_alcaldia_anual_tr_cifra_2012.csv" , stringsAsFactors = TRUE,  encoding = "UTF-8"))

muni_2012$ID_ENTIDAD <- (muni_2012$ID_ENTIDAD/100)
muni_2012$ID_ALCALDIA <- muni_2012$ID_ALCALDIA/100000
muni_2012$CVE_INEGI <- specify_decimal((muni_2012$ID_ENTIDAD+muni_2012$ID_ALCALDIA), 5)
muni_2012$CVE_INEGI <- as.character(muni_2012$CVE_INEGI)
muni_2012$CVE_INEGI <- str_remove(muni_2012$CVE_INEGI , "0.")


muni_2012 <-  select(muni_2012, CVE_INEGI, DESCRIPCION_CATEGORIA, VALOR)



muni_2012_2 <- filter(muni_2012, DESCRIPCION_CATEGORIA == c( "Impuestos y derechos"))

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
muni_2013 <- as.data.frame(read.csv("efipem_alcaldia_anual_tr_cifra_2013.csv" , stringsAsFactors = TRUE,  encoding = "UTF-8"))

muni_2013$ID_ENTIDAD <- (muni_2013$ID_ENTIDAD/100)
muni_2013$ID_ALCALDIA <- muni_2013$ID_ALCALDIA/100000
muni_2013$CVE_INEGI <- specify_decimal((muni_2013$ID_ENTIDAD+muni_2013$ID_ALCALDIA), 5)
muni_2013$CVE_INEGI <- as.character(muni_2013$CVE_INEGI)
muni_2013$CVE_INEGI <- str_remove(muni_2013$CVE_INEGI , "0.")


muni_2013 <-  select(muni_2013, CVE_INEGI, DESCRIPCION_CATEGORIA, VALOR)



muni_2013_2 <- filter(muni_2013, DESCRIPCION_CATEGORIA == c( "Impuestos y derechos"))

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
muni_2014 <- as.data.frame(read.csv("efipem_alcaldia_anual_tr_cifra_2014.csv" , stringsAsFactors = TRUE,  encoding = "UTF-8"))

muni_2014$ID_ENTIDAD <- (muni_2014$ID_ENTIDAD/100)
muni_2014$ID_ALCALDIA <- muni_2014$ID_ALCALDIA/100000
muni_2014$CVE_INEGI <- specify_decimal((muni_2014$ID_ENTIDAD+muni_2014$ID_ALCALDIA), 5)
muni_2014$CVE_INEGI <- as.character(muni_2014$CVE_INEGI)
muni_2014$CVE_INEGI <- str_remove(muni_2014$CVE_INEGI , "0.")


muni_2014 <-  select(muni_2014, CVE_INEGI, DESCRIPCION_CATEGORIA, VALOR)



muni_2014_2 <- filter(muni_2014, DESCRIPCION_CATEGORIA == c( "Impuestos y derechos"))

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
muni_2015 <- as.data.frame(read.csv("efipem_alcaldia_anual_tr_cifra_2015.csv" , stringsAsFactors = TRUE,  encoding = "UTF-8"))

muni_2015$ID_ENTIDAD <- (muni_2015$ID_ENTIDAD/100)
muni_2015$ID_ALCALDIA <- muni_2015$ID_ALCALDIA/100000
muni_2015$CVE_INEGI <- specify_decimal((muni_2015$ID_ENTIDAD+muni_2015$ID_ALCALDIA), 5)
muni_2015$CVE_INEGI <- as.character(muni_2015$CVE_INEGI)
muni_2015$CVE_INEGI <- str_remove(muni_2015$CVE_INEGI , "0.")


muni_2015 <-  select(muni_2015, CVE_INEGI, DESCRIPCION_CATEGORIA, VALOR)



muni_2015_2 <- filter(muni_2015, DESCRIPCION_CATEGORIA == c( "Impuestos y derechos"))

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
muni_2016 <- as.data.frame(read.csv("efipem_alcaldia_anual_tr_cifra_2016.csv" , stringsAsFactors = TRUE,  encoding = "UTF-8"))

muni_2016$ID_ENTIDAD <- (muni_2016$ID_ENTIDAD/100)
muni_2016$ID_ALCALDIA <- muni_2016$ID_ALCALDIA/100000
muni_2016$CVE_INEGI <- specify_decimal((muni_2016$ID_ENTIDAD+muni_2016$ID_ALCALDIA), 5)
muni_2016$CVE_INEGI <- as.character(muni_2016$CVE_INEGI)
muni_2016$CVE_INEGI <- str_remove(muni_2016$CVE_INEGI , "0.")


muni_2016 <-  select(muni_2016, CVE_INEGI, DESCRIPCION_CATEGORIA, VALOR)



muni_2016_2 <- filter(muni_2016, DESCRIPCION_CATEGORIA == c( "Impuestos y derechos"))

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
muni_2017 <- as.data.frame(read.csv("efipem_alcaldia_anual_tr_cifra_2017.csv" , stringsAsFactors = TRUE,  encoding = "UTF-8"))

muni_2017$ID_ENTIDAD <- (muni_2017$ID_ENTIDAD/100)
muni_2017$ID_ALCALDIA <- muni_2017$ID_ALCALDIA/100000
muni_2017$CVE_INEGI <- specify_decimal((muni_2017$ID_ENTIDAD+muni_2017$ID_ALCALDIA), 5)
muni_2017$CVE_INEGI <- as.character(muni_2017$CVE_INEGI)
muni_2017$CVE_INEGI <- str_remove(muni_2017$CVE_INEGI , "0.")


muni_2017 <-  select(muni_2017, CVE_INEGI, DESCRIPCION_CATEGORIA, VALOR)



muni_2017_2 <- filter(muni_2017, DESCRIPCION_CATEGORIA == c( "Impuestos y derechos"))

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



muni_2018 <- as.data.frame(read.csv("efipem_alcaldia_anual_tr_cifra_2018.csv" , stringsAsFactors = TRUE,  encoding = "UTF-8"))

muni_2018$ID_ENTIDAD <- (muni_2018$ID_ENTIDAD/100)
muni_2018$ID_ALCALDIA <- muni_2018$ID_ALCALDIA/100000
muni_2018$CVE_INEGI <- specify_decimal((muni_2018$ID_ENTIDAD+muni_2018$ID_ALCALDIA), 5)
muni_2018$CVE_INEGI <- as.character(muni_2018$CVE_INEGI)
muni_2018$CVE_INEGI <- str_remove(muni_2018$CVE_INEGI , "0.")


muni_2018 <-  select(muni_2018, CVE_INEGI, DESCRIPCION_CATEGORIA, VALOR)



muni_2018_2 <- filter(muni_2018, DESCRIPCION_CATEGORIA == c( "Impuestos y derechos"))

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





muni_2019 <- as.data.frame(read.csv("efipem_alcaldia_anual_tr_cifra_2019.csv" , stringsAsFactors = TRUE,  encoding = "UTF-8"))

muni_2019$ID_ENTIDAD <- (muni_2019$ID_ENTIDAD/100)
muni_2019$ID_ALCALDIA <- muni_2019$ID_ALCALDIA/100000
muni_2019$CVE_INEGI <- specify_decimal((muni_2019$ID_ENTIDAD+muni_2019$ID_ALCALDIA), 5)
muni_2019$CVE_INEGI <- as.character(muni_2019$CVE_INEGI)
muni_2019$CVE_INEGI <- str_remove(muni_2019$CVE_INEGI , "0.")


muni_2019 <-  select(muni_2019, CVE_INEGI, DESCRIPCION_CATEGORIA, VALOR)



muni_2019_2 <- filter(muni_2019, DESCRIPCION_CATEGORIA == c( "Impuestos y derechos"))

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


efipem_alc <- full_join(efipem_1999, efipem_2000)
efipem_alc <- full_join(efipem_alc, efipem_2001)
efipem_alc <- full_join(efipem_alc, efipem_2002)
efipem_alc <- full_join(efipem_alc, efipem_2003)
efipem_alc <- full_join(efipem_alc, efipem_2004)
efipem_alc <- full_join(efipem_alc, efipem_2005)
efipem_alc <- full_join(efipem_alc, efipem_2006)
efipem_alc <- full_join(efipem_alc, efipem_2007)
efipem_alc <- full_join(efipem_alc, efipem_2008)
efipem_alc <- full_join(efipem_alc, efipem_2009)
efipem_alc <- full_join(efipem_alc, efipem_2010)
efipem_alc <- full_join(efipem_alc, efipem_2011)
efipem_alc <- full_join(efipem_alc, efipem_2012)
efipem_alc <- full_join(efipem_alc, efipem_2013)
efipem_alc <- full_join(efipem_alc, efipem_2014)
efipem_alc <- full_join(efipem_alc, efipem_2015)
efipem_alc <- full_join(efipem_alc, efipem_2016)
efipem_alc <- full_join(efipem_alc, efipem_2017)
efipem_alc <- full_join(efipem_alc, efipem_2018)
efipem_alc <- full_join(efipem_alc, efipem_2019)


efipem_alc <- efipem_alc[order(efipem_alc$CVE_INEGI , efipem_alc$year),]

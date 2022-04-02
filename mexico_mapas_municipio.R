


library(tidyverse)
library(readxl)
library(sjPlot)
# library(DescTools)
library(tidyverse)
# library(xlsx)

setwd("C:/Users/alexb/Documents/github/Mexico-Municipal-Finance-Data")
efipem <- read.csv("efipem_mun_data_v4.csv", encoding = "utf-8")%>%
  dplyr::select(-X)%>%
  rename(Inversion.publica = `Inversión.pública`)

specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))

efipem$ID_ENTIDAD <- efipem$ID_ENTIDAD/100
efipem$ID_MUNICIPIO <- efipem$ID_MUNICIPIO/100000
efipem$CVE_INEGI <- specify_decimal((efipem$ID_ENTIDAD+efipem$ID_MUNICIPIO), 5)
efipem$CVE_INEGI <- as.character(efipem$CVE_INEGI)
efipem$CVE_INEGI <- str_remove(efipem$CVE_INEGI , "0.")
efipem$CVE_INEGI <- as.character(as.numeric(efipem$CVE_INEGI))


claves <- efipem%>%
  dplyr::select(CVE_INEGI, ANIO)%>%
  unique()

poblacion <- read.csv("pob_mun_mex_v1.csv", encoding = "utf-8")%>%
  dplyr::select(-X)%>%
  mutate(CVE_INEGI = as.character(CVE_INEGI))
colnames(efipem)

mun <- full_join(poblacion, efipem)%>%
  # filter(ANIO != "2020")%>%
  ungroup()%>%
  mutate(ingresos_propios = rowSums(dplyr::select(.,Aprovechamientos,Contribuciones.de.Mejoras,
                                                  Cuotas.y.Aportaciones.de.Seguridad.Social,
                                                  Derechos, Impuestos,Productos, `Otros.ingresos`), na.rm=T),
         participaciones_no_regalias = as.numeric(Participaciones.federales),
         aportaciones_33_capital = rowSums(dplyr::select(., FA.para.la.Infraestructura.Social.Municipal, FA.para.el.Fortalecimiento.de.los.Municipios), na.rm=T),
         recursos_federales_reasignados = as.numeric(Recursos.federales.y.estatales.reasignados),
         transferencias_condicionadas = aportaciones_33_capital, 
         transferencias_no_condicionadas = participaciones_no_regalias ,
         transferencias_discrecionales = recursos_federales_reasignados,
         financiamiento = Financiamiento,
         gasto_total_munis = Total.de.egresos,
         inversion_publica = Inversion.publica, 
         ingresos_predial_tax = Impuesto.predial,
         gasto_corriente = gasto_total_munis - inversion_publica,
         ingresos_total_munis = Total.de.ingresos,  #Ingresos_menos_finanzas,
         pais = rep("Mexico")
  )%>%
  mutate(total_transferencias = rowSums(dplyr::select(.,participaciones_no_regalias, aportaciones_33_capital, recursos_federales_reasignados),na.rm=T))%>%
  arrange( estado, municipio,ANIO)%>%
  group_by(estado, municipio)%>%
  filter(ingresos_propios>0)%>%
  rename(year = ANIO)%>%
  select(CVE_INEGI, year, estado, municipio, poblacion, ingresos_predial_tax, Impuestos, Productos, Derechos, Aprovechamientos, ingresos_total_munis,financiamiento, aportaciones_33_capital, recursos_federales_reasignados,
         participaciones_no_regalias, total_transferencias, inversion_publica, gasto_corriente, gasto_total_munis,
         ingresos_propios, total_transferencias)

# Gini
mun_names <- read.csv("https://raw.githubusercontent.com/AlejandroBeltranA/diss_data/main/datos_municipales_add_v1.csv") %>%
  select(NOM_MUN, CVE_INEGI, year, Gini, IRS, pob_lengua_indigena)
# Drop variables not related to current analysis. 
mun_names <- mun_names%>%
  mutate(Gini = case_when(
    CVE_INEGI== 23010 ~ 0.66, #Gini for Bacalar
    CVE_INEGI== 23009 ~ 0.40, # Gini for Tulum
    CVE_INEGI== 14125 ~ 0.42, # Gini for San Ignacion Cerro Gordo
    TRUE ~ Gini
  ))%>%
  filter(CVE_INEGI != "23011")%>% # Puerto Morelos didn't exist prior to 2016.
  mutate(CVE_INEGI = as.character(CVE_INEGI))


data <- full_join(mun, mun_names)%>%
  select(CVE_INEGI, estado, municipio, year, poblacion, ingresos_total_munis,
         ingresos_propios, total_transferencias, financiamiento, gasto_total_munis, gasto_corriente, inversion_publica)%>%
  mutate(ing_propios_de_total_mun = ingresos_propios/ingresos_total_munis,
         ing_transferencias_de_total_mun = total_transferencias/ingresos_total_munis,
         ing_financiamiento_de_total_mun = financiamiento/ingresos_total_munis,
         egr_corriente_de_total_mun = gasto_corriente/gasto_total_munis,
         egre_inversion_de_total_mun = inversion_publica/gasto_total_munis)%>%
  relocate(CVE_INEGI, estado, municipio, year, ing_propios_de_total_mun, ing_transferencias_de_total_mun, ing_financiamiento_de_total_mun,
         egr_corriente_de_total_mun, egre_inversion_de_total_mun)


colnames(data)
# install.packages("cli")
# install.packages("devtools", type = "win.binary")
# devtools::install_github("diegovalle/mxmaps")





ing <- data%>%
  ungroup()%>%
  select(year, ing_propios_de_total_mun, ing_transferencias_de_total_mun, ing_financiamiento_de_total_mun)%>%
  group_by(year)%>%
  summarise_at(vars(ing_propios_de_total_mun, ing_transferencias_de_total_mun, ing_financiamiento_de_total_mun),
               list(mean),na.rm=T)%>%
  pivot_longer(cols = c("ing_propios_de_total_mun", "ing_transferencias_de_total_mun", "ing_financiamiento_de_total_mun"),
               names_to = "var",
               values_to = "valor")%>%
  # filter(!var == "ingresos_total_munis")%>%
  ggplot(aes(x=year, y=valor, fill = var))+
  geom_area(stat= "identity")+
  theme_classic()+
  theme(legend.position="bottom",
        legend.title=element_blank())+
  ggtitle("Evolucion Ingresos Municipales")+
  xlab("")+
  ylab("MXN$")
ing

eg <- data%>%
  ungroup()%>%
  select(year, egr_corriente_de_total_mun, egre_inversion_de_total_mun)%>%
  group_by(year)%>%
  summarise_at(vars(egr_corriente_de_total_mun, egre_inversion_de_total_mun),
               list(mean),na.rm=T)%>%
  pivot_longer(cols = c("egr_corriente_de_total_mun", "egre_inversion_de_total_mun"),
               names_to = "var",
               values_to = "valor")%>%
  # filter(!var == "gasto_total_munis")%>%
  ggplot(aes(x=year, y=valor, fill = var))+
  geom_area(stat= "identity")+
  theme_classic()+
  theme(legend.position="bottom",
        legend.title=element_blank())+
  ggtitle("Evolucion Egresos Municipales")+
  xlab("")+
  ylab("MXN$")
eg



library("mxmaps")


data("df_mxmunicipio_2020")
geo_data <- df_mxmunicipio_2020%>%
  mutate(CVE_INEGI = as.character(as.numeric(region)))%>%
  select(CVE_INEGI,state_name, municipio_name, long, lat)%>%
  full_join(data)

sub <- geo_data%>%
  filter(year==2020)%>%
  unique()%>%
  mutate(value = ing_propios_de_total_mun,
         region = CVE_INEGI)%>%
  mxmunicipio_choropleth(., 
                         zoom = subset(., state_name %in% 
                                         c("Sinaloa"))$region,
                       title = "",
                       legend = "Ingresos Propios")
sub


full <- geo_data%>%
  filter(year==2019)%>%
  unique()%>%
  mutate(value = ing_propios_de_total_mun,
         region = CVE_INEGI)%>%
  mxmunicipio_choropleth(., 
                         # zoom = subset(., state_name %in% 
                         #                 c("Sinaloa"))$region,
                         title = "",
                         legend = "Ingresos Propios")
full


write.csv(geo_data, "G:/My Drive/idb/datos/mexico/datos finales/mex_mun_geo_v2.csv")

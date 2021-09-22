library(DescTools)
library(tidyverse)
library(xlsx)

setwd("C:/Users/alexb/Documents/github/Mexico-Municipal-Finance-Data")
efipem <- read.csv("efipem_mun_data_v2.csv", encoding = "utf-8")%>%
  dplyr::select(-X)%>%
  rename(Inversion.publica = `Inversi√≥n.p√∫blica`)

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
  filter(ANIO != "2020")%>%
  ungroup()%>%
  mutate(Ingresos_propios = rowSums(dplyr::select(.,Aprovechamientos,Contribuciones.de.Mejoras,
                                                  Cuotas.y.Aportaciones.de.Seguridad.Social,
                                                  Derechos, Impuestos,Productos), na.rm=T),
         Ingresos_propios_pc = Ingresos_propios/poblacion,
         Inversion_publica_pc = Inversion.publica/poblacion,
         Ingresos_menos_finanzas_pc = Ingresos_menos_finanzas/poblacion,
         Total_de_egresos_pc = Total.de.egresos/poblacion,
         Total_de_ingresos_pc = Total.de.ingresos/poblacion,
         participaciones_no_regalias = Participaciones.federales,
         aportaciones_33_capital = rowSums(dplyr::select(., FA.para.la.Infraestructura.Social.Municipal, FA.para.el.Fortalecimiento.de.los.Municipios)),
         recursos_federales_reasignados = Recursos.federales.y.estatales.reasignados)%>%
  mutate(
    total_transferencias = rowSums(dplyr::select(.,participaciones_no_regalias, aportaciones_33_capital, recursos_federales_reasignados),na.rm=T)
  )
         


names <- mun%>%select(estado, ID_ENTIDAD)%>%unique()%>%group_by(ID_ENTIDAD)%>%count()

missing <- mun%>%filter(is.na(.))
missing <- mun%>%filter(Ingresos_propios == 0)%>%filter(ANIO <2019)

summary(mun$Ingresos_propios)
# y.2018<- mun%>%
#   filter(ANIO == "2018")%>%
#   summarise(sd = sd(educacion_pc), mean = mean(educacion_pc))%>%
#   mutate(cv = sd/mean)
  


#write.csv(mun, "efipem_state_poblacion_v1.csv")
### Standardization
mun <- mun%>%
  filter(!ANIO == 2019)
mun[mun == 0] <- NA
mun[mun < 0] <- NA

missing <- mun%>%filter(is.na(Ingresos_propios))
summary(mun$Ingresos_propios)

# Let's compare the original to the transformed one. 
summary(mun$Ingresos_propios_pc)


# Generate the municipios for table 2.1
mun2.1 <- mun%>%
  group_by(ANIO)%>%
  summarise(
    ingresos_tributarios = (sd(Ingresos_propios_pc, na.rm = T)/mean(Ingresos_propios_pc, na.rm = T)),
    gastos = (sd(Total_de_egresos_pc, na.rm = T)/mean(Total_de_egresos_pc, na.rm = T)),
    # ingresos_tributarios_wins = (sd(wins_Ingresos_propios_pc, na.rm = T)/mean(wins_Ingresos_propios_pc, na.rm = T)),
    # gastos_wins = (sd(wins_Total_de_egresos_pc, na.rm = T)/mean(wins_Total_de_egresos_pc, na.rm = T)),
  )%>%
  pivot_longer(!ANIO,
               names_to = "categoria",
               values_to = "value")%>%
  mutate(pais = rep("Mexico"),
         nivel = rep("Gobiernos Locales"))%>%
  pivot_wider(names_from = ANIO,
              values_from = value)%>%
  relocate(pais, nivel)

spike <- mun%>%
  group_by(ANIO)%>%
  summarise(across(everything(), list(mean, min, max), na.rm=T))

### Combine with municipios table

# table_2.1 <- full_join(municipios2.1, mun2.1)


# Figure 2.2
library(scales)

mun%>%
  group_by(ANIO)%>%
  summarise(#ingresos_tributarios = cv(wins_Ingresos_propios.pc),
    ingresos_tributarios = (sd(Ingresos_propios_pc,na.rm=T)/mean(Ingresos_propios_pc,na.rm=T)),
    gastos = (sd(Total_de_egresos_pc,na.rm=T)/mean(Total_de_egresos_pc,na.rm=T)),
  )%>%
  ggplot(aes(x=ANIO))+
  geom_line(aes(y=ingresos_tributarios, color="Ingresos tributarios"), size=1)+
  geom_line(aes(y=gastos, color = "Gasto"), size=1)+
  theme_classic() +
  labs(title = "Gobiernos locales",
       subtitle = "Mexico",
       x = "", 
       y = "Promedio Disparidad",
       color = "")+
  theme(legend.position = "bottom")+  
  scale_color_manual(values=c("#999999", "royalblue", "#56B4E9"))+  
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))


# # Cuadro 2.2
# municipios2.2 <- municipios%>%
#   group_by(A—O)%>%
#   summarise(gasto_total = (sd(wins_Total_de_egresos_pc, na.rm = T)/mean(wins_Total_de_egresos_pc, na.rm = T)),
#             educacion = (sd(wins_educacion_pc, na.rm = T)/mean(wins_educacion_pc, na.rm = T)),
#             saneamiento = (sd(wins_limpieza_pc, na.rm = T)/mean(wins_limpieza_pc, na.rm = T)),
#             agua = (sd(wins_agua_pc, na.rm = T)/mean(wins_agua_pc, na.rm = T)),
#   )%>%
#   pivot_longer(!A—O,
#                names_to = "categoria",
#                values_to = "value")%>%
#   mutate(pais = rep("Colombia"),
#          nivel = rep("Gobiernos Regionales"))%>%
#   pivot_wider(names_from = A—O,
#               values_from = value)%>%
#   relocate(pais, nivel)


# Cuadro 5.4 pending

mun5.4 <- mun%>%
  # filter(ANIO ==2013)%>%
  mutate(coparticipaciones = rowSums(select(.,participaciones_no_regalias,   aportaciones_33_capital),na.rm = T),
         capital_y_otros = rowSums(select(., recursos_federales_reasignados),na.rm=T)
  )%>%
  group_by(ANIO)%>%
  summarise(coparticipacion = mean(coparticipaciones/total_transferencias, na.rm=T),
            # regalias = mean(regalias/total_transferencias, na.rm=T),
            capital_y_otros = mean(capital_y_otros/total_transferencias, na.rm=T))%>% # This doesn't match up with book. The book says 2013 had 75% coparticipacion transferencias_nacionales
  pivot_longer(!ANIO,
               names_to = "categoria",
               values_to = "value")%>%
  mutate(pais = rep("Mexico"),
         nivel = rep("Gobiernos Locales"))%>%
  pivot_wider(names_from = ANIO,
              values_from = value)%>%
  relocate(pais, nivel)

#####
# Cuadro 5.5
mun5.5 <- mun%>%
  group_by(ANIO)%>%
  summarise(Ingresos_antes_gini = Gini(Ingresos_propios_pc, na.rm = T),
            Ingresos_despues_gini = Gini(Ingresos_menos_finanzas_pc, na.rm = T),
            # Ingresos_antes_gini_wins = Gini(wins_Ingresos_propios_pc, na.rm = T),
            # Ingresos_despues_gini_wins = Gini(wins_Ingresos_menos_finanzas_pc, na.rm = T),
            Ingresos_antes_min_max = max(Ingresos_propios_pc, na.rm = T)/min(Ingresos_propios_pc, na.rm = T),
            Ingresos_despues_min_max = max(Ingresos_menos_finanzas_pc, na.rm = T)/min(Ingresos_menos_finanzas_pc, na.rm = T),
            # Ingresos_antes_min_max_wins = max(wins_Ingresos_propios_pc, na.rm = T)/min(wins_Ingresos_propios_pc, na.rm = T),
            # Ingresos_despues_min_max_wins = max(wins_Ingresos_menos_finanzas_pc, na.rm = T)/min(wins_Ingresos_menos_finanzas_pc, na.rm = T),
            
  )%>%
  pivot_longer(!ANIO,
               names_to = "categoria",
               values_to = "value")%>%
  mutate(pais = rep("Mexico"),
         nivel = rep("Gobiernos Locales"))%>%
  pivot_wider(names_from = ANIO,
              values_from = value)%>%
  relocate(pais, nivel)



# Cuadro 5.6
mun5.6 <- mun%>%
  # filter(ANIO==2013)%>%
  mutate(ingresos_mas_part_pc = rowSums(select(.,Ingresos_propios, participaciones_no_regalias))/poblacion,
         # ingresos_mas_part_reg_pc = rowSums(select(.,Ingresos_propios, participaciones_no_regalias))/poblacion,
         ingresos_mas_part_reg_aport_pc = rowSums(select(.,Ingresos_propios, participaciones_no_regalias,  aportaciones_33_capital,recursos_federales_reasignados),na.rm=T)/poblacion,
         ingresos_mas_part_aport_pc = rowSums(select(.,Ingresos_propios, participaciones_no_regalias, aportaciones_33_capital, recursos_federales_reasignados),na.rm=T)/poblacion)%>%
  group_by(ANIO)%>%
  summarise(ingresos_propios_gini_1 = Gini(Ingresos_propios_pc, na.rm=T),
            ingresos_rev_sharing_gini_2 = Gini(ingresos_mas_part_pc, na.rm=T),
            # ingresos_regalias_gini_4 = Gini(ingresos_mas_part_reg_pc, na.rm=T),
              ingresos_condic_gini_6 = Gini(ingresos_mas_part_reg_aport_pc, na.rm=T),
            ingresos_totales_fin_gini_7 = Gini(Total_de_ingresos_pc, na.rm=T),
            ingresos_totales_sin_fin_gini_7 = Gini(Ingresos_menos_finanzas_pc, na.rm=T))%>%
  pivot_longer(!ANIO,
               names_to = "categoria",
               values_to = "value")%>%
  mutate(pais = rep("Mexico"),
         nivel = rep("Gobiernos Locales"))%>%
  pivot_wider(names_from = ANIO,
              values_from = value)%>%
  relocate(pais, nivel)


grafica_muns_mex <- mun%>%
  mutate(ingresos_mas_part_pc = rowSums(dplyr::select(.,Ingresos_propios, participaciones_no_regalias))/poblacion,
         # ingresos_mas_part_reg_pc = rowSums(select(.,Ingresos_propios, participaciones_no_regalias))/poblacion,
         ingresos_mas_part_reg_aport_pc = rowSums(dplyr::select(.,Ingresos_propios, participaciones_no_regalias,  aportaciones_33_capital,recursos_federales_reasignados),na.rm=T)/poblacion,
         ingresos_mas_part_aport_pc = rowSums(dplyr::select(.,Ingresos_propios, participaciones_no_regalias, aportaciones_33_capital, recursos_federales_reasignados),na.rm=T)/poblacion)%>%
  group_by(ANIO)%>%
  summarise(ingresos_propios_gini_1 = Gini(Ingresos_propios_pc, na.rm=T),
            ingresos_rev_sharing_gini_2 = Gini(ingresos_mas_part_pc, na.rm=T),
            # ingresos_regalias_gini_4 = Gini(ingresos_mas_part_reg_pc, na.rm=T),
            ingresos_condic_gini_6 = Gini(ingresos_mas_part_reg_aport_pc, na.rm=T),
            ingresos_totales_fin_gini_7 = Gini(Total_de_ingresos_pc, na.rm=T),
            ingresos_totales_sin_fin_gini_7 = Gini(Ingresos_menos_finanzas_pc, na.rm=T))%>%
  ggplot(aes(x=ANIO))+
  geom_line(aes(y=ingresos_propios_gini_1, color = "Ingresos Propios"))+
  geom_line(aes(y=ingresos_rev_sharing_gini_2, color = "Transferencias Corrientes"))+
  # geom_line(aes(y=ingresos_regalias_gini_4, color = "Regalias"))+
  geom_line(aes(y=ingresos_condic_gini_6, color = "Transferencias Capital"))+
  geom_line(aes(y=ingresos_totales_fin_gini_7, color = "Total Ingresos"))+
  # geom_line(aes(y=ingresos_totales_sin_fin_gini_7, color = "Total Con Finanzas"))+
  theme_classic() +
  labs(title = "Mexico",
       subtitle = "Gobiernos locales",
       x = "", 
       y = "Coeficiente Gini",
       color = "")+
  theme(legend.position = "bottom")+  
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
  guides(color=guide_legend(nrow=2))


grafica_muns_mex
# 
# ### Standardization
# mun <- mun%>%
#   select( CVE_INEGI, estado, municipio, ANIO, Ingresos_propios_pc, Inversion_publica_pc,
#           Total_de_egresos_pc, Total_de_ingresos_pc, Ingresos_menos_finanzas_pc,
#           limpieza_pc, educacion_pc, hospitales_pc,agua_pc,coparticipacion_pc,derechos_pc,
#           aportaciones_pc, total_transferencias, Aportaciones.federales.y.estatales, Participaciones.federales,Ingresos_menos_finanzas
#   )%>%
#   filter(!ANIO == 2019)
#   
# 
# # Winsorize
# mun <-mun%>%
#   group_by(ANIO)%>%
#   mutate(across(Ingresos_propios_pc:Ingresos_menos_finanzas, Winsorize, .names = "wins_{col}" ))%>%
#   ungroup()
# 
# # Let's compare the original to the transformed one. 
# summary(mun$Ingresos_propios_pc)
# summary(mun$wins_Ingresos_propios_pc)
# 
# 
# # Generate the mun for table 2.1
# mun2.1 <- mun%>%
#   group_by(ANIO)%>%
#   summarise(#ingresos_tributarios = cv(wins_Ingresos_propios.pc),
#     ingresos_tributarios = (sd(wins_Ingresos_propios_pc)/mean(wins_Ingresos_propios_pc)),
#     gastos = (sd(wins_Total_de_egresos_pc)/mean(wins_Total_de_egresos_pc)),)%>%
#   pivot_longer(!ANIO,
#                names_to = "categoria",
#                values_to = "value")%>%
#   mutate(pais = rep("Mexico"),
#          nivel = rep("Gobiernos Locales"))%>%
#   pivot_wider(names_from = ANIO,
#               values_from = value)%>%
#   relocate(pais, nivel)
# 
# 
# 
# 
# 
# # view(cv)
# 
# # Figure 2.2
# cv%>%
#   ggplot(aes(x=ANIO))+
#   geom_line(aes(y=ingresos_tributarios, color="Ingresos Propios"))+
#   geom_line(aes(y=gastos, color = "Gasto Total"))+
#   theme_classic() +
#   labs(title = "Gobiernos locales",
#        subtitle = "Mexico",
#        x = "", 
#        y = "Promedio Disparidad",
#        color = "")+
#   theme(legend.position = "bottom")
# 
# # Cuadro 2.2
# cv <- mun%>%
#   group_by(ANIO)%>%
#   summarise(gastos = (sd(wins_Total_de_egresos_pc, na.rm = T)/mean(wins_Total_de_egresos_pc, na.rm = T)),
#             educacion = (sd(educacion_pc, na.rm = T)/mean(educacion_pc, na.rm = T)),
#             saneamiento = (sd(limpieza_pc, na.rm = T)/mean(limpieza_pc, na.rm = T)),
#             agua = (sd(agua_pc, na.rm = T)/mean(agua_pc, na.rm = T)),
#   )
# 
# yes <-mun%>%
#   group_by(ANIO)%>%
#   summarise(sd = sd(wins_educacion_pc), mean = mean(wins_educacion_pc))%>%
#   mutate(cv = sd/mean)
# 
# # Cuadro 5.4 pending
# 
# # Cuadro 5.4 pending
# 
# mun5.4 <- mun%>%
#   group_by(ANIO)%>%
#   summarise(coparticipacion = mean((wins_Participaciones.federales)/wins_total_transferencias),
#             capital_y_otros = mean((wins_total_transferencias-wins_Participaciones.federales)/wins_total_transferencias))%>% # This doesn't match up with book. The book says 2013 had 75% coparticipacion
#   pivot_longer(!ANIO,
#                names_to = "categoria",
#                values_to = "value")%>%
#   mutate(pais = rep("Mexico"),
#          nivel = rep("Gobiernos locales"))%>%
#   pivot_wider(names_from = ANIO,
#               values_from = value)%>%
#   relocate(pais, nivel)
# 
# #####
# 
# # Cuadro 5.5
# muns5.5 <- mun%>%
#   group_by(ANIO)%>%
#   summarise(Ingresos_antes_gini = Gini(Ingresos_propios_pc),
#             Ingresos_despues_gini = Gini(Ingresos_menos_finanzas_pc))%>%
#   pivot_longer(!ANIO,
#                names_to = "categoria",
#                values_to = "value")%>%
#   mutate(pais = rep("Mexico"),
#          nivel = rep("Gobiernos Locales"))%>%
#   pivot_wider(names_from = ANIO,
#               values_from = value)%>%
#   relocate(pais, nivel)
# 
# # %>%
# #   mutate(max_ingresos_antes = (max(Ingresos_antes_gini)),
# #          min_ingresos_antes = (min(Ingresos_antes_gini))
# #             )
# # %>%
# #   mutate(razon_max_min_antes = wins_max_antes/wins_min_antes)
# 
# 
# # Cuadro 5.6
# muns5.6 <- mun%>%
#   group_by(ANIO)%>%
#   summarise(Ingresos_propios_gini = Gini(wins_Ingresos_propios_pc),
#             coparticipacion_gini = Gini(wins_coparticipacion_pc),
#             aportaciones_gini = Gini(wins_aportaciones_pc),
#             ingresos_totales_gini = Gini(wins_Ingresos_menos_finanzas_pc))%>%
#   pivot_longer(!ANIO,
#                names_to = "categoria",
#                values_to = "value")%>%
#   mutate(pais = rep("Mexico"),
#          nivel = rep("Gobiernos Locales"))%>%
#   pivot_wider(names_from = ANIO,
#               values_from = value)%>%
#   relocate(pais, nivel)
# 

mex_mun <- mun%>%
  arrange(estado,CVE_INEGI, ANIO)%>%
  select(CVE_INEGI, ANIO, estado, municipio, poblacion, Ingresos_menos_finanzas, Ingresos_menos_finanzas_pc, Ingresos_propios, Ingresos_propios_pc, Total.de.egresos, Total_de_egresos_pc , Inversion.publica, Inversion_publica_pc)%>%
  rename(ingresos_tributarios = Ingresos_propios,
         ingresos_tributarios_pc = Ingresos_propios_pc,
         ingresos_totales = Ingresos_menos_finanzas,
         ingresos_totales_pc = Ingresos_menos_finanzas_pc,
         gasto_total = Total.de.egresos,
         gasto_total_pc = Total_de_egresos_pc,
         inversion_publica = Inversion.publica,
         inversion_publica_pc = Inversion_publica_pc,
         year = ANIO
         )


mex_mun_desc <- mex_mun%>%
  select(year, ingresos_tributarios_pc,  ingresos_totales_pc, gasto_total_pc, inversion_publica_pc )%>%
  group_by(year)%>%
  summarise(across(everything(), list(cv=CoefVar),na.rm=T))%>% # min=min, max=max mean=mean,sd=sd,
  mutate(pais = rep("Mexico"),
         nivel = rep("Gobiernos Locales"))%>%
  relocate(pais, nivel)
# 
# write.csv(mex_mun, "G:/My Drive/idb/datos/mexico/datos finales/mex_mun_datos.csv")
# write.csv(mex_mun_desc, "G:/My Drive/idb/datos/mexico/datos finales/mex_mun_cv.csv")

            
## Figure 2.1

mun%>%
  group_by(ANIO)%>%
  summarise(#ingresos_tributarios = cv(wins_Ingresos_propios.pc),
    ingresos_tributarios = (sd(Ingresos_propios_pc,na.rm=T)/mean(Ingresos_propios_pc,na.rm=T)),
    gastos = (sd(Total_de_egresos_pc,na.rm=T)/mean(Total_de_egresos_pc,na.rm=T)),
  )%>%
  ggplot(aes(x=ANIO))+
  geom_line(aes(y=ingresos_tributarios, color="Ingresos tributarios"), size=1)+
  geom_line(aes(y=gastos, color = "Gasto"), size=1)+
  theme_classic() +
  labs(title = "Gobiernos locales",
       subtitle = "Mexico",
       x = "", 
       y = "Promedio Disparidad",
       color = "")+
  theme(legend.position = "bottom")+  
  scale_color_manual(values=c("#999999", "royalblue", "#56B4E9"))+  
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

ggsave("G:/My Drive/idb/datos/mexico/figures/mun_loc_disp.jpeg", width = 5, height = 5, units = "in")

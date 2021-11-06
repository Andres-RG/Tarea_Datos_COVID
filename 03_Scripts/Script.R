library(ggplot2)
library(tidyverse)
library(viridisLite)
library(viridis)
library(deSolve)
source("02_Functions/Functions.R")

#1 Hacer una gráfica apilada de casos positivos a covid por rangos de edades en adultos (18-29,30-39,40-49,50-59,60-70, 70+)

pxe <- select(datos_covid_gto, 
                 c(EDAD, FECHA_INGRESO, CLASIFICACION_FINAL))

pxe_f <- filter(pxe, EDAD >= 18 &
                     CLASIFICACION_FINAL == 1 | EDAD >= 18 &
                     CLASIFICACION_FINAL == 2 | EDAD >= 18 &
                     CLASIFICACION_FINAL == 3 )

re <- rangos_edades(pxe_f$EDAD)

pxe_f <- mutate(pxe_f, rango_edad = re)

grafica_rangos_edad <- ggplot(pxe_f, aes(x=FECHA_INGRESO, y=EDAD, fill = rango_edad)) + 
  geom_bar(position="stack", stat="identity") + 
  ggtitle("Casos positivos a COVID por rangos de edades para el estado de Guanajuato") + 
  labs(x="Tiempo", y="Casos") +
  labs(fill="Rangos de Edad") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(panel.background = element_rect(fill = "white"), 
        axis.line = element_line(colour = "black", size = 1)) +
  scale_fill_viridis(discrete = T) 

jpeg("grafica de rangos de edad.jpeg", width = 750, height = 350)
grafica_rangos_edad
dev.off()








# 2. Hacer una gráfica de casos totales positivos por fecha de inicio de síntomas

sintomas <- select(datos_covid_gto, 
                   c(EDAD, FECHA_SINTOMAS, CLASIFICACION_FINAL))

sintomas <- filter(sintomas, CLASIFICACION_FINAL == 1 |
                     CLASIFICACION_FINAL == 2 |
                     CLASIFICACION_FINAL == 3 )


plot_sintomas <- ggplot(sintomas, aes(x=FECHA_SINTOMAS)) + 
  geom_bar( col = "royalblue3") + 
  ggtitle("Casos positivos a COVID totales para el estado de Guanajuato") + 
  labs(x="Tiempo", y="Casos")  +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(panel.background = element_rect(fill = "white"), 
        axis.line = element_line(colour = "black", size = 1))
plot_sintomas

jpeg("casos positivos totales.jpeg", width = 750, height = 350)
plot_sintomas
dev.off()








# 3. Hacer una gráfica apilada de muertes por covid por rangos de edades en adultos (18-29,30-39,40-49,50-59,60-70, 70+)

base_2 <- select(datos_covid_gto, EDAD, FECHA_INGRESO, FECHA_DEF)
deaths <- base_2 %>% filter(!is.na(FECHA_DEF))
deaths <- filter(deaths, EDAD >= 18)

re_d <- rangos_edades(deaths$EDAD)

deaths_re <- mutate(deaths, rangos_edad=re_d)

grafica_edada_muertes <- ggplot(deaths_re, aes(x=FECHA_DEF, y=EDAD, 
                                               fill=rangos_edad)) + 
  geom_bar(position="stack", stat="identity")  + 
  ggtitle("Muertes de COVID por rangos de edades para el estado de Guanajuato") + 
  labs(x="Tiempo", y="Muertes") +
  labs(fill="Rangos de Edad") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(panel.background = element_rect(fill = "white"), 
        axis.line = element_line(colour = "black", size = 1)) +
  scale_fill_viridis(discrete = T) 
grafica_edada_muertes

jpeg("grafica de muertes por rangos de edad.jpeg", width = 750, height = 350)
grafica_edada_muertes
dev.off()








# 4. Hacer una gráfica de muertes totales positivos por fecha de inicio de síntomas.

muertes_tot <- select(datos_covid_gto, 
                      c(FECHA_SINTOMAS, CLASIFICACION_FINAL, FECHA_DEF))
deaths_totales <- muertes_tot %>% filter(!is.na(FECHA_DEF))
deaths_totales <- filter(deaths_totales, CLASIFICACION_FINAL == 1 |
                           CLASIFICACION_FINAL == 2 |
                           CLASIFICACION_FINAL == 3)

plot_muertes_totales <- ggplot(deaths_totales, aes(x=FECHA_SINTOMAS)) + 
  geom_bar(col = "darkorchid4") + 
  ggtitle("Muertes totales de casos positivos para el estado de Guanajuato") + 
  labs(x="Tiempo", y="Muertes")  +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(panel.background = element_rect(fill = "white"), 
        axis.line = element_line(colour = "black", size = 1))
plot_muertes_totales

jpeg("muertes totales.jpeg", width = 750, height = 350)
plot_muertes_totales
dev.off()








# 5. Hacer una gráfica apilada de hospitalizados por covid por rangos de edades en adultos (18-29,30-39,40-49,50-59,60-70, 70+)

base_3 <- select(datos_covid_gto, 
                 EDAD, FECHA_INGRESO, TIPO_PACIENTE, CLASIFICACION_FINAL)

hospitalizados <- filter(base_3,
                         TIPO_PACIENTE == 2,
                         CLASIFICACION_FINAL == 1 |
                           CLASIFICACION_FINAL == 3, 
                         EDAD >= 18)

re_h <- rangos_edades(hospitalizados$EDAD)

hospitalizados_re <- mutate(hospitalizados, rangos_edad = re_h)

grafica_hospitalizaciones_edad <- ggplot(hospitalizados_re, 
                                         aes(x=FECHA_INGRESO,
                                             y=EDAD,fill=rangos_edad)) +
  geom_bar(position="stack", stat="identity") + 
  ggtitle("Hospitalizaciones por COVID por rangos de edades para el estado de Guanajuato") + 
  labs(x="Tiempo", y="Hospitalizaciones") +
  labs(fill="Rangos de Edad") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(panel.background = element_rect(fill = "white"), 
        axis.line = element_line(colour = "black", size = 1)) +
  scale_fill_viridis(discrete = T) 

grafica_hospitalizaciones_edad

jpeg("grafica de hospitalizaciones por rangos de edad.jpeg", width = 750, height = 350)
grafica_hospitalizaciones_edad
dev.off()








# 6. Hacer una gráfica de hospitalizados totales positivos por fecha de inicio de síntomas.

ht <- select(datos_covid_gto, 
             c(FECHA_SINTOMAS, CLASIFICACION_FINAL, TIPO_PACIENTE))

ht <- filter(ht, 
             TIPO_PACIENTE == 2, 
             CLASIFICACION_FINAL == 1 | 
               CLASIFICACION_FINAL == 2 |
               CLASIFICACION_FINAL == 3)

hospitalizados_totales <- ggplot(ht, aes(x=FECHA_SINTOMAS)) + 
  geom_bar(col = "turquoise3") + 
  ggtitle("Hospitalizaciones totales de casos positivos para el estado de Guanajuato") + 
  labs(x="Tiempo", y="Hospitalizados")  +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(panel.background = element_rect(fill = "white"), 
        axis.line = element_line(colour = "black", size = 1))
hospitalizados_totales

jpeg("hospitalizados totales.jpeg", width = 750, height = 350)
hospitalizados_totales
dev.off()







# 8. ¿Cuál es la probabilidad de que individuos positivos a covid sin comorbilidades sean hospitalizados por rangos de edades en adultos (18-29,30-39,40-49,50-59,60-70, 70+)?

conmor <- select(datos_covid_gto, c(EDAD, CLASIFICACION_FINAL, TIPO_PACIENTE, DIABETES, 
                                  EPOC, ASMA, INMUSUPR, HIPERTENSION, OTRA_COM,
                                  CARDIOVASCULAR, OBESIDAD, RENAL_CRONICA, TABAQUISMO,
                                  OTRO_CASO, FECHA_INGRESO))

wo_conmor <- filter(conmor, DIABETES & EPOC & INMUSUPR & HIPERTENSION
                 & OTRA_COM & CARDIOVASCULAR & OBESIDAD & RENAL_CRONICA & TABAQUISMO
                 & OTRO_CASO == 2 & CLASIFICACION_FINAL == 1 | CLASIFICACION_FINAL == 3 &
                   TIPO_PACIENTE != 2)

wo_t_conmor <- filter(conmor, DIABETES & EPOC & INMUSUPR & HIPERTENSION
                      & OTRA_COM & CARDIOVASCULAR & OBESIDAD & RENAL_CRONICA & TABAQUISMO
                      & OTRO_CASO == 2 & CLASIFICACION_FINAL == 1 | CLASIFICACION_FINAL == 3)

e_18_29 <- filter(wo_conmor, EDAD >= 18 & EDAD <= 29)
e_18_29 <- mutate(e_18_29, rango_edad = "18-29")
e_t_18_29 <- filter(wo_t_conmor, EDAD >= 18 & EDAD <= 29)
p_18_29 <- length(e_18_29$TIPO_PACIENTE)/length(e_t_18_29$TIPO_PACIENTE)*100
e_18_29 <- mutate(e_18_29, p= p_18_29)

e_30_39 <- filter(wo_conmor, EDAD >= 30 & EDAD <= 39)
e_30_39 <- mutate(e_30_39, rango_edad = "30-39")
e_t_30_39 <- filter(wo_t_conmor, EDAD >= 18 & EDAD <= 29)
p_30_39 <- length(e_30_39$TIPO_PACIENTE)/length(e_t_30_39$TIPO_PACIENTE)*100
e_30_39 <- mutate(e_30_39, p= p_30_39)

e_40_49 <- filter(wo_conmor, EDAD >= 40 & EDAD <= 49)
e_40_49 <- mutate(e_40_49, rango_edad = "40-49")
e_t_40_49 <- filter(wo_t_conmor, EDAD >= 18 & EDAD <= 29)
p_40_49 <- length(e_40_49$TIPO_PACIENTE)/length(e_t_40_49$TIPO_PACIENTE)*100
e_40_49 <- mutate(e_40_49, p= p_40_49)

e_50_59 <- filter(wo_conmor, EDAD >= 50 & EDAD <= 59)
e_50_59 <- mutate(e_50_59, rango_edad = "50-59")
e_t_50_59 <- filter(wo_t_conmor, EDAD >= 18 & EDAD <= 29)
p_50_59 <- length(e_50_59$TIPO_PACIENTE)/length(e_t_50_59$TIPO_PACIENTE)*100
e_50_59 <- mutate(e_50_59, p= p_50_59)

e_60_69 <- filter(wo_conmor, EDAD >= 60 & EDAD <= 69)
e_60_69 <- mutate(e_60_69, rango_edad = "60-69")
e_t_60_69 <- filter(wo_t_conmor, EDAD >= 18 & EDAD <= 29)
p_60_69 <- length(e_60_69$TIPO_PACIENTE)/length(e_t_60_69$TIPO_PACIENTE)*100
e_60_69 <- mutate(e_60_69, p= p_60_69)

e_70 <- filter(wo_conmor, EDAD >= 70)
e_70 <- mutate(e_70, rango_edad = "+70")
e_t_70 <- filter(wo_t_conmor, EDAD >= 18 & EDAD <= 29)
p_70 <- length(e_70$TIPO_PACIENTE)/length(e_t_70$TIPO_PACIENTE)*100
e_70 <- mutate(e_70, p= p_70)


re_wo_conmor <- rbind(e_18_29, e_30_39, e_40_49, e_50_59, e_60_69, e_70)

plot_p_hosp <- ggplot(re_wo_conmor, aes(x=rango_edad, y=p,fill=rango_edad)) +
  geom_bar(position="stack", stat="identity") +
  ggtitle("Probabilidad de hospitalización para personas sin conmorbilidades de individuos positivos a COVID por rango de edad para el estado de Guanajuato") + 
  labs(x="Rangos de edad", y="Probabilidad") +
  labs(fill="Rangos de Edad") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(panel.background = element_rect(fill = "white"), 
        axis.line = element_line(colour = "black", size = 1)) +
  scale_fill_viridis(discrete = T) 

plot_p_hosp

jpeg("probabilidad de hospitalizacion.jpeg", width = 950, height = 350)
plot_p_hosp
dev.off()



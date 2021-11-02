library(ggplot2)
library(tidyverse)


#1 Hacer una gráfica apilada de casos positivos a covid por rangos de edades en adultos (18-29,30-39,40-49,50-59,60-70, 70+)

base_1 <- select(datos_covid_gto, 
                 c(EDAD, FECHA_INGRESO, CLASIFICACION_FINAL))

edades_18_29 <- filter(base_1, EDAD >= 18 & EDAD <= 29 & 
                         CLASIFICACION_FINAL == 1 | 
                         CLASIFICACION_FINAL == 2 | 
                         CLASIFICACION_FINAL == 3)
edades_18_29 <- mutate(edades_18_29, rango_edad = "18-29")

edades_30_39 <- filter(base_1, EDAD >= 30 & EDAD <= 39 &
                         CLASIFICACION_FINAL == 1 | 
                         CLASIFICACION_FINAL == 2 | 
                         CLASIFICACION_FINAL == 3)
edades_30_39 <- mutate(edades_30_39, rango_edad = "30-39")

edades_40_49 <- filter(base_1, EDAD >= 40 & EDAD <= 49 &
                         CLASIFICACION_FINAL == 1 | 
                         CLASIFICACION_FINAL == 2 | 
                         CLASIFICACION_FINAL == 3)
edades_40_49 <- mutate(edades_40_49, rango_edad = "40-49")

edades_50_59 <- filter(base_1, EDAD >= 50 & EDAD <= 59 & 
                         CLASIFICACION_FINAL == 1 | 
                         CLASIFICACION_FINAL == 2 | 
                         CLASIFICACION_FINAL == 3)
edades_50_59 <- mutate(edades_50_59, rango_edad = "50-59")

edades_60_69 <- filter(base_1, EDAD >= 60 & EDAD <= 69 & 
                         CLASIFICACION_FINAL == 1 | 
                         CLASIFICACION_FINAL == 2 | 
                         CLASIFICACION_FINAL == 3)
edades_60_69 <- mutate(edades_60_69, rango_edad = "60-69")

edades_70 <- filter(base_1, EDAD >= 70 &  
                      CLASIFICACION_FINAL == 1 | 
                      CLASIFICACION_FINAL == 2 | 
                      CLASIFICACION_FINAL == 3)
edades_70 <- mutate(edades_70, rango_edad = "70")


base_re <- rbind(edades_18_29, edades_30_39, edades_40_49, 
                 edades_50_59, edades_60_69, edades_70)

grafica_rangos_edad <- ggplot(base_re, aes(x=FECHA_INGRESO, y=EDAD, fill = rango_edad)) + 
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


# 3. Hacer una gráfica apilada de muertes por covid por rangos de edades en adultos (18-29,30-39,40-49,50-59,60-70, 70+)

base_2 <- select(datos_covid_gto, EDAD, FECHA_INGRESO, FECHA_DEF)
deaths <- base_2 %>% filter(!is.na(FECHA_DEF))

edades_m_18_29 <- filter(deaths, EDAD >= 18 & EDAD <= 29)
edades_m_18_29 <- mutate(edades_m_18_29, rango_edad = "18-29")

edades_m_30_39 <- filter(deaths, EDAD >= 30 & EDAD <= 39)
edades_m_30_39 <- mutate(edades_m_30_39, rango_edad = "30-39")

edades_m_40_49 <- filter(deaths, EDAD >= 40 & EDAD <= 49)
edades_m_40_49 <- mutate(edades_m_40_49, rango_edad = "40-49")

edades_m_50_59 <- filter(deaths, EDAD >= 50 & EDAD <= 59)
edades_m_50_59 <- mutate(edades_m_50_59, rango_edad = "50-59")

edades_m_60_69 <- filter(deaths, EDAD >= 60 & EDAD <= 69)
edades_m_60_69 <- mutate(edades_m_60_69, rango_edad = "60-69")

edades_m_70 <- filter(deaths, EDAD >= 70)
edades_m_70 <- mutate(edades_m_70, rango_edad = "70")

base_m_re <- rbind(edades_m_18_29, edades_m_30_39, edades_m_40_49,
                   edades_m_50_59, edades_m_60_69, edades_m_70)

grafica_edada_muertes <- ggplot(base_m_re, aes(x=FECHA_DEF, y=EDAD, 
                                               fill=rango_edad)) + 
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

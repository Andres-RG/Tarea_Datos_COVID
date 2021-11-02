library(ggplot2)
library(tidyverse)
source("02_Functions/Functions.R")

#1 Hacer una gráfica apilada de casos positivos a covid por rangos de edades en adultos (18-29,30-39,40-49,50-59,60-70, 70+)

base_1 <- select(datos_covid_gto, 
                 c(EDAD, FECHA_INGRESO, CLASIFICACION_FINAL))
edades <- filter(base_1, EDAD >= 18 & CLASIFICACION_FINAL == 3)
edades <- mutate(edades, rango_edad = EDAD)

for(i in 1:length(edades$EDAD) ) {
  if (edades$EDAD[[i]] >= 18 & edades$EDAD[[i]] <=29){
    edades$rango_edad[[i]] <- c("18-29")
  } else if (edades$EDAD[[i]] >= 30 & edades$EDAD[[i]] <=39){
    edades$rango_edad[[i]] <- c("30-39")
  } else if (edades$EDAD[[i]] >= 40 & edades$EDAD[[i]] <=49){
    edades$rango_edad[[i]] <- c("40-49")
  } else if (edades$EDAD[[i]] >= 50 & edades$EDAD[[i]] <=59){
    edades$rango_edad[[i]] <- c("50-59")
  } else if (edades$EDAD[[i]] >= 60 & edades$EDAD[[i]] <=69){
    edades$rango_edad[[i]] <- c("60-69")
  } else if (edades$EDAD[[i]] >= 70){
    edades$rango_edad[[i]] <- c("+70")
  } 
}

grafica_rangos_edad <- ggplot(edades, aes(x=FECHA_INGRESO, y=EDAD, fill = rango_edad)) + 
  geom_bar(position="stack", stat="identity") + 
  ggtitle("Casos positivos a covid por rangos de edades para el estado de Guanajuato") + 
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
edades_muertes <- c(deaths$EDAD)
mxe <- rangos_edades(edades_muertes)
deaths_re <- mutate(deaths, rango_edad=mxe)

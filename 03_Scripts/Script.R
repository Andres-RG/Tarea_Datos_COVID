library(ggplot2)
library(tidyverse)

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

ggplot(edades, aes(x=FECHA_INGRESO, y=EDAD, fill = rango_edad)) + 
  geom_bar(position="stack", stat="identity") + 
  ggtitle("Casos positivos a covid por rangos de edades para el estado de Guanajuato") + 
  labs(x="Tiempo", y="Casos") + 
  theme(plot.title = element_text(hjust = 0.5))+
  theme(panel.background = element_rect(fill = "white"), 
        axis.line = element_line(colour = "black", size = 1)) +
  scale_fill_viridis(discrete = T)
  



###### Carga de V-Dem

vdem <- readRDS("~/Documents/V-Dem/V-Dem-CY-Full+Others-v14.rds")
head("vdem")


###### Instalación de paquetes de trabajo

install.packages("ggplot2")
library(ggplot2)
install.packages("dplyr")
library(dplyr)
install.packages("tidyr")
library(tidyr)
install.packages("plotly")
library(plotly)
install.packages("lattice")
library(lattice)
install.packages("cowplot")
library(cowplot)
install.packages("reshape2")
library("reshape2")

# Filtrar los datos para los países y años específicos
filteredvdem <- vdem %>%
  filter(country_name %in% c("Chile", "Argentina", "New Zealand", "India"),
         year >= 1960 & year <= 2023)



         head(filteredvdem)         
         
# Seleccionar las variables relevantes
         model_data <-filteredvdem  %>%
           select(v2smprivex, v2smlawpr, v2smpolhate, e_boix_regime, e_democracy_breakdowns)
         
         
         
# Ajustar el modelo de regresión lineal
         modelo <- lm(e_boix_regime ~ v2smprivex + v2smlawpr + v2smpolhate + e_democracy_breakdowns, data = model_data)
         
# Resumen del modelo
         summary(modelo)

# Crear un gráfico
         ggplot(filteredvdem, aes(x = v2smprivex, y = e_boix_regime, color = country_name)) +
           geom_point(size = 3, alpha = 0.7) +  # Puntos del gráfico
           geom_smooth(method = "lm", se = FALSE, color = "black") +  # Línea de tendencia
           scale_color_manual(values = c("Chile" = "#E03C31", 
                                         "Argentina" = "#F6C34A", 
                                         "New Zealand" = "#A6D8D6", 
                                         "India" = "#4C8BF5")) +  # Colores personalizados
           labs(title = "Relación entre la Protección de Privacidad y el Régimen Democrático",
                x = "Protección de la Privacidad por Ley (v2smprivex)",
                y = "Régimen Democrático (e_boix_regime)",
                color = "País") +
           theme_minimal(base_size = 15) +  # Tema minimalista
           theme(legend.position = "bottom",  # Posición de la leyenda
                 plot.title = element_text(hjust = 0.5, size = 18),  # Centrar el título
                 panel.grid.minor = element_blank())  # Quitar las líneas de la cuadrícula menor
         
         
######## SCatter plot
         
         ggplot(filteredvdem, aes(x = v2smprivex, y = e_boix_regime, color = country_name)) +
           geom_point(size = 3, alpha = 0.7) +
           geom_smooth(method = "lm", se = FALSE, color = "black") +
           labs(title = "Relación entre Protección de Privacidad y Régimen Democrático",
                x = "Protección de la Privacidad",
                y = "Régimen Democrático") +
           theme_minimal()
         
######## Calcular medias
         mean_data <- filteredvdem %>%
           group_by(country_name) %>%
           summarize(mean_privacy = mean(v2smprivex, na.rm = TRUE), 
                     mean_regime = mean(e_boix_regime, na.rm = TRUE))
         
         # Convertir a formato largo
         long_data <- melt(mean_data, id.vars = "country_name")
         
         ggplot(long_data, aes(x = country_name, y = value, fill = variable)) +
           geom_bar(stat = "identity", position = "dodge") +
           labs(title = "Comparación de Protección de Privacidad y Régimen Democrático por País",
                x = "País",
                y = "Valor Medio") +
           scale_fill_manual(values = c("mean_privacy" = "#E03C31", 
                                        "mean_regime" = "#4C8BF5"),
                             labels = c("mean_privacy" = "Protección de la Privacidad", 
                                        "mean_regime" = "Régimen Democrático")) +
           theme_minimal()     
         
######### Line Chart
         
         ggplot(filteredvdem, aes(x = year)) +
           geom_line(aes(y = v2smprivex, color = "Protección de la Privacidad"), size = 1) +
           geom_line(aes(y = e_boix_regime, color = "Régimen Democrático"), size = 1) +
           facet_wrap(~country_name) +
           labs(title = "Tendencias en Protección de Privacidad y Régimen Democrático",
                x = "Año",
                y = "Valor") +
           scale_color_manual(values = c("Protección de la Privacidad" = "#E03C31", 
                                         "Régimen Democrático" = "#4C8BF5")) +
           theme_minimal()
         
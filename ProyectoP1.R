###PROYECTO: PRIMER PARCIAL


#Instalar y cargar librerías necesarias
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(skimr)
library(visdat)
library(pacman)
library(datos)
library(GWalkR)
library(janitor)
library(inspectdf)
library(tidyverse)
library(corrplot)
library(car)


#Cargar la base
library(readxl)
datos <- read_excel("proyecto_datos.xlsx")
View(datos)



##Análisis exploratorio de datos

#Valores atípicos y datos faltantes
#Conocer el nombre de las columnas ambas (son iguales)  
names(datos)
#Revisión de datos nulos, vacios
is.na(datos)
sum(is.na(datos))
str(datos)
summary(datos)
datos$TN <- as.numeric(gsub(",", ".", datos$TN))
datos$IF <- as.numeric(gsub(",", ".", datos$IF))
datos$TD <- as.numeric(gsub(",", ".", datos$TD))
datos$RP <- as.numeric(gsub(",", ".", datos$RP))
datos$IG <- as.numeric(gsub(",", ".", datos$IG))

#Estadíticas descrptivas
#Puedo tener una vista de las medidas de tendencia central de cada una de las variables
summary(datos)
#Resumen mas detallado de cada una de las variables
skim(datos)
#Media, mediana, varianza y desviación estándar
estadisticas_adicionales <- datos %>%
  summarise(across(where(is.numeric), 
                   list(media = mean, 
                        mediana = median, 
                        varianza = var, 
                        desviacion_std = sd,
                        minimo = min,
                        maximo = max)))
print(estadisticas_adicionales)
#Percentiles y rango
percentiles_rango <- datos %>%
  summarise(across(where(is.numeric), 
                   list(p25 = ~quantile(., 0.25), 
                        p50 = ~quantile(., 0.5),
                        p75 = ~quantile(., 0.75),
                        rango = ~diff(range(.)))))
print(percentiles_rango)
#Calcular estadísticas descriptivas de las variables numericas
datos_n <- datos[, -1]
summary_mestadisticas <- data.frame(
  Media = sapply(datos_n, mean, na.rm = TRUE),
  Mediana = sapply(datos_n, median, na.rm = TRUE),
  Moda = sapply(datos_n, function(x) names(sort(table(x), decreasing = TRUE))[1]),
  Desviacion_Estandar = sapply(datos_n, sd, na.rm = TRUE),
  Varianza = sapply(datos_n, var, na.rm = TRUE)
)
summary_mestadisticas
#Matriz de correlación
cor_matrix <- cor(datos[, sapply(datos, is.numeric)], use = "complete.obs")
corrplot(cor_matrix, method = "color", tl.cex = 0.8)


#Visualización de datos
#Gráfico de barras: Gasto público en salud por país (Top 10)
top_10_gasto <- datos %>% 
  arrange(desc(GPS)) %>% 
  head(10)
grafico_barras <- ggplot(top_10_gasto, aes(x = reorder(Pais, GPS), y = GPS, fill = Pais)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 Países por Gasto Público en Salud", y = "% Gasto en Salud", x = "País") +
  theme_minimal()
grafico_barras
#Dispersión: Gasto en salud vs Esperanza de vida
grafico_dispersion1 <- ggplot(datos, aes(x = GPS, y = EV)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "Gasto en Salud vs. Esperanza de Vida", x = "% Gasto en Salud", y = "Esperanza de Vida (años)") +
  theme_minimal()
grafico_dispersion1
#Dispersión: IDH vs Esperanza de vida
grafico_dispersion2 <- ggplot(datos, aes(x = IDH, y = EV)) +
  geom_point(alpha = 0.6, color = "darkgreen") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "IDH vs. Esperanza de Vida", x = "Índice de Desarrollo Humano", y = "Esperanza de Vida (años)") +
  theme_minimal()
grafico_dispersion2
#Histograma: Distribución del gasto en salud (% PIB)
hist_gps_pib <- ggplot(datos, aes(x = GPS_PIB)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 20, alpha = 0.7) +
  labs(title = "Distribución del Gasto en Salud (% del PIB)", x = "% PIB en Salud", y = "Frecuencia") +
  theme_minimal()
hist_gps_pib


##Propuesta de Modelo
#Analisis de Componentes Principales
library(pacman)
p_load(readxl, tidyverse, ggplot2, corrplot, rpubs, factoextra)
# Seleccionar solo las columnas numéricas
datos_n <- datos[, -1] # Asumimos que la primera columna no es numérica
# Verificar si hay datos faltantes
sum(is.na(datos_n))
# Remover filas con datos faltantes
datos_n <- na.omit(datos_n)
# Escalar los datos (normalización de las variables numéricas)
datos_scaled <- scale(datos_n)
# Realizar el análisis de componentes principales (ACP)
acp <- prcomp(datos_scaled, center = TRUE, scale. = TRUE)
# Resumen del ACP
summary(acp)
# Varianzas explicadas por cada componente
fviz_eig(acp, addlabels = TRUE, ylim = c(0, 50))
# Cargar las contribuciones de las variables a los componentes principales
acp_var <- get_pca_var(acp)
# Mostrar las contribuciones de las variables a los dos primeros componentes
acp_var$contrib[, 1:2]
# Correlación entre las variables originales y los componentes principales
corrplot(acp_var$cor, method = "circle", type = "upper", tl.col = "black", tl.srt = 45)


#Ajustar modelo de regresión para calcular VIF
modelo1 <- lm(EV ~ ., data = datos_n)
modelo1
summary(modelo1)
#Calcular VIF
vif_values <- vif(modelo1)#############################################################################################3
vif_values
#Modelo 2
modelo2 <- lm(EV ~ RPC + TM + TN + IF + IDH, data = datos_n)
summary(modelo2)
#Modelo elegido
modelo3 <- lm(EV ~ TM + TN + IF + IDH + GPS + GPS_PIB, data = datos_n)
summary(modelo3)

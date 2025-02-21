library(insuranceData)
library(dplyr)
library(skimr)
library(visdat)
library(ggplot2)
#activar el data frame de dataCar
data(dataCar)
#para conocer la estructura del data frame
str(dataCar)
#puedo tener una vista de las medidas de tendencia central de cada una de las variables
summary(dataCar)
#para obtener un resumen más detallado
skim(dataCar)
#para visualizar las primeras filas de un conjuno de datos
head(dataCar)
#para visualizar las ulimas filas de un conjunto de datos
tail(dataCar)
#dimensión del data frame
dim(dataCar)
#conocer la definición de las variables (opción diferente a str)
glimpse(dataCar)
#para conocer el nombre de las variables
names(dataCar)
#para conocer el nombre de las columnas del conjunto de datos
colnames(dataCar)
#verificar si hay valores faltantes (true/false)
any(is.na(dataCar))
#visualizar la estructura y el contenido del conjunto de datos
vis_dat(dataCar)
#visualiuzar los valores faltantes
vis_miss(dataCar)
#porcentaje de pólizas que incurren en al menos una reclamación
sum(dataCar$numclaims!=0)
((4624/67856)*100)
#top 5 de vehiculos con mayor numero de reclamaciones
claims_tipo<-dataCar%>%
  group_by(veh_body)%>%
  summarise(totclaims=sum(numclaims))%>%
  arrange(desc(totclaims))
#muestra solo los primeros 5 
head(claims_tipo)

#library(dplyr)%>%
  
  top5<-dataCar %>%
  group_by(veh_body)%>%
  summarise(numclaims=n())%>%
  arrange(desc(numclaims))%>%
  top_n(5, numclaims)
print(top5)

#grafica de barras para vehiculos y claims
ggplot(claims_tipo, aes(x=reorder(veh_body, -totclaims), y=totclaims))+
  geom_bar(stat= "identity",fill="blue", color="black")+#stat es para que no se junten las barras
  labs(title="Numero de Reclamaciones por tipo de Vehiculo",
       x= "tipo de vehiculo",
       y="Total de reclamaciones")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust=1))

#top 10 de vehiculos con mayor monto de reclamaciones
reclamaciones_tipo<-dataCar %>%
  group_by(veh_body) %>%
  summarise(total_claims=sum(claimcst0)) %>%
  arrange(desc(total_claims))
#muestra solo los primeros 10
head(reclamaciones_tipo)

#Análisis por género
#contar el número de reclamaciones por género
claims_gen<-dataCar %>%
  group_by(gender) %>%
  summarise(numclaims=sum(numclaims)) %>%
  arrange(desc(numclaims))
#mostrar el género con el mayor número de reclamaciones
print(claims_gen)
#calcular el monto total de reclamaciones por género
montot_gen<-dataCar %>%
  group_by(gender,veh_body) %>%
  summarise(total_claims=sum(claimcst0)) %>%
  arrange(desc(total_claims))
#mostrar el género con el mayor monto de reclamaciones
print(montot_gen)
#número total de reclamaciones para cada combinación de género y categoría de edad
claims_gen_age<-dataCar %>%
  group_by(gender, agecat) %>%
  summarise(numclaims=sum(numclaims)) %>%
  arrange(desc(numclaims))
#mostrar el mayor número de reclamaciones para cada combinación de género y categoría de edad
print(claims_gen_age)
#número total de reclamaciones para cada combinación de género y modelo del auto
claims_veh_gen<-dataCar %>%
  group_by(gender, veh_body) %>%
  summarise(totclaims=sum(numclaims)) %>%
  arrange(desc(totclaims))
#mostrar el mayor número de reclamaciones para cada combinación de género y categoría de edad
print(claims_veh_gen)
#crear un gráfico de barras con respecto al género
ggplot(claims_veh_gen, aes(x=reorder(veh_body, -totclaims), y=totclaims, fill=gender)) +
  geom_bar(stat="identity", position = "dodge")+
  labs(title = "Tipos de vehículos reclamados por Género",
       x="Tipo de Vehículo",
       y="Número de Reclamaciones")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


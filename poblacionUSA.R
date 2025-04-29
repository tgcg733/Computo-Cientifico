library(pacman)
p_load(haven, dplyr, factoextra, FactoMineR, readr, rgl, fpc, psych)
require(psych)
library(haven)
library(readxl)
data_USA <- read_excel("PoblacionUSA.xlsm")
View(data_USA)

############################ Año 2000 ##################################
data_2000 <- data_USA[, grepl("2000", names(data_USA))]
View(data_2000)
#Normalizar datos
data2000 <- scale(data_2000[,-1]) #[filas,columnas]
#Datos normalizados
View(data2000)

#Calcular factor de adecuación muestral de kaiser-Meyer
psych::KMO(data2000) #KMO > 0.5
#Diagnóstico para el PCA
pca2000 <- princomp(data2000)

#Diagnóstico
summary(pca2000)

#Revisar varianza y eigenvalores
fviz_eig(pca2000, choice = "variance")
fviz_eig(pca2000, choice = "eigenvalue")

#Gráfico de las puntuaciones factoriales y su representación
fviz_pca_ind(pca2000,
             col.ind = "cos2",
             gradient.cols=c("red", "yellow", "green"),
             repel = FALSE)

#Gráfico de las cargas
fviz_pca_var(pca2000,
             col.var = "contrib",
             gradient.cols=c("red", "yellow","green"),
             repel = FALSE)

#Visualizar puntuaciones
fviz_pca_biplot(pca2000,
                col.var="red",
                col.ind = "black")
pca2000$loadings

x11()
psych::cor.plot(data_2000)

det(cor(data_2000))
pca2000<- psych::principal(data_2000, nfactors=2, residuals= FALSE, rotate="varimax",
                           scores=TRUE, oblique.scores= FALSE, method="regression",
                           use="pairwise", cor="cor", weight= NULL)
pca2000

#Matriz de coeficientes para las puntuaciones de los componentes
pca2000$weights[,1]
pca2000$weights[,2]

#Las variables son las siguientes:
pca2000$scores


############################ Año 2001 ##################################
data_2001 <- data_USA[, grepl("2001", names(data_USA))]
View(data_2001)
#Normalizar datos
data2001 <- scale(data_2001[,-1]) #[filas,columnas]
#Datos normalizados
View(data2001)

#Calcular factor de adecuación muestral de kaiser-Meyer
psych::KMO(data2001) #KMO > 0.5
#Diagnóstico para el PCA
pca2001 <- princomp(data2001)

#Diagnóstico
summary(pca2001)

#Revisar varianza y eigenvalores
fviz_eig(pca2001, choice = "variance")
fviz_eig(pca2001, choice = "eigenvalue")

#Gráfico de las puntuaciones factoriales y su representación
fviz_pca_ind(pca2001,
             col.ind = "cos2",
             gradient.cols=c("red", "yellow", "green"),
             repel = FALSE)

#Gráfico de las cargas
fviz_pca_var(pca2001,
             col.var = "contrib",
             gradient.cols=c("red", "yellow","green"),
             repel = FALSE)

#Visualizar puntuaciones
fviz_pca_biplot(pca2001,
                col.var="red",
                col.ind = "black")
pca2001$loadings

x11()
psych::cor.plot(data_2001)
#Análisis como lo proporciona spss
det(cor(data_2001))
pca2001<- psych::principal(data_2001, nfactors=2, residuals= FALSE, rotate="varimax",
                           scores=TRUE, oblique.scores= FALSE, method="regression",
                           use="pairwise", cor="cor", weight= NULL)
pca2001

#Matriz de coeficientes para las puntuaciones de los componentes
pca2001$weights[,1]
pca2001$weights[,2]

#Las variables son las siguientes:
pca2001$scores

library(pacman)
p_load(haven, dplyr, factoextra, FactoMineR, readr, rgl, fpc, psych)
require(psych)
library(haven)
datapca_attributes <- read.csv2("C:/Users/USER/Downloads/data_pca.csv")
View(datapca_attributes)


#Normalizar datos
data1 <- scale(datapca_attributes[,-16]) #[filas,columnas]
#Datos normalizados
View(data1)

#Diagnóstico para el PCA
#Todas las variables poseen una msa mayor a 0.5, por lo que es pertinente el pca
pca <- princomp(data1)

#Diagnóstico
summary(pca)
#considerar los componentes que tengan proporcion de varianza mayor al 5%.

#Revisar varianza y eigenvalores
fviz_eig(pca, choice = "variance")
fviz_eig(pca, choice = "eigenvalue")
#Hay 6 componentes mayores a la unidad.

#Análisis gráfico
#Gráfico de las puntuaciones factoriales y su representación
fviz_pca_ind(pca,
             col.ind = "cos2",
             gradient.cols=c("red", "yellow", "green"),
             repel = FALSE)

#Gráfico de las cargas
fviz_pca_var(pca,
             col.var = "contrib",
             gradient.cols=c("red", "yellow","green"),
             repel = FALSE)

#Visualizar puntuaciones
fviz_pca_biplot(pca,
                col.var="red",
                col.ind = "black")
pca$loadings


#Realizando el PCA2
pca2<- psych::principal(data1, nfactors=6, residuals= FALSE, rotate="varimax",
                        scores=TRUE, oblique.scores= FALSE, method="regression",
                        use="pairwise", cor="cor", weight= NULL)
pca2


#Matriz de coeficientes para las puntuaciones de los componentes
pca2$weights[,1]
pca2$weights[,2]
pca2$weights[,3]
pca2$weights[,4]
pca2$weights[,5]
pca2$weights[,6]

#Por lo anterior, un conjunto de 15 variables altamente relacionadas
#se redujo a unicamente 6 variables cuya característica es que son
#ortogonales.

#Las variables son las siguientes:
pca2$scores

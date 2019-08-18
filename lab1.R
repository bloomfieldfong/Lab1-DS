
########################################
## Universidad del Valle de Guatemala ##
## Laboratorio 1: Data Science        ##
## Autores:                           ##
##    Mayra Silva                     ##
##    Michelle Bloomfield             ##
########################################
require(ggpubr)
library(ggplot2)
library(caret)
library("ggpubr")
library(corrplot)
library(lattice)
library(magrittr)
library(dplyr)
library(psych)
library(rela)
library(FactoMineR)
library(factoextra)
library("devtools")
<<<<<<< HEAD
library(arules)
library(cluster) 
library(NbClust)
library(fpc) 
=======

library(cluster) 
library(fpc) 
library(NbClust)

install.packages("factoextra")
>>>>>>> master
library(factoextra) 



#C:/Users/smayr/Documents/Tercer año/Semestre 6/Data Science/Laboratorio 1/Lab1-DS
setwd("C:/Users/smayr/Documents/Tercer año/Semestre 6/Data Science/Laboratorio 1/Lab1-DS")

##se jalan los datos de el csv 
data <- read.csv("train_data.csv")

##Resumen de los datos
summary(data)



##Graficos

datosd <- data.frame(LotFrontage = data$LotFrontage,  yearbuilt = data$YearBuilt, yearremod = data$YearRemodAdd, bsmtfins1 = data$BsmtFinSF1, bsmtfin2 = data$BsmtFinSF2, BsmtUnSF = data$BsmtUnfSF, totalBmsntSF = data$TotalBsmtSF, firststFloor = data$X1stFlrSF, secondndFloor = data$X2ndFlrSF, MoSold = data$MoSold, pool = data$PoolArea, woodDeck = data$WoodDeckSF, openPorch = data$OpenPorchSF, encloseporch = data$EnclosedPorch, treessnporch = data$X3SsnPorch, sscreenporch = data$ScreenPorch, garageArea = data$GarageArea, garaYearBuilt = data$GarageYrBlt, fireplaces = data$Fireplaces, totalroomsabvgrd = data$TotRmsAbvGrd, kitchenabvgr = data$KitchenAbvGr, bedabvgr = data$BedroomAbvGr, halfbath = data$HalfBath, fullbath = data$FullBath, bsmtFullBath = data$BsmtFullBath, bsmtHalfBath = data$BsmtHalfBath, grlivArea = data$GrLivArea, masvrarea = data$MasVnrArea)


boxplot(data$LotArea, xlab = "Area")







##Correlacion entre el año de remodelacion y las condiciones que se encuentra la casa

correlacion <- cor(data$OverallCond,data$YearRemodAdd, "pearson", use = "complete.obs")
correlacion

##Gafica de los años de remodelacion y condiciones que se encuentra la casa

plot(data$YearRemodAdd, data$OverallCond)

## Dividiré las variables categóricas de las numericas
dataN <- data[,c(4,5,18,19,20,21,27,35,37,38,39,44,45,46,47,48,49,50,51,52,53,55,57,60,62,63,67,68,69,70,71,72,76,77,78,81)]
dataC <- data[,-c(4,5,18,19,20,21,27,35,37,38,39,44,45,46,47,48,49,50,51,52,53,55,57,60,62,63,67,68,69,70,71,72,76,77,78,81)]
dataN <- na.omit(dataN)


##graficasa

boxplot(data$LotArea, xlab = "Area del lote")


qqnorm(data$LotArea)
qqline(data$LotArea)



#Matriz de correlación


matr <- cor(dataN)
 

m <- corrplot(matr, method="number", type="upper")
m <- na.omit(m)

#Test de esferecidad

cortest.bartlett(dataN)
#R was not square, finding R from data
#chisq
#[1] 56046

#$p.value
#[1] 0

#$df
#[1] 630

## PCA
m <- dataN[,c(3, 5, 8, 11,12, 15, 22, 25, 26, 36)]
KMO(m)
bartlett.test(dataN)


<<<<<<< HEAD

=======
>>>>>>> master
#Definiendo componentes pricipales
compPrin <- prcomp(m, scale=T)
summary(m)
#Graficando los eigenvalores

res <- fviz_eig(compPrin,  geom="bar", width=0.8, addlabels=T)

res

<<<<<<< HEAD

##correlacion de variables

=======
##correlacion de variables
>>>>>>> master

cor.test(data$OverallCond,data$YearRemodAdd,method = "pearson")
ggscatter(data, x = "OverallCond", y = "YearRemodAdd", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "ESTADO", ylab = "FALTA")

##Gafica de los años de remodelacion y condiciones que se encuentra la casa

plot(data$YearRemodAdd, data$OverallCond)


##Normalidad de datos

qqnorm(data$YearBuilt)
qqline(data$YearBuilt)


qqnorm(data$SalePrice)
qqline(data$SalePrice)


#--------------------------------------------------------- Clustering ------------------------------------------------------#



library(factoextra) 
library(cluster) #Para calcular la silueta
library(e1071)#para cmeans
library(mclust) #mixtures of gaussians
library(fpc) #para hacer el plotcluster
library(NbClust) #Para determinar el número de clusters óptimo
library(factoextra) #Para hacer gráficos bonitos de clustering



datosCluster <- data.frame(LotFrontage = data$LotFrontage, LotArea = data$LotArea, yearbuilt = data$YearBuilt, yearremod = data$YearRemodAdd, bsmtfins1 = data$BsmtFinSF1, bsmtfin2 = data$BsmtFinSF2, BsmtUnSF = data$BsmtUnfSF, totalBmsntSF = data$TotalBsmtSF, firststFloor = data$X1stFlrSF, secondndFloor = data$X2ndFlrSF, MoSold = data$MoSold, miscVal = data$MiscVal, pool = data$PoolArea, woodDeck = data$WoodDeckSF, openPorch = data$OpenPorchSF, encloseporch = data$EnclosedPorch, treessnporch = data$X3SsnPorch, sscreenporch = data$ScreenPorch, garageArea = data$GarageArea, garaYearBuilt = data$GarageYrBlt, fireplaces = data$Fireplaces, totalroomsabvgrd = data$TotRmsAbvGrd, kitchenabvgr = data$KitchenAbvGr, bedabvgr = data$BedroomAbvGr, halfbath = data$HalfBath, fullbath = data$FullBath, bsmtFullBath = data$BsmtFullBath, bsmtHalfBath = data$BsmtHalfBath, grlivArea = data$GrLivArea, masvrarea = data$MasVnrArea)

datosCluster <- na.omit(datosCluster)

##Silueta para clustering, fuzzy cmean, kmedias y gusiana

#0.43
silkm<-silhouette(km$cluster,dist(datosCluster[,1:30]))
mean(silkm[,3]) 

#0.5
silch<-silhouette(groups,dist(datosCluster[,1:30]))
mean(silch[,3]) 

silfcm<-silhouette(fcm$cluster,dist(datosCluster[,1:30]))
mean(silfcm[,3]) 

silmg<-silhouette(mc$classification,dist(datosCluster[,1:30]))
mean(silmg[,3]) 



x <- (nrow(datosCluster[,1:30])-1)
y <- sum(apply(datosCluster[,1:30],2,var))
wss <- (nrow(datosCluster[,1:30])-1)*sum(apply(datosCluster[,1:30],2,var))
for (i in 2:10) 
  wss[i] <- sum(kmeans(datosCluster[1:30], centers=i)$withinss)

plot(1:10, wss, type="b", xlab="Numero de clusters",  ylab="Within grupo de sumas de cuadrados")


km<-kmeans(datosCluster[,1:30],4)
datosCluster$grupo<-km$cluster


##Grupo 1

g1<- datosCluster[datosCluster$grupo==1,]
nrow(g1)
summary(g1)

##Grupo 2

g2<- datosCluster[datosCluster$grupo==2,]
nrow(g2)
summary(g2)


##Grupo 3

g3<- datosCluster[datosCluster$grupo==3,]
nrow(g3)
summary(g3)

##Grupo 4

g4<- datosCluster[datosCluster$grupo==4,]
nrow(g4)
summary(g4)

boxplot(g4$firststFloor, xlab = "Piso 1 en pies cuadracos")


##Grafica de clusters 

plotcluster(datosCluster[,1:30],km$cluster)






### Análisis exploratorio continuación
data1 <- dataN[,c(3)]
boxplot(data1, main="Dispersión de la calidad en general")


##tabla de frecuencias de la condición de venta de la casa
table(dataC$SaleCondition)

##histograma del estilo de casa
hist(dataN$FullBath)











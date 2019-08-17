
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


#C:/Users/smayr/Documents/Tercer año/Semestre 6/Data Science/Laboratorio 1/Lab1-DS
setwd("C:/Users/smayr/Documents/Tercer año/Semestre 6/Data Science/Laboratorio 1/Lab1-DS")

##se jalan los datos de el csv 
data <- read.csv("train_data.csv")

##Resumen de los datos
summary(data)


##Correlacion entre el año de remodelacion y las condiciones que se encuentra la casa

correlacion <- cor(data$OverallCond,data$YearRemodAdd, "pearson", use = "complete.obs")
correlacion

##Gafica de los años de remodelacion y condiciones que se encuentra la casa

plot(data$YearRemodAdd, data$OverallCond)

## Dividiré las variables categóricas de las numericas
dataN <- data[,c(4,5,18,19,20,21,27,35,37,38,39,44,45,46,47,48,49,50,51,52,53,55,57,60,62,63,67,68,69,70,71,72,76,77,78,81)]
dataC <- data[,-c(4,5,18,19,20,21,27,35,37,38,39,44,45,46,47,48,49,50,51,52,53,55,57,60,62,63,67,68,69,70,71,72,76,77,78,81)]
dataN <- na.omit(dataN)


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


#Definiendo componentes pricipales
compPrin <- prcomp(m, scale=T)
summary(m)
#Graficando los eigenvalores

res <- fviz_eig(compPrin,  geom="bar", width=0.8, addlabels=T)

res








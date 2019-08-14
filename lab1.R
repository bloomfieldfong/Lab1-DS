########################################
## Universidad del Valle de Guatemala ##
## Laboratorio 1: Data Science        ##
## Autores:                           ##
##    Mayra Silva                     ##
##    Michelle Bloomfield             ##
########################################

library(ggplot2)
library(caret)

setwd("C:/Users/User/Desktop/Data Science/Lab1-DS")

##se jalan los datos de el csv 
data <- read.csv("train_data.csv")

##Resumen de los datos
summary(data)


##Correlacion entre el area del lote y la distancia en la que esta conectada a la calle 

correlacion <- cor(data$LotArea,data$LotFrontage, "pearson", use = "complete.obs")
correlacion
library("ggpubr")
install.packages("ggpubr")
plot(data$LotArea,data$YearBuilt)


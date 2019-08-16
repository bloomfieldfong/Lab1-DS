
########################################
## Universidad del Valle de Guatemala ##
## Laboratorio 1: Data Science        ##
## Autores:                           ##
##    Mayra Silva                     ##
##    Michelle Bloomfield             ##
########################################

library(ggplot2)
library(caret)
library("ggpubr")
install.packages("ggpubr")
install.packages("corrplot")
library(corrplot)


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
dataN <- data[,c(1,4,5,18,19,20,21,27,35,37,38,43,44,45,46,47,48,49,50,51,52,54,56,59,61,62,66,67,68,69,70,71,75,76,77,80)+1]
dataC <- data[,-c(1,4,5,18,19,20,21,27,35,37,38,43,44,45,46,47,48,49,50,51,52,54,56,59,61,62,66,67,68,69,70,71,75,76,77,80)+1]


#Matriz de correlación

matrix <- round(cor(dataN)) 

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

setwd("C:/Users/User/Desktop/Data Science/Lab1-DS")

##se jalan los datos de el csv 
data <- read.csv("train_data.csv")

##Resumen de los datos
summary(data)


##Correlacion entre el año de remodelacion y las condiciones que se encuentra la casa

correlacion <- cor(data$OverallCond,data$YearRemodAdd, "pearson", use = "complete.obs")
correlacion

##Gafica de los años de remodelacion y condiciones que se encuentra la casa

plot(data$YearRemodAdd, data$OverallCond)


#Import du fichier sur les associés
library(readxl)
library(xlsx)
library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(readr)
library(lubridate) # gestion des dates
############################## Pipe 2017 #############################################
#setwd("D:/Data/R_Sources/PR Augmentée/data")

#On import le fichier 
data_Associe <- read_excel("Sheet_1_Data.xls", col_names = FALSE)

NomColonnes <- c("ID",	"ID_collecteur",	"début",	"fin",	"AdresseIP",
                 "e-mail",	"Prénom",	"Nom",	"Custom",
                 "AvisBrice","AvisOlivierG","AvisMarc","AvisOlivierR","AvisJulien",
                 "Q1Brice","Q1OlivierG","Q1Marc","Q1OlivierR","Q1Julien",
                 "Q2Brice","Q2OlivierG","Q2Marc","Q2OlivierR","Q2Julien",
                 "Q3Brice","Q3OlivierG","Q3Marc","Q3OlivierR","Q3Julien",
                 "Q4Brice","Q4OlivierG","Q4Marc","Q4OlivierR","Q4Julien")

colnames(data_Associe) <-NomColonnes

#Simplification des réponses
data_Associe$AvisBrice <- ifelse(is.na(data_Associe$AvisBrice), TRUE, FALSE)
data_Associe$AvisOlivierG <- ifelse(is.na(data_Associe$AvisOlivierG), TRUE, FALSE)  
data_Associe$AvisMarc <- ifelse(is.na(data_Associe$AvisMarc), TRUE, FALSE)  
data_Associe$AvisOlivierR <- ifelse(is.na(data_Associe$AvisOlivierR), TRUE, FALSE) 
data_Associe$AvisJulien <- ifelse(is.na(data_Associe$AvisJulien), TRUE, FALSE) 

hum <- function (colonne) {
  if (str_detect(colonne, "Content")) "Content"
  else if (str_detect(colonne, "Mécontent")) "Mécontent" 
  else if(str_detect(colonne, "Neutre"))    "Neutre" 

}

humeur <- function (data_Associe,Colnom){
data_Associe[[Colnom]][str_detect(data_Associe[[Colnom]], "Mécontent")] <- "Mécontent"
data_Associe[[Colnom]][str_detect(data_Associe[[Colnom]], "Content")] <- "Content"
data_Associe[[Colnom]][str_detect(data_Associe[[Colnom]], "Neutre")] <- "Neutre"
data_Associe[[Colnom]]
}
#on remplace sur l'ensemble des colonnes de 14 à 34 avec l'humeur pour simplifier par la suite
for (n in colnames(data_Associe)[15:34]) {
  data_Associe[n] <-  humeur(data_Associe,n)
}

#On sauvegarde le dataFrame pour la suite de l'analyse
save(data_Associe,file="data_Associe.RData")
  
  
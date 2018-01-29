# Fichier Global
#libraires utilisées
library(shiny)
library(shinythemes)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(RColorBrewer) #coulors pour ggplot
library(D3partitionR) # version  0.5.0 à charger avec les commandes ci dessous
      # library(devtools)
      # install_github("AntoineGuillot2/D3partitionR")
library(visNetwork) #package pour visualiser des reseaux
library(fmsb)
library(treemap)
couleurs <- c("#8361E8","#74F260","#B74E57","#F2E863","#61E8E1","#9382B7",  "#F25757","#9CA89A")
names(couleurs) <- c("Transformation", "ETM","Data", "Digital Innovation","Sécurité", "PocLab","x","y")

# library(randomForest)
# colonnes <- c("ENTITE__APPORTEUR","LEAD__RESP","ASSOCIE",
#               "SELL_ON",
#               "OFFRE_PRINCIPALE", "SECTEUR", "GROUPE","STEP")
# pp <- pilotage_data %>% filter(WEEK==max(pipe$WEEK))
# dataForet <- pp[,colonnes]
# dataForet <- dataForet %>% replace_na(list(ASSOCIE = "unknown", GROUPE="unknown",
#                                            ENTITE__APPORTEUR="unknown",LEAD__RESP="unknown",
#                                            SELL_ON ="Non",OFFRE_PRINCIPALE="unknown",SECTEUR="unknown"
#                                            ))
# dataForet$STEP <- factor(dataForet$STEP)
# dataForet$ENTITE__APPORTEUR <- factor(dataForet$ENTITE__APPORTEUR)
# dataForet$LEAD__RESP <- factor(dataForet$LEAD__RESP)
# dataForet$ASSOCIE <- factor(dataForet$ASSOCIE)
# dataForet$SELL_ON <- factor(dataForet$SELL_ON)
# dataForet$OFFRE_PRINCIPALE <- factor(dataForet$OFFRE_PRINCIPALE)
# dataForet$SECTEUR <- factor(dataForet$SECTEUR)
# dataForet$GROUPE <- factor(dataForet$GROUPE)
# 
# pipe.rf <- randomForest(dataForet[,1:7], dataForet$STEP)
# print(pipe.rf)


#données 2017
load("data/pilotage_data.RData")



#on ne garde que le pipe
#0 - A qualifier, 1 - Qualifiée, 2 - A émettre, 3 - Emise
pipe <- pilotage_data %>% filter (STEP %in% c("0 - A qualifier", "1 - Qualifiée", "2 - A émettre", "3 - Emise"))
Assot <-c("JPR","MMO","ORE","JSO","UHE","JPP","OGR")

              
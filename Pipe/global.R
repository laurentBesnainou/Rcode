library(shiny)
library(shinythemes)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(RColorBrewer) #coulors pour ggplot
library(stringr)
library(lubridate) # gestion des dates
library(xts) #serie temporelle
library(purrr) # 
library(tidyr)
library(scales) 
library(googleVis) # Permet d'utiliser les graphiques >  Google Chart 
library(highcharter) # courbe de comparaison des CA de 2015 à 2017
library(treemap)
library(viridis)
library(sunburstR)
library(data.table)

#chargement des données
load("data/pilotage2016_data.RData")
load("data/pilotage2015_data.RData")
load("data/staffing2017.RData")

#données 2017
load("data/pilotage_data.RData")

  library(shiny)
  library(shinydashboard)
  library(flexdashboard)
  library(dplyr)
  library(ggplot2)
  library(ggthemes)
  library(RColorBrewer) #coulors pour ggplot
  library(stringr)
  library(lubridate) # gestion des dates
  library(xts) #serie temporelle
library(dygraphs) #affichage de graphe avec affichage dynamique
#library(forcats)
library(DT) # Affichage des tableaux dynamiques
library(purrr) # 
library(tidyr)
library(scales) 
library(googleVis) # Permet d'utiliser les graphiques >  Google Chart 
library(timevis)# Gantt
library(gridExtra) #Pour avoir les 2 graphes dans le meme plot
library(circlize)
library(D3partitionR) # permet de générer le treemap dynamique
library(highcharter) # courbe de comparaison des CA de 2015 à 2017
library(daff) #PErmet de voir les différences entre deux data frame
# library(readxl)
# library(xlsx)
# Chargement des données Pipe de 2015 à 2017

#Variables globales pour le dashboard BT de la partie Server.R
# chargement des données
#données 2016 et 2015
load("data/pilotage2016_data.RData")
load("data/pilotage2015_data.RData")

#données 2017
load("data/pilotage_data.RData")
pilotage_2017 <- pilotage_data
load("data/staffing2017.RData")
load("data/StaffingS10.RData")

load("data/Effectif2017.RData")
load("data/Effectifs2016.RData")
load("data/Effectif2015.RData")


############################# on va recuperer au niveau des Data la semaine 
#Liste des personnes dans le staffing
people <- Staffing %>% arrange(CONSULTANTS) %>% distinct(CONSULTANTS) 

## 
Type_Staffing <- c(1,2,3,4,5,6,7)
totem <- Staffing %>% filter(TYPE == 1) %>% arrange(ID_TOTEM) %>% distinct(ID_TOTEM)

## 
maxWeek <- as.numeric( format(today(), "%U"))
nbMois <- month(today())


my_cols <- c("JANV","FEV","MAR","AVR","MAI","JUIN","JUIL","AOUT","SEPT","OCT","NOV","DEC")
#on va calculer le taux de staffing par grade pour les jours staffés
staffFerme <- Staffing %>% filter (TYPE == 1) %>%
  select (GRADE, JANV,FEV,MAR,AVR,MAI,JUIN,JUIL,AOUT,SEPT,OCT,NOV,DEC) %>%
  group_by(GRADE) %>% summarise_each(funs(sum(., na.rm = TRUE)))


#on va calculer le provisiore
staffProvisiore <- Staffing %>% filter (TYPE == 2) %>%
  select (GRADE, JANV,FEV,MAR,AVR,MAI,JUIN,JUIL,AOUT,SEPT,OCT,NOV,DEC) %>%
  group_by(GRADE) %>% summarise_each(funs(sum(., na.rm = TRUE)))


#on va calculer le nombre de jours de congéÂ¨spar grade pour les jours staffés
staffconges <- Staffing %>% filter (TYPE == 3) %>%
  select (GRADE, JANV,FEV,MAR,AVR,MAI,JUIN,JUIL,AOUT,SEPT,OCT,NOV,DEC) %>%
  group_by(GRADE) %>% summarise_each(funs(sum(., na.rm = TRUE)))

#palette de couleurs
group.colors <- c("Data" = "#60D394", "Digital Innovation" = "#AAF683","ETM" ="#FFD97D", "Transformation" = "#FF9B85", "PocLab" = "#ED7D3A")
group.secteurs <- c("Banque" = "#D8DBE2","Assurance" = "#5bc0eb","Energie" = "#fde74c","Industrie & services" = "#9bc53d","Para-public" = "#e55934"," Retail" = "#fa7921")

#generation d'un dataframe avec une valeur par semaine
week_seq15 <- data_frame("week_seq" = seq(dmy("01/01/2015"), dmy("31/12/2015"), by = "week"))
week_seq16 <- data_frame("week_seq" = seq(dmy("01/01/2016"), dmy("31/12/2016"), by = "week"))
week_seq17 <- data_frame("week_seq" = seq(dmy("01/01/2017"), dmy("31/12/2017"), by = "week"))


#on coompte le nombre de personnes présente par semaine sur les 3 années
effectifs <- Effectifs2017
effectif_2016 <- Effectifs2016 %>% 
  select(NOM, DATE_ENTREE, DATESORTIE, ETAT, GRADE_2016) %>% 
  crossing(week_seq16) %>% #Crossing permet de constituer pour chaque ligne d'effectif autant de ligne que de semaines 
  mutate(DATE_ENTREE = ymd(DATE_ENTREE),
         DATESORTIE = ymd(DATESORTIE)) %>% 
  replace_na(list(DATESORTIE = dmy("01/01/2018"))) %>% 
  mutate(present_window = interval(DATE_ENTREE, DATESORTIE)) %>% #on regarde si la personne est presente sur l'intervalle Entree Sortie sur l'année
  mutate(is_present = week_seq %within% present_window) %>% 
  mutate(poids_effectif = ifelse(GRADE_2016 == "8-Associé", 0.5, 
                                 ifelse(GRADE_2016 == "7-Directeur", 0.75, 1))) %>% 
  filter(is_present == TRUE) %>% 
  mutate(WEEK = week(week_seq)) %>% 
  group_by(WEEK) %>% 
  summarise(POIDS_EFFECTIF_TOT = sum(poids_effectif)) %>% 
  mutate(YEAR = 2016)


effectif_2015 <- Effectifs2015 %>% 
  select(NOM, DATE_ENTREE, DATESORTIE, ETAT, GRADE_2016) %>% 
  crossing(week_seq15) %>% 
  mutate(DATE_ENTREE = ymd(DATE_ENTREE),
         DATESORTIE = ymd(DATESORTIE)) %>% 
  replace_na(list(DATESORTIE = dmy("01/01/2018"))) %>% 
  mutate(present_window = interval(DATE_ENTREE, DATESORTIE)) %>% 
  mutate(is_present = week_seq %within% present_window) %>% 
  mutate(poids_effectif = ifelse(GRADE_2016 == "8-Associé", 0.5, 
                                 ifelse(GRADE_2016 == "7-Directeur", 0.75, 1))) %>% 
  filter(is_present == TRUE) %>% 
  mutate(WEEK = week(week_seq)) %>% 
  group_by(WEEK) %>% 
  summarise(POIDS_EFFECTIF_TOT = sum(poids_effectif)) %>% 
  mutate(YEAR = 2015)


effectif_2017 <- Effectifs2017 %>% 
  select(NOM, DATE_ENTREE, DATESORTIE, ETAT, GRADE_2016) %>% 
  crossing(week_seq17) %>% 
  mutate(DATE_ENTREE = ymd(DATE_ENTREE),
         DATESORTIE = ymd(DATESORTIE)) %>% 
  replace_na(list(DATESORTIE = dmy("01/01/2018"))) %>% 
  mutate(present_window = interval(DATE_ENTREE, DATESORTIE)) %>% 
  mutate(is_present = week_seq %within% present_window) %>% 
  mutate(poids_effectif = ifelse(GRADE_2016 == "8-Associé", 0.5, 
                                 ifelse(GRADE_2016 == "7-Directeur", 0.75, 1))) %>% 
  filter(is_present == TRUE) %>% 
  mutate(WEEK = week(week_seq)) %>% 
  group_by(WEEK) %>% 
  summarise(POIDS_EFFECTIF_TOT = sum(poids_effectif)) %>% 
  mutate(YEAR = 2017)

################################ Fonctions pour le chargement d'une semaine en plus

format_col_names <- function(x) { 
  x <- str_replace_all(x, "[éèê]", "e")
  x <- str_replace_all(x, " ", "_")
  x <- str_replace_all(x, "\\.", "")
  x <- str_replace_all(x, "/", "")
  x <- str_replace_all(x, "€", "E")
  x <- str_replace_all(x, "\\(", "_")
  x <- str_replace_all(x, "\\)", "")
  x <- str_replace_all(x, "\n", "")
  x <- str_replace_all(x, "\r", "")
  x <- str_replace_all(x, "\\+1", "_PLUS_1_")
  x <- str_replace_all(x, "\\-1", "_MOINS_1_")
  x <- str_to_upper(x)
  x
}

trim <- function (x) gsub("^\\s+|\\s+$", "", x)


#on regarde le nombre de jours ouvrés par mois
nbjours2016 <- c(20,21,22,21,20,22,20,22,22,21,20,22)

nbjours2017 <- c(22,20,23,20,21,22,20,22,21,22,20,20)
#Cette fonction permet de basculer sur l'année désirée
ChoixAnnee <- function(Annee){
  if (Annee==2015) {
    #effectifs <<- Effectifs2015
    pilotage_data <<- pilotage_2015 
    Objectif_BT <<- 11120
    Objectif_CA <<- 13000}
  if (Annee==2016) {
    #effectifs <<- Effectifs2016
    pilotage_data <<- pilotage_2016
    Objectif_BT <<-12500
    Objectif_CA <<- 14500
    nbjours <<- nbjours2016}
  if (Annee==2017) {
    #effectifs <<- Effectifs2017
    pilotage_data <<- pilotage_2017
    Objectif_BT <<-13000
    Objectif_CA <<- 14500
    nbjours <<- nbjours2017}
}

library(shiny)
library(shinydashboard)
library(googlesheets) #permet d'acceder à la feuille Google du Staffing
library(purrr) #pour typer les colonnes
library(lubridate)
library(dplyr)
library(stringr) 
library(tidyr)#fonction replace_NA
library(daff) #PErmet de voir les différences entre deux data frame
library(ggthemes) #pour les thèmes ggplot
library(ggplot2)
library(readxl)
library(DT)
library(xlsx)

###### Fonctions qui permet de renommer les colonnes importées
format_col_names <- function(x) { 
  x <- str_replace_all(x, "[éèê]", "e")
  x <- str_replace_all(x, "û", "u")
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

#fonction qui remplace dans les colonnes le xx -> yy par yy 
clean_split <- function(x) {
  split_result <- str_split(x, "->")
  split_result
}

clean_split_pourcentage <- function(x) {
  
  split_result <- str_split(x, "->")
  split_result <- paste(as.character(as.numeric(split_result[[1]]) *100) , "%")
  if (is.na(split_result[2]) ) {
    split_result <- split_result[1]
  } else {
    split_result <- paste(split_result[1],"->",split_result[2])
  }
  split_result
}

df <- data.frame(per=c("20%",".2->.5"))

  clean_split_CA <- function(x) {
    split_result <- str_split(x, "->")
    split_result <- as.character(round(as.numeric(split_result[[1]],digit=0)))
    split_result <- paste(split_result[1],"->",split_result[2])
    split_result
  }
  
  remplace_Type <- function (x) {
    #on va remplacer les listes par la valeur

    # 1   Ferme
    # 2		Prévi
    # 3		WBT - CONGES
    # 4		WBT - Formation
    # 5		WBT - Activités Internes ou à affecter
    # 6		WBT - Arrêt maladie
    # 7		WBT - Inactivité
    typeList <- c(1:7)
    nomList <- c("Ferme","Prévi","WBT - CONGES","WBT - Formation","WBT - Activités Internes","WBT - Arrêt maladie","WBT - Inactivité")
    dframe <- data.frame(typeList,nomList)
    dframe[,2][x]
    
  }

  
  labels <- structure(list(V1 = 1:6, V2 = c("LABEL1", "LABEL2", "LABEL3", 
                                            "LABEL4", "LABEL5", "LABEL6")), 
                      .Names = c("V1", "V2"), class = "data.frame", row.names = 
                        c("1",  "2", "3", "4", "5", "6"))
                        
  df <- data.frame(colX= sample(1:6,25, replace=TRUE), colY=rnorm(25))
  labels[,2][df$colX]
last_elem <- function(x) {
  str_to_title(str_trim(x[length(x)]))
}
fisrt_elem <- function(x) {
  str_to_title(str_trim(x[1]))
}

#map_chr(clean_split(c("AA->BB", "TT")), last_elem)

#Fonction qui va remplacer les , par des . ainsi que les % (cela se produit sur les data Google ramenées)
remplaceVal <- function (x) {
  x <- str_replace_all(x,",",".")
  x <- str_replace_all(x,"%",".")
  x
}
#Liste des type de staffing et Pipe pour l'UI
Type_Staffing <- c(1,2,3,4,5,6,7)
STEPS <- list("0 - A qualifier"  , "1 - Qualifiée" , "2 - A émettre" ,"3 - Emise", "4 - Gagnée", "5 - No follow","6 - En sommeil",   "7 - Perdue"   )

#Chargement des derniers Dataframe valides
load("data/staffing2017.RData")
load("data/Pilotage.RData")
#Chargement des logs
load("data/LogFichier.RData")
load("data/LogGoogle.RData")


mois <- as.numeric(format(Sys.Date(), "%m"))
moisSuiv <- mois +1
listMois <- c("JAN","FEV","MAR","AVR","MAI","JUIN","JUIL","AOUT","SEPT","OCT","NOV","DEC")
selection <- listMois [mois:moisSuiv]

My_colonnes <- c("CONSULTANTS","TYPE","MISSIONS","ID_TOTEM"  )
My_colonnes <- c(My_colonnes, selection)
##################################### Connexion à Google et récupération du Staffing et du Pipe en ligne #################################################


# # Procedure qui permet de stocker son Token pour s'authentifier sur Drive
# gs_ls()
# # #On récupère le token
#  token <- gs_auth(cache = FALSE)
#  gd_token()
# # #On le sauve au niveau du drive
#  saveRDS(token, file = "googlesheets_token.rds")

## on utilise le token (de façon transparente) pour se connecter
suppressMessages(gs_auth(token = "googlesheets_token.rds", verbose = FALSE))


# On récupère le lien vers le fichier Pilotage dans R qui contient le staffing
BT_Pilotage <- gs_title("BT - Pilotage 2017")
#on charge ensuite le fichier Pipe depuis Google Sheets
Pilotage_Actuel <- gs_read(ss=BT_Pilotage, ws = "Pipe", skip=4)
# On charge la feuille google dans un dataframe (skip permet de passer les 15 premieres lignes)
Staff_Actuel <- gs_read(ss=BT_Pilotage, ws = "Staffing", skip=15)

# Fonction qui supprime les colonnes du staffing en trop et qui transforme les colonnes de char à num 
prepareStaffing1 <- function (df) {
  #on va suprimer touts les colonnes X en fin de dataframe
  df <- df [, -c(30:43)]

  #On ajoute la semaine et l'année 
  df <- df %>% mutate(WEEK=week(now())) %>% mutate(YEAR=year(now()))
  colnames(df) <- format_col_names(colnames(df))
  df$JANV <- as.numeric(remplaceVal(df$JANV))
  df$FEV <- as.numeric(remplaceVal(df$FEV))
  df$MAR <- as.numeric(remplaceVal(df$MAR))
  df$AVR <- as.numeric(remplaceVal(df$AVR))
  df$MAI <- as.numeric(remplaceVal(df$MAI))
  df$JUIN <- as.numeric(remplaceVal(df$JUIN))
  df$JUIL <- as.numeric(remplaceVal(df$JUIL))
  df$AOUT <- as.numeric(remplaceVal(df$AOUT))
  df$SEPT <- as.numeric(remplaceVal(df$SEPT))
  df$OCT <- as.numeric(remplaceVal(df$OCT))
  df$NOV <- as.numeric(remplaceVal(df$NOV))
  df$DEC <- as.numeric(remplaceVal(df$DEC))
  #df$TOTAL <- as.numeric(remplaceVal(df$TOTAL))
  

 df <- df %>% select(one_of(My_colonnes))
  df
}
prepareStaffing2 <- function (df) {
  df2 <- df %>% 
    filter(TYPE != 0,CONSULTANTS != "(ex)")  %>%
    select(one_of(My_colonnes))
  df2
} 


# fonction qui compare le Staffing de référence avec la version google en ligne
compareStaff <- function(df){
  df <- df %>% filter(! V2 %in%  c("ZZ_NE RIEN AJOUTER SOUS CETTE LIGNE",NA)) %>%
    mutate(V1= as.character(V1))%>% 
    replace_na(list(V6=0))%>%
    mutate(V8= ifelse(grepl("->",V6),as.character(V6),paste(V6,"->0"))) %>%
    mutate(V9= ifelse(grepl("->",V7),as.character(V7),paste(V7,"->0"))) %>%
    
    filter(V1 %in% c("+++","---","->")) %>% 
    mutate(V1= ifelse(V1=="->","mod",V1)) %>% 
    separate(V8, into = c("MPrev","M"), sep = "->",convert=TRUE) %>%
    replace_na(list(M=0,MPrev=0)) %>%
    separate(V9, into = c("M1Prev","M1"), sep = "->",convert=TRUE) %>%
    mutate(M=ifelse(V1=="+++",as.numeric(MPrev),as.numeric(M))) %>%
    mutate(MPrev= ifelse(V1=="+++",0,as.numeric(MPrev)))%>%
    mutate(M1=ifelse(V1=="+++",as.numeric(M1Prev),as.numeric(M1))) %>%
    mutate(M1Prev= ifelse(V1=="+++",0,as.numeric(M1Prev)))%>%
    
    mutate(DELTA=ifelse(V1=="+++",M,
                         ifelse(V1=="---",-MPrev,M - MPrev))) %>% 
  mutate(DELTA1=ifelse(V1=="+++",M1,
                      ifelse(V1=="---",-M1Prev,M1 - M1Prev))) 
  #%>%
    #mutate(V4= map_chr(clean_split(as.character(V4)), last_elem) ) %>%
  #   mutate(V5= map_chr(clean_split(as.character(V5)), last_elem) ) %>% filter (V1 %in% c("+++","---") | grepl("->",V6))
 colnames(df)  <-  c('Changement', My_colonnes, "MPrev","M","M1Pev","M1","DELTA","DELTA1")
  df
}

preparePipe1 <- function (df) {
  colnames(df) <- format_col_names(colnames(df))
  #On va supprimer les colonnes non utiles
  df <- df[, -c(24)]
  df <- df[, -1]
  #ON SUPPRIME LA LIGNE 1
  df <- df[-1, ]
  #On va renommer les colonnes COMPTE,ASSOCIE, SUJET,STEP,PROB,CODE_TOTEM,TOTAL_CA_VENDU_N__KE)
  df <- df %>% select(1,5,2,6,9,7,10)
  colnames(df) <- c("COMPTE","ASSOCIE", "SUJET","STEP","CODE_TOTEM","PROB","CA_BT__N__KE")
  df <- df %>% arrange(COMPTE,SUJET)
  df$PROB <- as.numeric(remplaceVal(df$PROB))/100
  df
}

comparePipe <- function (df) {
  comparaisonP <- df %>% filter(! V4 %in%  c("ZZ_NE RIEN AJOUTER SOUS CETTE LIGNE",NA)) %>%
    mutate(V1= as.character(V1))%>% 
    filter(V1 %in% c("+++","---","->")) %>%
    mutate(V1= ifelse(V1=="->","mod",V1)) %>% 
    filter (V1 == "+++"|V1 == "---"|(V1=="mod" & grepl("->",paste(V5,V6,V8) ))) #%>%
  
  colnames(comparaisonP)  <-   c('Changement',"COMPTE","ASSOCIE", "SUJET","STATUT","TOTEM","PROBA","CA_BT__N__KE")
  #comparaisonP$PROBA <- clean_split_pourcentage(comparaisonP$PROBA)
  comparaisonP <-  comparaisonP %>%  mutate(STEP_Prev= map_chr(clean_split(as.character(STATUT)), last_elem) ) %>%  
    mutate(STEP= map_chr(clean_split(as.character(STATUT)), fisrt_elem) ) %>%
    mutate(PROB_Prev= map_chr(clean_split(as.character(PROBA)), last_elem) ) %>%  
    mutate(PROB= map_chr(clean_split(as.character(PROBA)), fisrt_elem) ) %>%
    mutate(CA= map_chr(clean_split(as.character(CA_BT__N__KE)), last_elem) ) %>%  
    mutate(CA_Prev= map_chr(clean_split(as.character(CA_BT__N__KE)), fisrt_elem) ) %>%
    mutate(DELTA=ifelse(Changement=="mod",as.numeric(format(as.numeric(CA) - as.numeric(CA_Prev), digits=2, nsmall=2)),
                        ifelse(Changement=="+++",as.numeric(CA),-as.numeric(CA)))) %>%
    mutate(DELTA = as.numeric(DELTA)) %>%
  
    replace_na(list(DELTA=0))
  
  comparaisonP
}


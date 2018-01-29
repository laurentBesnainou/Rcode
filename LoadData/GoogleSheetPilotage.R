# load package
library(googlesheets)
library(stringr)
library(dplyr)
library(utils)
library(purrr)

# # Procedure qui permet de stocker son Token pour s'authentifier sur Drive
# gs_ls()
# #On récupère le token
# token <- gs_auth(cache = FALSE)
# gd_token()
# #On le sauve au niveau du drive
# saveRDS(token, file = "googlesheets_token.rds")

#gs_auth(token = "googlesheets_token.rds")
## and you're back in business, using the same old token
## on utilise le token (de façon transparente) 
suppressMessages(gs_auth(token = "googlesheets_token.rds", verbose = FALSE))

#Permet de connaitre le User authentifié
gs_user()

# On récupère le lien vers le fichier Pilotage dans R
BT_Pilotage <- gs_title("BT - Pilotage 2017")

TEST <- gs_title("PIPE2017")
tt <- gs_read(ss=TEST, ws = "Feuille 1")
# liste des worksheets de ce fichier
#gs_ws_ls(BT_Pilotage)

# On charge la feuille google dans un dataframe (skip permet de passer les 15 premieres lignes)
Staff_Actuel <- gs_read(ss=BT_Pilotage, ws = "Staffing", skip=15)

#
map_int(Staff_Actuel,~sum(is.na(.x)))
map_dbl(Staff_Actuel,~sum(is.na(.x)/nrow(Staff_Actuel)))
#map avec conditions
map_if(Staff_Actuel,~is.character(.x),~sum(is.na(.x)/nrow(Staff_Actuel)))


#On va supprimer toutes les lignes de fin du fichier de staffing qui ne sont as utiles (ex et lignes vide en colonne 1)
Staff_Actuel2 <- subset(Staff_Actuel, !is.na(Staff_Actuel$Consultants))
Staff_Actuel3 <- subset(Staff_Actuel2, Staff_Actuel2$Consultants!="(ex)")

#On modifie le nom des colonne
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

colnames(Staff_Actuel3) <- format_col_names(colnames(Staff_Actuel3))
Staffing <- Staff_Actuel3
save(Staffing,file="staffing2017.RData")

#O va a resent lire la feuille Pipe
Pipe_Actuel <- gs_read(ss=BT_Pilotage, ws = "Pipe", skip=3)

#on supprime la premiere colonne anomalies                       
Pipe_Actuel$Anomalies <- NULL
#on supprie la 1er ligne
Pipe_Actuel<- Pipe_Actuel[-1,]

# On va supprimer toutes les lignes de fin du fichier de pipe qui ne sont as utiles (NA + "ZZ_NE RIEN AJOUTER SOUS CETTE LIGNE")
Pipe_Actuel2 <- subset(Pipe_Actuel, !is.na(Pipe_Actuel$Compte))
Pipe_Actuel3 <- subset(Pipe_Actuel2, Pipe_Actuel2$Compte!="ZZ_NE RIEN AJOUTER SOUS CETTE LIGNE")
Pipe_Actuel3 <- subset(Pipe_Actuel3, Pipe_Actuel3$Compte!="Sell On")

colnames(Pipe_Actuel3) <- format_col_names(colnames(Pipe_Actuel3))
Pipe_Actuel3$CA_BT__N__K€ <- as.numeric(Pipe_Actuel3$CA_BT__N__K€)

# On ajoute le numéro de semaine au Dataframe
nb <- week(today())
date_ref <- data_frame(
  date_ref = seq(from = dmy("01/01/2017"), to = dmy("31/12/2017"), by = "weeks"),
  week = week(date_ref)
)
Pipe_Actuel3 <- Pipe_Actuel3 %>%  mutate(week=nb) %>% mutate(date_ref = today())
colnames(Pipe_Actuel3) <- format_col_names(colnames(Pipe_Actuel3))
#on transforme les % en numeric
Pipe_Actuel3 <- Pipe_Actuel3 %>% mutate (PROB = ifelse(is.na(PROB),0,as.numeric(sub("%","",PROB))/100 )) %>%
  mutate (DATE_DE_DEBUT_PREVUE_MISSION = ifelse(is.na(DATE_DE_DEBUT_PREVUE_MISSION),NA,
                                                paste(DATE_DE_DEBUT_PREVUE_MISSION,"/2017", sep="")))





#on supprime les info si la semaine est deja présente
pilotage_data2 <- subset(pilotage_data, pilotage_data$WEEK != nb)
#on ajoute le pipe récupéré de la google sheet
pilotage_data3 <- bind_rows(pilotage_data2,Pipe_Actuel3)

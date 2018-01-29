#lecture des fichiers de pipe depuis le debut de l'année

library(readxl)
library(xlsx)
library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(readr)
library(lubridate) # gestion des dates

############################## Pipe 2017 #############################################
setwd("D:/tmp/importDashbardData")
# on parcours le repertoire et on constitu un CSV avec toutes les donn?es
filenames <- list.files(path = "input", pattern = "data_pilotage_S")
lstFile <- list.files(path = "input", pattern = "data_pilotage_S", full.names = TRUE)
AjouteSemaine <- function(fichier){
        pilotage_data_tmp <- read_excel(fichier, col_types = c(
                                                rep("text", 6),
                                                "numeric",
                                                rep("text", 2),
                                                rep("numeric", 6),
                                                "text",
                                                "date",
                                                rep("text", 3),
                                                rep("numeric", 2),
                                                rep("date", 5)))
        nb <- str_sub(fichier,22)
        fin <- str_locate(nb,".xlsx")[1]
        nb <-  as.numeric(str_sub(nb,1,fin-1))
        #on ajoute la semaine et aussi la date de début de la semaine
        date_ref <- data_frame(
          date_ref = seq(from = dmy("01/01/2017"), to = dmy("31/12/2017"), by = "weeks"),
          week = week(date_ref)
        )
        pilotage_data_tmp %>% mutate(week=nb) -> pilotage_data_tmp
        pilotage_data_tmp %>% 
        inner_join(date_ref, by = "week")
}

# constitution d'un data frame vide
pilotage_data <- data.frame()

# on charge tous les fichier Excel
for (i in lstFile) {
  pilotage_data_temp <- AjouteSemaine(i)
  pilotage_data <- bind_rows(pilotage_data,pilotage_data_temp)
  print(i)
}

format_col_names <- function(x) { 
  euro <- "\u20AC"
  x <- str_replace_all(x, "[éèê]", "e")
  x <- str_replace_all(x, " ", "_")
  x <- str_replace_all(x, "\\.", "")
  x <- str_replace_all(x, "/", "")
  x <- str_replace_all(x, euro, "E")
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

colnames(pilotage_data) <- format_col_names(colnames(pilotage_data))

pilotage_data$`ANO-MALIES__12` <- NULL
pilotage_data$DATE_DERNIERE_MODIF <- NULL
# on retire les blancs inutiles sur les groupes qui peuvent causer des erreurs de group_by
pilotage_data$GROUPE <- trim(pilotage_data$GROUPE)
pilotage_data$COMPTE[pilotage_data$COMPTE =="La poste"] = "La Poste"
save(pilotage_data,file="pilotage_data.RData")





pilotage2016_data <-pilotage_data
save(pilotage2016_data,file="pilotage2016_data.RData")
write_csv(pilotage_data, "input/pilotage_clean_All.csv")

###############################
# On importe les données 2015 PIPE #
###############################
######################"" 2015 ###########################
formData <- c("date", "text", "text", 
              "text", "text", "text", "numeric", 
              "text", "text", "numeric", "numeric", "numeric", 
              "numeric", "numeric", "numeric", 
              "numeric", "numeric", "date", "date", 
              "text", "text", "text", "text", "text", 
              "date", "date", "date", 
              "date", "date")

setwd("D:/Data/R sources/data_pilotage/input")
# on parcours le repertoire et on constitu un CSV avec toutes les donn?es
filenames <- list.files(path = "2015", pattern = "data_pilotage_S")
lstFile <- list.files(path = "2015", pattern = "data_pilotage_S", full.names = TRUE)
Ajoute2015 <- function(fichier){
  #on recupere le numero de la semaine
  po <- regexpr('data_pilotage_S', fichier)
  fin <- str_locate(fichier,".xlsx")[1]
  semaine <- strtoi(str_sub(fichier,po+15,fin-1))
  #Lecture du fichier
  tmp <- read_excel(fichier, 
             col_types =formData)
  
  #on ajoute la semaine et aussi la date de début de la semaine
  date_ref <- data_frame(
    date_ref = seq(from = dmy("05/01/2015"), to = dmy("31/12/2015"), by = "weeks"),
    week = week(date_ref)
  )
  #on ajoute la colonne semaine ainsi que la date correspondante sur 2015
  tmp %>% mutate(week=semaine) %>% 
    inner_join(date_ref, by = "week")
}
  
# constitution d'un data frame vide
pilotage2015_data <- data.frame()

# on charge tous les fichier Excel
for (i in lstFile) {
  pilotage_data_temp <- Ajoute2015(i)
  pilotage2015_data <- bind_rows(pilotage2015_data,pilotage_data_temp)
  print(i)
}

colnames(pilotage2015_data) <- format_col_names(colnames(pilotage2015_data))

#on va modifier des valeurs qui ne sont plus les même entre 2015 et 2016
pilotage2015_data$STEP[pilotage2015_data$STEP =="2 - Prop. à émettre"] = "2 - A émettre"
pilotage2015_data$STEP[pilotage2015_data$STEP =="3 - Prop. émise"] = "3 - Emise"

# on retire les blancs inutiles sur les groupes qui peuvent causer des erreurs de group_by
pilotage2015_data$GROUPE <- trim(pilotage2015_data$GROUPE)
pilotage2015_data$GROUPE[pilotage2015_data$GROUPE =="La poste"] = "La Poste"

# 
save(pilotage2015_data,file="pilotage2015_data1.RData")


############################## Staffing 2016 #############################################

Staffing <- read_excel("input/staffing.xlsx")
colnames(Staffing) <- format_col_names(colnames(Staffing))
save(Staffing,file="staffing2017.RData")

Staffing <- read_excel("input/Staffing2017_PREV.xlsx")
colnames(Staffing) <- format_col_names(colnames(Staffing))
save(Staffing,file="staffing2017PREV.RData")


write_csv(Staffing, "input/staffing.csv")
StaffingS10 <- read_excel("input/StaffingS10.xlsx")
colnames(StaffingS10) <- format_col_names(colnames(StaffingS10))
save(StaffingS10,file="StaffingS10.RData")




############################## Staffing 2016 #############################################
setwd("D:/tmp/importDashbardData")
Staffing <- read_excel("input/StaffingS38.xlsx")
colnames(Staffing) <- format_col_names(colnames(Staffing))
save(Effectifs2017,file="Effectif2017.RData")
save(Effectifs2015,file="Effectif2015.RData")

save(Staffing,file="staffing2017.RData")
write_csv(Staffing, "input/staffing.csv")

############################## TOTEM 2013-2015 #############################################
setwd("D:/Data/R sources/data_pilotage")
TOTEM <- read_excel("input/TOTEM2013-2015.xlsx")
colnames(TOTEM) <- format_col_names(colnames(TOTEM))
save(TOTEM,file="TOTEM.RData")
pilotage2016_data <- pilo
save(pilotage2016_data,file="pilotage2016_data.RData")

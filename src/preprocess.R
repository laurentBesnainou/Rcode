library(readxl)
library(tidyr)
library(dplyr)
library(stringr)
######################################### Formulaire Associé 
#PR2017 - PERF2016 - M_Datalab.xls
data_associe <- read_excel("PR2017 - PERF2016 - A_Datalab.xls", col_names = FALSE)

nbr_cols <- ncol(data_associe)
nbr_rows <- nrow(data_associe)

remove_html <- function(str_vec) {
  str_trim(str_replace_all(data_associe[[i]], "<.*?>", ""))
}

remove_html_str <- function(str_vec) {
  str_trim(str_replace_all(str_vec, "<.*?>", ""))
}

respondant_ids <- data_associe[-1,][[6]] %>% as.numeric()
# On recupere le nom des associés de la premiere question
for (i in 10:14) {
  if (is.character(data_associe[[i]])) {
    data_associe[[i]] <- remove_html(data_associe[[i]])
  }
}
# on supprime les colonnes vides
null_cols <- vector()
for (i in seq_len(nbr_cols)) {
  if (sum(is.na(data_associe[[i]]))/(nbr_rows - 1) == 1) {
    null_cols <- c(null_cols, i)
  }
}
#n supprime les lignes vides
data_associe[null_cols] <- NULL

#Les 2 premieres lignes contiennent le nom des colonnes a reconmposer
col_fields <- data_associe[c(1, 2),]
#les données commencent à partir de la ligne 3
true_data <- data_associe[-c(1, 2),]

#les questions sont en ligne 1
question_rows <- data_associe[1, ]
#les personnes associées aux questions sont en ligne 2
name_rows <- data_associe[2, ]

#Liste des questions posées
q1_str <- "Pour quelle personne ci-dessous considères-tu que ton avis n'est pas pertinent ?"
q2_str <- "Dans quelle mesure considères-tu que les associés suivants incarnent la valeur de leadership ?"
q3_str <- "Dans quelle mesure considères-tu que les associés suivants sont porteurs de vision ?"
q4_str <- "Dans quelle mesure considères-tu que les associés suivants incarnent un savoir-faire et savoir-être permettant l'origination commerciale ?"
q5_str <- "Dans quelle mesure considères-tu que les associés suivants incarnent un savoir-faire et savoir-être permettant le développement des collaborateurs ?"

colnames(data_associe) <- 1:ncol(data_associe)
#Position des questions dans le fichier Excel
q1_cols <- 8:11
q2_cols <- 12:16
q3_cols <- 17:21
q4_cols <- 22:26
q5_cols <- 27:31
q2_5 <- 12:31

#Fonction qui permet de récupèrer le nom de l'associé sur la Q1
for (i in q1_cols) {
  data_associe[1, i] <- q1_str
  data_associe[2, i] <- str_trim(str_split(data_associe[2, i], " - ")[[1]][1])
}

#fonction qui enleve le HTML sur les questions 2 à 5
for (i in q2_5) {
  data_associe[1, i] <- remove_html_str(data_associe[1, i])
  data_associe[2, i] <- remove_html_str(data_associe[2, i])
}

#Rappel de la questions sur la ligne 1
for (i in q2_cols) {
  data_associe[1, i] <- q2_str
}

for (i in q3_cols) {
  data_associe[1, i] <- q3_str
}

for (i in q4_cols) {
  data_associe[1, i] <- q4_str
}

for (i in q5_cols) {
  data_associe[1, i] <- q5_str
}

#On remplace sur les colonnes 2 à 5 le HTML par Content Mécotent Neutre
for (i in q2_5) {
  data_associe[[i]][-c(1,2)] <- ifelse(str_detect(data_associe[[i]][-c(1,2)], "Content"), "Content", 
                                       ifelse(str_detect(data_associe[[i]][-c(1,2)], "Mécontent"), 
                                              "Mécontent", "Neutre"))
}

#On fusionne sur la ligne 1 la question avec le nom de l'associé
for (i in q1_cols) {
  data_associe[1, i] <- str_c(data_associe[1, i], data_associe[2, i], sep = " - ")
}

#on fusionne également sur les autres questions
for (i in q2_5) {
  data_associe[1, i] <- str_c(data_associe[1, i], data_associe[2, i], sep = " - ")
}



#on supprime la ligne 2
data_associe <- data_associe[-2, ]

#on initialise le nom des colonnes
colnames(data_associe) <- data_associe[1, ]

#on supprime la ligne 1
data_associe <- data_associe[-1, ]

data_associe[[6]] <- data_associe[[6]] %>% as.numeric()

#on supprime les 5 premieres colonnes et la derniere
data_associe[32] <- NULL
irrelevant_cols <- 1:5
data_associe[irrelevant_cols] <- NULL

# PARTIE de constitution des données sous la forme 
data_tidy <- data_associe %>% gather(key=QA, value=response, -c(`Adresse e-mail`))

#on contitue 2 colonnes la colonne QA
data_tidy <- data_tidy %>% separate(QA, into=c("Question", "Consultant"), sep = " - ")
#on renomme la 1er colonne
colnames(data_tidy)[1] <- "ID_consultant"

#on sauvegarde
questionnaire_associe <- data_tidy
save(questionnaire_associe, file="questionnaire_associe.RData")

################## Forulaire DM
################## Forulaire DM
################## Forulaire DM
################## Forulaire DM
#PR2017 - PERF2016 - M_Datalab.xls
data_manager <- read_excel("PR2017 - PERF2016 - M_Datalab.xls", col_names = FALSE)

nbr_cols <- ncol(data_manager)
nbr_rows <- nrow(data_manager)

remove_html <- function(str_vec) {
  str_trim(str_replace_all(data_manager[[i]], "<.*?>", ""))
}

remove_html_str <- function(str_vec) {
  str_trim(str_replace_all(str_vec, "<.*?>", ""))
}

respondant_ids <- data_manager[-1,][[6]] %>% as.numeric()
# On recupere le nom des managers de la premiere question
for (i in 10:30) {
  if (is.character(data_manager[[i]])) {
    data_manager[[i]] <- remove_html(data_manager[[i]])
  }
}
# on supprime les colonnes vides
null_cols <- vector()
for (i in seq_len(nbr_cols)) {
  if (sum(is.na(data_manager[[i]]))/(nbr_rows - 1) == 1) {
    null_cols <- c(null_cols, i)
  }
}
#n supprime les lignes vides
data_manager[null_cols] <- NULL

#Les 2 premieres lignes contiennent le nom des colonnes a reconmposer
col_fields <- data_manager[c(1, 2),]
#les données commencent à partir de la ligne 3
true_data <- data_manager[-c(1, 2),]

#les questions sont en ligne 1
question_rows <- data_manager[1, ]
#les personnes associées aux questions sont en ligne 2
name_rows <- data_manager[2, ]

#Liste des questions posées Managers
q1_str <- "Pour quelle personne ci-dessous considères-tu que ton avis n'est pas pertinent ?"
q2_str <- "Dans quelle mesure considères-tu que les personnes suivantes incarnent la proposition de valeur BT ?"
q3_str <- "Dans quelle mesure considères-tu que les personnes suivants encouragent et créent les conditions nécessaires à l'incarnation de la proposition de valeur BT ?"
q4_str <- "Dans quelle mesure considères-tu que les personnes suivantes sont des bons managers opérationnels (direction de mission ou de dispositif interne) ?"
q5_str <- "Dans quelle mesure considères-tu que les personnes suivantes sont des bons coachs ?"

#on supprime les 5 premieres colonnes
irrelevant_cols <- 1:5
data_manager[irrelevant_cols] <- NULL

colnames(data_manager) <- 1:ncol(data_manager)

#Position des questions dans le fichier Excel +21
q1_cols <- 2:22
q2_cols <- 23:43
q3_cols <- 44:64
q4_cols <- 65:85
q5_cols <- 86:106
q2_5 <- 23:106

#Fonction qui permet de récupèrer le nom du manager sur la Q1
for (i in q1_cols) {
  data_manager[1, i] <- q1_str
  data_manager[2, i] <- str_trim(str_split(data_manager[2, i], " - ")[[1]][1])
}

#fonction qui enleve le HTML sur les questions 2 à 5
for (i in q2_5) {
  data_manager[1, i] <- remove_html_str(data_manager[1, i])
  data_manager[2, i] <- remove_html_str(data_manager[2, i])
}

#Rappel de la questions sur la ligne 1
for (i in q2_cols) {
  data_manager[1, i] <- q2_str
}

for (i in q3_cols) {
  data_manager[1, i] <- q3_str
}

for (i in q4_cols) {
  data_manager[1, i] <- q4_str
}

for (i in q5_cols) {
  data_manager[1, i] <- q5_str
}

#On remplace sur les colonnes 2 à 5 le HTML par Content Mécotent Neutre
for (i in q2_5) {
  data_manager[[i]][-c(1,2)] <- ifelse(str_detect(data_manager[[i]][-c(1,2)], "Content"), "Content", 
                                       ifelse(str_detect(data_manager[[i]][-c(1,2)], "Mécontent"), 
                                              "Mécontent", "Neutre"))
}

#On fusionne sur la ligne 1 la question avec le nom de l'associé
for (i in q1_cols) {
  data_manager[1, i] <- str_c(data_manager[1, i], data_manager[2, i], sep = " - ")
}

#on fusionne également sur les autres questions
for (i in q2_5) {
  data_manager[1, i] <- str_c(data_manager[1, i], data_manager[2, i], sep = " - ")
}



#on supprime la ligne 2
data_manager <- data_manager[-2, ]

#on initialise le nom des colonnes
colnames(data_manager) <- data_manager[1, ]

#on supprime la ligne 1
data_manager <- data_manager[-1, ]

#data_manager[[6]] <- data_manager[[6]] %>% as.numeric()



# PARTIE de constitution des données sous la forme 
data_tidy <- data_manager %>% gather(key=QA, value=response, -`Adresse e-mail`)

#on contitue 2 colonnes la colonne QA
data_tidy <- data_tidy %>% separate(QA, into=c("Question", "Consultant"), sep = " - ")
#on renomme la 1er colonne
colnames(data_tidy)[1] <- "ID_consultant"
data_tidy[[1]] <- data_tidy[[1]] %>% as.numeric()

#on sauvegarde
questionnaire_manager <- data_tidy
save(questionnaire_manager, file="questionnaire_manager.RData")


####################"" Questioinnaire Consultants
####################"" Questioinnaire Consultants
####################"" Questioinnaire Consultants
####################"" Questioinnaire Consultants
#PR2017 - PERF2016 - M_Datalab.xls
data_consultant <- read_excel("PR2017 - PERF2016 - C_Datalab.xls", col_names = FALSE)

nbr_cols <- ncol(data_consultant)
nbr_rows <- nrow(data_consultant)

remove_html <- function(str_vec) {
  str_trim(str_replace_all(data_consultant[[i]], "<.*?>", ""))
}

remove_html_str <- function(str_vec) {
  str_trim(str_replace_all(str_vec, "<.*?>", ""))
}

respondant_ids <- data_consultant[-1,][[6]] %>% as.numeric()
# On recupere le nom des managers de la premiere question
for (i in 10:41) {
  if (is.character(data_consultant[[i]])) {
    data_consultant[[i]] <- remove_html(data_consultant[[i]])
  }
}
# on supprime les colonnes vides
null_cols <- vector()
for (i in seq_len(nbr_cols)) {
  if (sum(is.na(data_consultant[[i]]))/(nbr_rows - 1) == 1) {
    null_cols <- c(null_cols, i)
  }
}
#n supprime les lignes vides 
data_consultant[null_cols] <- NULL

#Les 2 premieres lignes contiennent le nom des colonnes a reconmposer
col_fields <- data_consultant[c(1, 2),]
#les données commencent à partir de la ligne 3
true_data <- data_consultant[-c(1, 2),]

#les questions sont en ligne 1
question_rows <- data_consultant[1, ]
#les personnes associées aux questions sont en ligne 2
name_rows <- data_consultant[2, ]

#Liste des questions posées Consutant
q1_str <- "Pour quelle personne ci-dessous considères-tu que ton avis n'est pas pertinent ?"
q2_str <- "Dans quelle mesure considères-tu que les personnes suivantes pensent Business Technology ?"
q3_str <- "Dans quelle mesure considères-tu que les personnes suivantes pensent différemment ?"
q4_str <- "Dans quelle mesure considères-tu que les personnes suivantes contribuent à l'intelligence connective ?"
q5_str <- "Dans quelle mesure considères-tu que les personnes suivantes savent tirer profit de la diversité créatrice de valeur ?"

#on supprime les 5 premieres colonnes
irrelevant_cols <- 1:5
data_consultant[irrelevant_cols] <- NULL

colnames(data_consultant) <- 1:ncol(data_consultant)

#Position des questions dans le fichier Excel +21
q1_cols <- 2:33
q2_cols <- 34:65
q3_cols <- 66:97
q4_cols <- 98:129
q5_cols <- 130:161
q2_5 <- 34:161

#Fonction qui permet de récupèrer le nom du manager sur la Q1
for (i in q1_cols) {
  data_consultant[1, i] <- q1_str
  data_consultant[2, i] <- str_trim(str_split(data_consultant[2, i], " - ")[[1]][1])
}

#fonction qui enleve le HTML sur les questions 2 à 5
for (i in q2_5) {
  data_consultant[1, i] <- remove_html_str(data_consultant[1, i])
  data_consultant[2, i] <- remove_html_str(data_consultant[2, i])
}

#Rappel de la questions sur la ligne 1
for (i in q2_cols) {
  data_consultant[1, i] <- q2_str
}

for (i in q3_cols) {
  data_consultant[1, i] <- q3_str
}

for (i in q4_cols) {
  data_consultant[1, i] <- q4_str
}

for (i in q5_cols) {
  data_consultant[1, i] <- q5_str
}

#On remplace sur les colonnes 2 à 5 le HTML par Content Mécotent Neutre
for (i in q2_5) {
  data_consultant[[i]][-c(1,2)] <- ifelse(str_detect(data_consultant[[i]][-c(1,2)], "Content"), "Content", 
                                       ifelse(str_detect(data_consultant[[i]][-c(1,2)], "Mécontent"), 
                                              "Mécontent", "Neutre"))
}

#On fusionne sur la ligne 1 la question avec le nom de l'associé
for (i in q1_cols) {
  data_consultant[1, i] <- str_c( data_consultant[1, i],data_consultant[2, i],  sep = " - ")
}

#on fusionne également sur les autres questions
for (i in q2_5) {
  data_consultant[1, i] <- str_c(data_consultant[1, i],data_consultant[2, i], sep = " - ")
}



#on supprime la ligne 2
data_consultant <- data_consultant[-2, ]

#on initialise le nom des colonnes
colnames(data_consultant) <- data_consultant[1, ]

#on supprime la ligne 1
data_consultant <- data_consultant[-1, ]

#data_manager[[6]] <- data_manager[[6]] %>% as.numeric()



# PARTIE de constitution des données sous la forme 
data_tidy <- data_consultant %>% gather(key=QA, value=response, -`Adresse e-mail`)



#on contitue 2 colonnes la colonne QA
data_tidy <- data_tidy %>% separate(col=QA, into=c( "Question","Consultant"), sep = " - ")

 
                                   
#on renomme la 1er colonne
colnames(data_tidy)[1] <- "ID_consultant"
data_tidy[[1]] <- data_tidy[[1]] %>% as.numeric()

#on sauvegarde
questionnaire_consultant<- data_tidy %>% select(ID_consultant, Question, Consultant, response)
save(questionnaire_consultant, file="questionnaire_consultant.RData")



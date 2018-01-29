# Load libs ----
library(readxl)
library(tidyr)
library(dplyr)
library(stringr)

# Auxiliary functions ----
remove_html_str <- function(str_vec) {
  str_trim(str_replace_all(str_vec, "<.*?>", ""))
}

# Formulaire Associé ----
# Load data
data_associe <- read_excel("raw-data/PR2017 - PERF2016 - A_Datalab.xls", col_names = FALSE)

# on supprime les colonnes vides ----
nbr_cols <- ncol(data_associe)
nbr_rows <- nrow(data_associe)
null_cols <- vector()
for (i in seq_len(nbr_cols)) {
  if (sum(is.na(data_associe[[i]]))/(nbr_rows - 1) == 1) {
    null_cols <- c(null_cols, i)
  }
}

data_associe[null_cols] <- NULL

# Position des questions ----
q1_str <- "Pour quelle personne ci-dessous considères-tu que ton avis n'est pas pertinent ?"
q2_str <- "Dans quelle mesure considères-tu que les associés suivants incarnent la valeur de leadership ?"
q3_str <- "Dans quelle mesure considères-tu que les associés suivants sont porteurs de vision ?"
q4_str <- "Dans quelle mesure considères-tu que les associés suivants incarnent un savoir-faire et savoir-être permettant l'origination commerciale ?"
q5_str <- "Dans quelle mesure considères-tu que les associés suivants incarnent un savoir-faire et savoir-être permettant le développement des collaborateurs ?"

q1_cols <- 7:11
q2_cols <- 12:16
q3_cols <- 17:21
q4_cols <- 22:26
q5_cols <- 27:31
q2_5 <- 12:31

question_rows <- data_associe[1, ]
name_rows <- data_associe[2, ]

for (i in q1_cols) {
  data_associe[1, i] <- q1_str
  data_associe[2, i] <- str_trim(str_split(remove_html_str(data_associe[2, i]), " - ")[[1]][1])
}

for (i in q2_5) {
  data_associe[1, i] <- remove_html_str(data_associe[1, i])
  data_associe[2, i] <- remove_html_str(data_associe[2, i])
}

# remplace les questions vides ----
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

# recoding reponses: Content Mécotent Neutre ----
for (i in q2_5) {
  data_associe[[i]][-c(1,2)] <- ifelse(str_detect(data_associe[[i]][-c(1,2)], "Content"), "Content", 
                                       ifelse(str_detect(data_associe[[i]][-c(1,2)], "Mécontent"), 
                                              "Mécontent", "Neutre"))
}

for (i in c(q1_cols, q2_5)) {
  data_associe[1, i] <- str_c(data_associe[1, i], data_associe[2, i], sep = " - ")
}

data_associe <- data_associe[-2, ]
colnames(data_associe) <- data_associe[1, ]
data_associe <- data_associe[-1, ]
data_associe[[6]] <- data_associe[[6]] %>% as.numeric()

irrelevant_cols <- c(1:5, 32)
data_associe[irrelevant_cols] <- NULL

colnames(data_associe)[1] <- "ID_consultant"
data_tidy <- data_associe %>% gather(key=QA, value=response, - ID_consultant)
data_tidy <- data_tidy %>% separate(QA, into=c("Question", "Associe"), sep = " - ")
questionnaire_associe <- data_tidy

# Extra info from global ----
accessible_ID <- c(32, 33, 61, 62)
names(accessible_ID) <- c("Julien Soyer", "Olivier Reisse", "Brice Escarguel", "Olivier Grandjean")

questionnaire_associe <- questionnaire_associe %>% 
  mutate(ID_cible = accessible_ID[Associe])

questionnaire_associe <- questionnaire_associe %>% 
  mutate(Associe = str_trim(Associe)) %>% 
  mutate(response = ifelse(Question == q1_str,
                           ifelse(is.na(response), "Je connais", "Je ne connais pas"), 
                           response))

questionnaire_associe <- questionnaire_associe %>% 
  mutate(qualite = recode(Question,
                          "Dans quelle mesure considères-tu que les associés suivants incarnent la valeur de leadership ?" = "LEADERSHIP",
                          "Dans quelle mesure considères-tu que les associés suivants sont porteurs de vision ?" = "VISION",
                          "Dans quelle mesure considères-tu que les associés suivants incarnent un savoir-faire et savoir-être permettant l'origination commerciale ?" = "ORIGINATION\nCOMMERCIALE",
                          "Dans quelle mesure considères-tu que les associés suivants incarnent un savoir-faire et savoir-être permettant le développement des collaborateurs ?" = "DEVELOPPEMENT\nDES\nCOLLABORATEURS"))

questionnaire_associe <- questionnaire_associe %>% 
  mutate(score = recode(response,
                        "Je connais" = 1,
                        "Je ne connais pas" = 0,
                        "Content" = 3,
                        "Neutre" = 2,
                        "Mécontent" = 1,
                        .missing = 0))

save(questionnaire_associe, file="pr_augmented_A/data/questionnaire_associe.RData")

# Formulaire Directeur / Manager ----
data_manager <- read_excel("raw-data/PR2017 - PERF2016 - M_Datalab.xls", col_names = FALSE)

nbr_cols <- ncol(data_manager)
nbr_rows <- nrow(data_manager)

null_cols <- vector()
for (i in seq_len(nbr_cols)) {
  if (sum(is.na(data_manager[[i]]))/(nbr_rows - 1) == 1) {
    null_cols <- c(null_cols, i)
  }
}
data_manager[null_cols] <- NULL

irrelevant_cols <- 1:5
data_manager[irrelevant_cols] <- NULL

q1_str <- "Pour quelle personne ci-dessous considères-tu que ton avis n'est pas pertinent ?"
q2_str <- "Dans quelle mesure considères-tu que les personnes suivantes incarnent la proposition de valeur BT ?"
q3_str <- "Dans quelle mesure considères-tu que les personnes suivants encouragent et créent les conditions nécessaires à l'incarnation de la proposition de valeur BT ?"
q4_str <- "Dans quelle mesure considères-tu que les personnes suivantes sont des bons managers opérationnels (direction de mission ou de dispositif interne) ?"
q5_str <- "Dans quelle mesure considères-tu que les personnes suivantes sont des bons coachs ?"

q1_cols <- 2:22
q2_cols <- 23:43
q3_cols <- 44:64
q4_cols <- 65:85
q5_cols <- 86:106
q2_5 <- 23:106

question_rows <- data_manager[1, ]
name_rows <- data_manager[2, ]

for (i in q1_cols) {
  data_manager[1, i] <- q1_str
  data_manager[2, i] <- str_trim(str_split(remove_html_str(data_manager[2, i]), " - ")[[1]][1])
}

for (i in q2_5) {
  data_manager[1, i] <- remove_html_str(data_manager[1, i])
  data_manager[2, i] <- remove_html_str(data_manager[2, i])
}

# remplace les questions vides ----
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

# recoding reponses: Content Mécotent Neutre ----
for (i in q2_5) {
  data_manager[[i]][-c(1,2)] <- ifelse(str_detect(data_manager[[i]][-c(1,2)], "Content"), "Content", 
                                       ifelse(str_detect(data_manager[[i]][-c(1,2)], "Mécontent"), 
                                              "Mécontent", "Neutre"))
}

for (i in c(q1_cols, q2_5)) {
  data_manager[1, i] <- str_c(data_manager[1, i], data_manager[2, i], sep = " - ")
}

data_manager <- data_manager[-2, ]
colnames(data_manager) <- data_manager[1, ]
data_manager <- data_manager[-1, ]
data_manager[[1]] <- data_manager[[1]] %>% as.numeric()

colnames(data_manager)[1] <- "ID_consultant"
data_tidy <- data_manager %>% gather(key=QA, value=response, - ID_consultant)
data_tidy <- data_tidy %>% 
  filter(QA != "As-tu des éléments à partager ?")
data_tidy <- data_tidy %>% separate(QA, into=c("Question", "DM"), sep = " - ")
questionnaire_dm <- data_tidy

# Extra info from global ----
accessible_ID <- c(32, 33, 61, 62)
names(accessible_ID) <- c("Julien Soyer", "Olivier Reisse", "Brice Escarguel", "Olivier Grandjean")

questionnaire_associe <- questionnaire_associe %>% 
  mutate(ID_cible = accessible_ID[Associe])

questionnaire_associe <- questionnaire_associe %>% 
  mutate(Associe = str_trim(Associe)) %>% 
  mutate(response = ifelse(Question == q1_str,
                           ifelse(is.na(response), "Je connais", "Je ne connais pas"), 
                           response))

questionnaire_associe <- questionnaire_associe %>% 
  mutate(qualite = recode(Question,
                          "Dans quelle mesure considères-tu que les associés suivants incarnent la valeur de leadership ?" = "LEADERSHIP",
                          "Dans quelle mesure considères-tu que les associés suivants sont porteurs de vision ?" = "VISION",
                          "Dans quelle mesure considères-tu que les associés suivants incarnent un savoir-faire et savoir-être permettant l'origination commerciale ?" = "ORIGINATION\nCOMMERCIALE",
                          "Dans quelle mesure considères-tu que les associés suivants incarnent un savoir-faire et savoir-être permettant le développement des collaborateurs ?" = "DEVELOPPEMENT\nDES\nCOLLABORATEURS"))

questionnaire_associe <- questionnaire_associe %>% 
  mutate(score = recode(response,
                        "Je connais" = 1,
                        "Je ne connais pas" = 0,
                        "Content" = 3,
                        "Neutre" = 2,
                        "Mécontent" = 1,
                        .missing = 0))

save(questionnaire_dm, file="pr_augmented_M/data/questionnaire_dm.RData")

# Formulaire Consultants ----
data_consultant <- read_excel("raw-data/PR2017 - PERF2016 - C_Datalab.xls", col_names = FALSE)

nbr_cols <- ncol(data_consultant)
nbr_rows <- nrow(data_consultant)

null_cols <- vector()
for (i in seq_len(nbr_cols)) {
  if (sum(is.na(data_consultant[[i]]))/(nbr_rows - 1) == 1) {
    null_cols <- c(null_cols, i)
  }
}
data_consultant[null_cols] <- NULL

irrelevant_cols <- 1:5
data_consultant[irrelevant_cols] <- NULL

q1_str <- "Pour quelle personne ci-dessous considères-tu que ton avis n'est pas pertinent ?"
q2_str <- "Dans quelle mesure considères-tu que les personnes suivantes pensent Business Technology ?"
q3_str <- "Dans quelle mesure considères-tu que les personnes suivantes pensent différemment ?"
q4_str <- "Dans quelle mesure considères-tu que les personnes suivantes contribuent à l'intelligence connective ?"
q5_str <- "Dans quelle mesure considères-tu que les personnes suivantes savent tirer profit de la diversité créatrice de valeur ?"

q1_cols <- 2:33
q2_cols <- 34:65
q3_cols <- 66:97
q4_cols <- 98:129
q5_cols <- 130:161
q2_5 <- 34:161

question_rows <- data_consultant[1, ]
name_rows <- data_consultant[2, ]

for (i in q1_cols) {
  data_consultant[1, i] <- q1_str
  data_consultant[2, i] <- str_trim(str_split(remove_html_str(data_consultant[2, i]), " - ")[[1]][1])
}

for (i in q2_5) {
  data_consultant[1, i] <- remove_html_str(data_consultant[1, i])
  data_consultant[2, i] <- remove_html_str(data_consultant[2, i])
}

# remplace les questions vides ----
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

# recoding reponses: Content Mécotent Neutre ----
for (i in q2_5) {
  data_consultant[[i]][-c(1,2)] <- ifelse(str_detect(data_consultant[[i]][-c(1,2)], "Content"), "Content", 
                                       ifelse(str_detect(data_consultant[[i]][-c(1,2)], "Mécontent"), 
                                              "Mécontent", "Neutre"))
}

for (i in c(q1_cols, q2_5)) {
  data_consultant[1, i] <- str_c(data_consultant[1, i], data_consultant[2, i], sep = " - ")
}

data_consultant <- data_consultant[-2, ]
colnames(data_consultant) <- data_consultant[1, ]
data_consultant <- data_consultant[-1, ]
data_consultant[[1]] <- data_consultant[[1]] %>% as.numeric()

colnames(data_consultant)[1] <- "ID_consultant"
data_tidy <- data_consultant %>% gather(key=QA, value=response, - ID_consultant)
data_tidy <- data_tidy %>% 
  filter(QA != "As-tu des éléments à partager ?")
data_tidy <- data_tidy %>% separate(QA, into=c("Question", "Consultant"), sep = " - ")
questionnaire_consultant <- data_tidy

save(questionnaire_consultant, file="pr_augmented_C/data/questionnaire_consultant.RData")

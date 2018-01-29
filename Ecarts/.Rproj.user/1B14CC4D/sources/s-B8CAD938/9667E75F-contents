library(shiny)
library(shinydashboard)
library(shinythemes)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(stringr)
library(forcats)
library(ggalt)

load("data/questionnaire_associe.RData")

q1_str <- "Pour quelle personne ci-dessous considéres-tu que ton avis n'est pas pertinent ?"
q2_str <- "Dans quelle mesure considères-tu que les associés suivants incarnent la valeur de leadership ?"
q3_str <- "Dans quelle mesure considères-tu que les associés suivants sont porteurs de vision ?"
q4_str <- "Dans quelle mesure considères-tu que les associés suivants incarnent un savoir-faire et savoir-Ãªtre permettant l'origination commerciale ?"
q5_str <- "Dans quelle mesure considères-tu que les associés suivants incarnent un savoir-faire et savoir-Ãªtre permettant le développement des collaborateurs ?"

questionnaire_associe <- questionnaire_associe %>% 
  mutate(ID_cible = ID_consultant)

questionnaire_associe <- questionnaire_associe %>% 
  mutate(Associe = str_trim(Associe)) %>% 
  mutate(response = ifelse(Question == q1_str,
                           ifelse(is.na(response), "Je connais", "Je ne connais pas"), 
                           response))

questionnaire_connaissance <- questionnaire_associe %>% 
  filter(Question == q1_str)

questionnaire_qualites <- questionnaire_associe %>% 
  filter(Question %in% c(q2_str, q3_str, q4_str, q5_str))

questionnaire_qualites <- questionnaire_qualites %>% 
  mutate(qualite = recode(Question,
                          "Dans quelle mesure considéres-tu que les associés suivants incarnent la valeur de leadership ?" = "LEADERSHIP",
                          "Dans quelle mesure considéres-tu que les associés suivants sont porteurs de vision ?" = "VISION",
                          "Dans quelle mesure considéres-tu que les associés suivants incarnent un savoir-faire et savoir-Ãªtre permettant l'origination commerciale ?" = "ORIGINATION\nCOMMERCIALE",
                          "Dans quelle mesure considéres-tu que les associés suivants incarnent un savoir-faire et savoir-Ãªtre permettant le développement des collaborateurs ?" = "DEVELOPPEMENT\nDES\nCOLLABORATEURS"))

questionnaire_qualites <- questionnaire_qualites %>% 
  mutate(score = recode(response,
                        "Content" = 3,
                        "Neutre" = 2,
                        "Mécontent" = 1,
                        .missing = 0))

accessible_ID <- c(24, 25, 52, 53)
names(accessible_ID) <- c("Julien Soyer", "Olivier Reisse", "Brice Escarguel", "Olivier Grandjean")

asso_ranking <- questionnaire_qualites %>% 
  mutate(Asso_ID = accessible_ID[Associe]) %>% 
  filter(!is.na(Asso_ID)) %>% 
  group_by(Asso_ID, qualite) %>% 
  summarise(score_moyen = mean(score)) %>% 
  ungroup() %>% 
  group_by(qualite) %>% 
  mutate(son_rang = rank(score_moyen, ties.method = "min")) %>% 
  ungroup()



#####################
#Liste des questions posées Managers
q1C_str <- "Pour quelle personne ci-dessous considères-tu que ton avis n'est pas pertinent ?"
q2C_str <- "Dans quelle mesure considères-tu que les personnes suivantes pensent Business Technology ?"
q3C_str <- "Dans quelle mesure considères-tu que les personnes suivants pensent différemment ?"
q4C_str <- "Dans quelle mesure considères-tu que les personnes suivantes contribuent à l'intelligence connective ?"
q5C_str <- "Dans quelle mesure considères-tu que les personnes suivantes savent tirer profit de la diversité créatrice de valeur ?"

load("data/questionnaire_Consultant.RData")

questionnaire_consultant <- questionnaire_Consultant %>% 
  mutate(ID_cible = ID_consultant)

questionnaire_consultant <- questionnaire_consultant %>% 
  mutate(Consultant = str_trim(Consultant)) %>% 
  mutate(response = ifelse(Question == q1C_str,
                           ifelse(is.na(response), "Je connais", "Je ne connais pas"), 
                           response))

questionnaire_connaissanceC <- questionnaire_consultant %>% 
  filter(Question != q1C_str) 

questionnaire_qualitesC <- questionnaire_consultant %>% 
  filter(Question %in% c(q2C_str, q3C_str, q4C_str, q5C_str))

questionnaire_qualitesC <- questionnaire_qualitesC %>% 
  mutate(qualite = recode(Question,
                          q2C_str = "PENSE BT",
                          q3C_str = "PENSE DIFFERMENT",
                          q4C_str = "INTELLIGENCE CONNECTIVE",
                          q5C_str = "DIVERSITE CREATRICE"))

questionnaire_qualitesC <- questionnaire_qualitesC %>% 
  mutate(score = recode(response,
                        "Content" = 3,
                        "Neutre" = 2,
                        "Mécontent" = 1,
                        .missing = 0))

accessible_ID_C <- c(24, 25, 52, 53)


names(accessible_ID) <- c("Julien Soyer", "Olivier Reisse", "Brice Escarguel", "Olivier Grandjean")

asso_rankingC <- questionnaire_qualitesC %>% 
  mutate(Cons_ID = 10) %>% 
  filter(!is.na(Cons_ID)) %>% 
  group_by(Cons_ID, qualite) %>% 
  summarise(score_moyen = mean(score)) %>% 
  ungroup() %>% 
  group_by(qualite) %>% 
  mutate(son_rang = rank(score_moyen, ties.method = "min")) %>% 
  ungroup()


library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(stringr)
library(forcats)
library(ggalt)

load("data/questionnaire_associe.RData")

q1_str <- "Pour quelle personne ci-dessous considères-tu que ton avis n'est pas pertinent ?"
q2_str <- "Dans quelle mesure considères-tu que les associés suivants incarnent la valeur de leadership ?"
q3_str <- "Dans quelle mesure considères-tu que les associés suivants sont porteurs de vision ?"
q4_str <- "Dans quelle mesure considères-tu que les associés suivants incarnent un savoir-faire et savoir-être permettant l'origination commerciale ?"
q5_str <- "Dans quelle mesure considères-tu que les associés suivants incarnent un savoir-faire et savoir-être permettant le développement des collaborateurs ?"

questionnaire_associe <- questionnaire_associe %>% 
  mutate(ID_cible = ID_consultant)

questionnaire_associe <- questionnaire_associe %>% 
  mutate(Consultant = str_trim(Consultant)) %>% 
  mutate(response = ifelse(Question == q1_str,
                           ifelse(is.na(response), "Je connais", "Je ne connais pas"), 
                           response))

questionnaire_connaissance <- questionnaire_associe %>% 
  filter(Question == q1_str)

questionnaire_qualites <- questionnaire_associe %>% 
  filter(Question %in% c(q2_str, q3_str, q4_str, q5_str))

questionnaire_qualites <- questionnaire_qualites %>% 
  mutate(qualite = recode(Question,
                          "Dans quelle mesure considères-tu que les associés suivants incarnent la valeur de leadership ?" = "LEADERSHIP",
                          "Dans quelle mesure considères-tu que les associés suivants sont porteurs de vision ?" = "VISION",
                          "Dans quelle mesure considères-tu que les associés suivants incarnent un savoir-faire et savoir-être permettant l'origination commerciale ?" = "ORIGINATION\nCOMMERCIALE",
                          "Dans quelle mesure considères-tu que les associés suivants incarnent un savoir-faire et savoir-être permettant le développement des collaborateurs ?" = "DEVELOPPEMENT\nDES\nCOLLABORATEURS"))

questionnaire_qualites <- questionnaire_qualites %>% 
  mutate(score = recode(response,
                        "Content" = 3,
                        "Neutre" = 2,
                        "Mécontent" = 1,
                        .missing = 0))

accessible_ID <- c(32, 33, 61, 62)
names(accessible_ID) <- c("Julien Soyer", "Olivier Reisse", "Brice Escarguel", "Olivier Grandjean")

asso_ranking <- questionnaire_qualites %>% 
  mutate(Asso_ID = accessible_ID[Consultant]) %>% 
  filter(!is.na(Asso_ID)) %>% 
  group_by(Asso_ID, qualite) %>% 
  summarise(score_moyen = mean(score)) %>% 
  ungroup() %>% 
  group_by(qualite) %>% 
  mutate(son_rang = rank(score_moyen, ties.method = "min")) %>% 
  ungroup()
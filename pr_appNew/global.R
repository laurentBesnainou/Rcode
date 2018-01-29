library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(forcats)

load("data/questionnaire_associe.RData")

q1_str <- "Pour quelle personne ci-dessous considères-tu que ton avis n'est pas pertinent ?"
q2_str <- "Dans quelle mesure considères-tu que les associés suivants incarnent la valeur de leadership ?"
q3_str <- "Dans quelle mesure considères-tu que les associés suivants sont porteurs de vision ?"
q4_str <- "Dans quelle mesure considères-tu que les associés suivants incarnent un savoir-faire et savoir-Ãªtre permettant l'origination commerciale ?"
q5_str <- "Dans quelle mesure considères-tu que les associés suivants incarnent un savoir-faire et savoir-Ãªtre permettant le développement des collaborateurs ?"


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
                       "Dans quelle mesure considères-tu que les associés suivants incarnent la valeur de leadership ?" = "LEADERSHIP",
                       "Dans quelle mesure considères-tu que les associés suivants sont porteurs de vision ?" = "VISION",
                       "Dans quelle mesure considères-tu que les associés suivants incarnent un savoir-faire et savoir-Ãªtre permettant l'origination commerciale ?" = "ORIGINATION\nCOMMERCIALE",
                       "Dans quelle mesure considères-tu que les associés suivants incarnent un savoir-faire et savoir-Ãªtre permettant le développement des collaborateurs ?" = "DEVELOPPEMENT\nDES\nCOLLABORATEURS"))

questionnaire_qualites <- questionnaire_qualites %>% 
  mutate(score = recode(response,
                        "Content" = 3,
                        "Neutre" = 2,
                        "Mécontent" = 0,
                        .missing = 0))

tmp <- questionnaire_connaissance %>% filter(response=="Je connais") %>% select(ID_consultant,Associe)

matriceConnais <- xtabs(~ ID_consultant + Associe, na.omit(tmp))
library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(forcats)
library(circlize)

load("data/questionnaire_associe.RData")

q1_str <- "Pour quelle personne ci-dessous considères-tu que ton avis n'est pas pertinent ?"
q2_str <- "Dans quelle mesure considères-tu que les associés suivants incarnent la valeur de leadership ?"
q3_str <- "Dans quelle mesure considères-tu que les associés suivants sont porteurs de vision ?"
q4_str <- "Dans quelle mesure considères-tu que les associés suivants incarnent un savoir-faire et savoir-être permettant l'origination commerciale ?"
q5_str <- "Dans quelle mesure considères-tu que les associés suivants incarnent un savoir-faire et savoir-être permettant le développement des collaborateurs ?"

Associes <- c(24,25,52,53)

questionnaire_associe <- questionnaire_associe %>% 
  mutate(response = ifelse(Question == q1_str,
                           ifelse(is.na(response), "Je connais", "Je ne connais pas"), 
                           response))

questionnaire_connaissance <- questionnaire_associe %>% 
  filter(Question == q1_str)

questionnaires_qualites <- questionnaire_associe %>% 
  filter(Question %in% c(q2_str, q3_str, q4_str, q5_str))

tmp <- questionnaire_connaissance %>% filter(response=="Je connais") %>% select(ID_consultant,Associe)

matriceConnais <- xtabs(~ ID_consultant + Associe, na.omit(tmp))
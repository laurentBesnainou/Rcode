library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(stringr)
library(forcats)

load("data/questionnaire_dm.RData")

#Liste des questions posées Managers
q1_str <- "Pour quelle personne ci-dessous considères-tu que ton avis n'est pas pertinent ?"
q2_str <- "Dans quelle mesure considères-tu que les personnes suivantes incarnent la proposition de valeur BT ?"
q3_str <- "Dans quelle mesure considères-tu que les personnes suivants encouragent et créent les conditions nécessaires à l'incarnation de la proposition de valeur BT ?"
q4_str <- "Dans quelle mesure considères-tu que les personnes suivantes sont des bons managers opérationnels (direction de mission ou de dispositif interne) ?"
q5_str <- "Dans quelle mesure considères-tu que les personnes suivantes sont des bons coachs ?"

questionnaire_connaissance <- questionnaire_dm %>% 
  filter(Question == q1_str)

questionnaire_qualites <- questionnaire_dm %>% 
  filter(Question %in% c(q2_str, q3_str, q4_str, q5_str))

dm_ranking <- questionnaire_qualites %>% 
  group_by(DM, qualite) %>% 
  summarise(score_moyen = mean(score)) %>% 
  ungroup() %>% 
  group_by(qualite) %>% 
  mutate(son_rang = rank(- score_moyen, ties.method = "min")) %>% 
  ungroup()
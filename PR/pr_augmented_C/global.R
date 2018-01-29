library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(stringr)
library(forcats)

load("data/questionnaire_consultant.RData")

#Liste des questions posées Consultants
q1_str <- "Pour quelle personne ci-dessous considères-tu que ton avis n'est pas pertinent ?"
q2_str <- "Dans quelle mesure considères-tu que les personnes suivantes pensent Business Technology ?"
q3_str <- "Dans quelle mesure considères-tu que les personnes suivantes pensent différemment ?"
q4_str <- "Dans quelle mesure considères-tu que les personnes suivantes contribuent à l'intelligence connective ?"
q5_str <- "Dans quelle mesure considères-tu que les personnes suivantes savent tirer profit de la diversité créatrice de valeur ?"

questionnaire_connaissance <- questionnaire_consultant %>% 
  filter(Question == q1_str)

questionnaire_qualites <- questionnaire_consultant %>% 
  filter(Question %in% c(q2_str, q3_str, q4_str, q5_str))

consultant_ranking <- questionnaire_qualites %>% 
  group_by(Consultant, qualite) %>% 
  summarise(score_moyen = mean(score)) %>% 
  ungroup() %>% 
  group_by(qualite) %>% 
  mutate(son_rang = rank(- score_moyen, ties.method = "min")) %>% 
  ungroup()

asso_moyenne <- questionnaire_qualites %>% 
  group_by(qualite) %>% summarise(moyenne = mean(score))

asso_ranking <-questionnaire_qualites %>% 
  group_by(Consultant, qualite) %>% 
  summarise(score_moyen = mean(score)) %>% 
  arrange(qualite, -score_moyen) %>%
  ungroup() %>% 
  group_by(qualite) %>% 
  mutate(son_rang=row_number()) %>%
  ungroup()

asso_ranking <-left_join(asso_ranking, asso_moyenne)
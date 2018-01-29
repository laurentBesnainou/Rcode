library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(stringr)
library(forcats)

load("data/questionnaire_associe.RData")

q1_str <- "Pour quelle personne ci-dessous considères-tu que ton avis n'est pas pertinent ?"
q2_str <- "Dans quelle mesure considères-tu que les associés suivants incarnent la valeur de leadership ?"
q3_str <- "Dans quelle mesure considères-tu que les associés suivants sont porteurs de vision ?"
q4_str <- "Dans quelle mesure considères-tu que les associés suivants incarnent un savoir-faire et savoir-être permettant l'origination commerciale ?"
q5_str <- "Dans quelle mesure considères-tu que les associés suivants incarnent un savoir-faire et savoir-être permettant le développement des collaborateurs ?"

questionnaire_connaissance <- questionnaire_associe %>% 
  filter(Question == q1_str)

questionnaire_qualites <- questionnaire_associe %>% 
  filter(Question %in% c(q2_str, q3_str, q4_str, q5_str))

asso_moyenne <- questionnaire_qualites %>% 
  group_by(qualite) %>% summarise(moyenne = mean(score))

# asso_ranking <- questionnaire_qualites %>% 
#   group_by(Associe, qualite) %>% 
#   summarise(score_moyen = mean(score)) %>% 
#   ungroup() %>% 
#   group_by(qualite) %>% 
#   mutate(son_rang = rank(- score_moyen, ties.method = "min")) %>% 
#   ungroup()

asso_ranking <-questionnaire_qualites %>% 
  group_by(Associe, qualite) %>% 
  summarise(score_moyen = mean(score)) %>% 
  arrange(qualite, -score_moyen) %>%
  ungroup() %>% 
  group_by(qualite) %>% 
  mutate(son_rang=row_number()) %>%
  ungroup()
 

asso_ranking <-left_join(asso_ranking, asso_moyenne)

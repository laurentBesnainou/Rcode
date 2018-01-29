library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(stringr)
library(forcats)
library(ggalt)

load("data/questionnaire_consultant.RData")

#Liste des questions posées Consultants
q1_str <- "Pour quelle personne ci-dessous considères-tu que ton avis n'est pas pertinent ?"
q2_str <- "Dans quelle mesure considères-tu que les personnes suivantes pensent Business Technology ?"
q3_str <- "Dans quelle mesure considères-tu que les personnes suivantes pensent différemment ?"
q4_str <- "Dans quelle mesure considères-tu que les personnes suivantes contribuent à l'intelligence connective ?"
q5_str <- "Dans quelle mesure considères-tu que les personnes suivantes savent tirer profit de la diversité créatrice de valeur ?"

questionnaire_consultant <- questionnaire_consultant %>% 
  mutate(ID_cible = ID_consultant)

questionnaire_consultant <- questionnaire_consultant %>% 
  mutate(Consultant = str_trim(Consultant)) %>% 
  mutate(response = ifelse(Question == q1_str,
                           ifelse(is.na(response), "Je connais", "Je ne connais pas"), 
                           response))

questionnaire_connaissance <- questionnaire_consultant %>% 
  filter(Question == q1_str)

questionnaire_qualites <- questionnaire_consultant %>% 
  filter(Question %in% c(q2_str, q3_str, q4_str, q5_str))

questionnaire_qualites <- questionnaire_qualites %>% 
  mutate(qualite = recode(Question,
                          "Dans quelle mesure considères-tu que les personnes suivantes pensent Business Technology ?" = "PENSE BT",
                          "Dans quelle mesure considères-tu que les personnes suivantes pensent différemment ?" = "PENSE DIFF",
                          "Dans quelle mesure considères-tu que les personnes suivantes contribuent à l'intelligence connective ?" = "INTLLIGENCE\nCONNECTIVE",
                          "Dans quelle mesure considères-tu que les personnes suivantes savent tirer profit de la diversité créatrice de valeur ?" = "DIVERSITE\nCREATRICE\nVALEUR"))

questionnaire_qualites <- questionnaire_qualites %>% 
  mutate(score = recode(response,
                        "Content" = 3,
                        "Neutre" = 2,
                        "Mécontent" = 1,
                        .missing = 0))

accessible_ID <- c(12, 13, 15, 17, 18,
                   21, 23, 25, 26, 28,
                   30, 32, 33, 34, 38,
                   39, 40, 41, 45, 47,
                   48, 49, 50, 51, 52,
                   56, 57, 58, 59, 62, 
                   63, 64, 66, 67, 68, 
                   69, 70, 71)


names(accessible_ID) <- c("Thomas Roche", "Alexandre Mur", "Emile Saloum", "Abdelilah Chouai", "Manfred Le Callonnec",
                          "Aurore Collumeau", "Bénédicte Lissoir", "Benedicte Lissoir2", "Edouard Tardu", "Niamkey Ackable",
                          "Niamkey Ackable2", "Sandy Malosse", "Maud Duforest", "Benjamin Faibis", "Tiphaine Lorant",
                          "Victoire de la Boulaye", "Charles Velten-Jameson", "Julie Maillard", "Laurent Besnainou","claire marchive", 
                          "Antoine Petit", "Charles-Guillaume Vanheeckhoet", "Erwan Thion", 
                          "Sybille Urvoy", "Margaux Lanfranchi", "Clément Coicault", "Junel Popp", "Pierrick Gall", 
                          "Renée-Laurie Sturtzer", "Francois Dias", "Quentin Hureau", "Juliette Beylle", "Damien Berger", 
                          "Nicolas Suchaud", "Tiphaine Gascon", "Romain Brunat", "Eymeline Pernet", "Clement Coicault")
#Manque Paulie Audinet, pb "Aurore Collumeau" Dans le sondage?"Niamkey Ackable"


asso_ranking <- questionnaire_qualites %>% 
  mutate(Asso_ID = accessible_ID[Consultant]) %>% 
  filter(!is.na(Asso_ID)) %>% 
  group_by(Asso_ID, qualite) %>% 
  summarise(score_moyen = mean(score)) %>% 
  ungroup() %>% 
  group_by(qualite) %>% 
  mutate(son_rang = rank(score_moyen, ties.method = "min")) %>% 
  ungroup()

asso_ranking <-questionnaire_qualites %>% 
  group_by(Associe, qualite) %>% 
  summarise(score_moyen = mean(score)) %>% 
  arrange(qualite, -score_moyen) %>%
  ungroup() %>% 
  group_by(qualite) %>% 
  mutate(son_rang=row_number()) %>%
  ungroup()
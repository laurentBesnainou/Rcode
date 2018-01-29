library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(stringr)
library(forcats)
library(ggalt)

load("data/questionnaire_manager.RData")

#Liste des questions posées Consultants
q1_str <- "Pour quelle personne ci-dessous considères-tu que ton avis n'est pas pertinent ?"
q2_str <- "Dans quelle mesure considères-tu que les personnes suivantes incarnent la proposition de valeur BT ?"
q3_str <- "Dans quelle mesure considères-tu que les personnes suivants encouragent et créent les conditions nécessaires à l'incarnation de la proposition de valeur BT ?"
q4_str <- "Dans quelle mesure considères-tu que les personnes suivantes sont des bons managers opérationnels (direction de mission ou de dispositif interne) ?"
q5_str <- "Dans quelle mesure considères-tu que les personnes suivantes sont des bons coachs ?"

questionnaire_manager <- questionnaire_manager %>% 
  mutate(ID_cible = ID_consultant)

questionnaire_manager <- questionnaire_manager %>% 
  mutate(Consultant = str_trim(Consultant)) %>% 
  mutate(response = ifelse(Question == q1_str,
                           ifelse(is.na(response), "Je connais", "Je ne connais pas"), 
                           response))

questionnaire_connaissance <- questionnaire_manager %>% 
  filter(Question == q1_str)

questionnaire_qualites <- questionnaire_manager %>% 
  filter(Question %in% c(q2_str, q3_str, q4_str, q5_str))

questionnaire_qualites <- questionnaire_qualites %>% 
  mutate(qualite = recode(Question,
                          "Dans quelle mesure considères-tu que les personnes suivantes incarnent la proposition de valeur BT ?" = "INCARNE BT",
                          "Dans quelle mesure considères-tu que les personnes suivants encouragent et créent les conditions nécessaires à l'incarnation de la proposition de valeur BT ?" = "PROPOSITION BT",
                          "Dans quelle mesure considères-tu que les personnes suivantes sont des bons managers opérationnels (direction de mission ou de dispositif interne) ?" = "BON MANAGER",
                          "Dans quelle mesure considères-tu que les personnes suivantes sont des bons coachs ?" = "BON COACH"))

questionnaire_qualites <- questionnaire_qualites %>% 
  mutate(score = recode(response,
                        "Content" = 3,
                        "Neutre" = 2,
                        "Mécontent" = 1,
                        .missing = 0))

accessible_ID <- c(11, 14, 16, 18, 21, 25, 27, 29, 32, 38, 42, 44, 45, 54, 58, 61, 62, 66, 67,30,31)


names(accessible_ID) <- c("Thibault Hudry", "Nicolas Quesnoit", "Cédric Genoyer", 
                          "Nourallah Seddiki", "Benjamin Faibis", "Jean-Patrick Girard", 
                          "Aude Saboul-Rosalie", "Martin Lauquin", "Matthieu Salles", "Jonathan Lepan", 
                          "Moez Louati", "Jonathan Vercruysse", "Grégory Denis", "Julien Doutremepuich", 
                          "Anthony Régent","Michael Sutter", "Marc Boutoille", "Jacques Lefebvre", 
                          "Laurent Pernel", "Sylvain Marty", "Olivier Tresor")


#manque S Marty, O. Tresor
#Pb Marc Boutoille



asso_ranking <- questionnaire_qualites %>% 
  mutate(Asso_ID = accessible_ID[Consultant]) %>% 
  filter(!is.na(Asso_ID)) %>% 
  group_by(Asso_ID, qualite) %>% 
  summarise(score_moyen = mean(score)) %>% 
  ungroup() %>% 
  group_by(qualite) %>% 
  mutate(son_rang = rank(score_moyen)) %>%
  ungroup()
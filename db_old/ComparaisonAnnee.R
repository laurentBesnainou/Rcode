library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(ggthemes)
library(tidyr)
setwd("D:/Data/R_Sources/dashboardBTV2")

load("data/pilotage2015_data.RData")
pilotage_2015 <- as_data_frame(pilotage_2015)

load("data/pilotage2016_data.RData")
pilotage_2016 <- as_data_frame(pilotage_2016)

load("data/pilotage_data.RData")
pilotage_2017 <- as_data_frame(pilotage_data)

pilotage_2015 <- pilotage_2015 %>% 
  select(STEP,
         CA_BT__N__KE,
         OFFRE_PRINCIPALE,
         WEEK) %>% 
  mutate(YEAR = 2015)

pilotage_2016 <- pilotage_2016 %>% 
  select(STEP,
         CA_BT__N__KE,
         OFFRE_PRINCIPALE,
         WEEK) %>% 
  mutate(YEAR = 2016)

pilotage_2017 <- pilotage_2017 %>% 
  select(STEP,
         CA_BT__N__KE,
         OFFRE_PRINCIPALE,
         WEEK) %>% 
  mutate(YEAR = 2017)

pilotage_tot <- bind_rows(pilotage_2015, pilotage_2016, pilotage_2017)

pilotage_tot <- pilotage_tot %>% 
  rename(CA_BT = CA_BT__N__KE)

format_ca <- function(x) {
  str_c(format(x, big.mark = " "), " K€")
}

max_week_2017 <- max(pilotage_2017$WEEK)

pilotage_tot %>% 
  mutate(YEAR = factor(YEAR)) %>% 
  filter(STEP == "4 - Gagnée") %>% 
  group_by(YEAR, WEEK) %>% 
  summarise(CA_TOT = sum(CA_BT, na.rm = TRUE)) %>%
  ungroup() %>% 
  ggplot(aes(x = WEEK, y = CA_TOT, colour = YEAR)) +
  geom_line() +
  geom_vline(xintercept = max_week_2017) +
  coord_cartesian(ylim = c(1000, 15000)) +
  scale_y_continuous(labels = format_ca) +
  labs(x = "Numéro de semaine",
       y = "CA total BT") +
  theme_bw()

pilotage_tot %>% 
  mutate(YEAR = factor(YEAR)) %>% 
  filter(STEP == "4 - Gagnée") %>%
  filter(!is.na(OFFRE_PRINCIPALE)) %>% 
  group_by(YEAR, WEEK, OFFRE_PRINCIPALE) %>% 
  summarise(CA_TOT = sum(CA_BT, na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(OFFRE_PRINCIPALE = factor(OFFRE_PRINCIPALE)) %>% 
  filter(OFFRE_PRINCIPALE != "IT Strategy") %>% # remove one offer
  filter(OFFRE_PRINCIPALE != "PocLab") %>% 
  ggplot(aes(x = WEEK, y = CA_TOT, colour = YEAR)) +
  geom_line() +
  geom_vline(xintercept = max_week_2017) +
  #coord_cartesian(ylim = c(1000, 15000)) +
  scale_y_continuous(labels = format_ca) +
  labs(x = "Numéro de semaine",
       y = "CA total BT") +
  facet_wrap(~ OFFRE_PRINCIPALE) +
  theme_bw()

#On charge les effectifs 2015 à 2017
load("data/effectifs2016.RData")
load("data/effectif2015.RData")
load("data/effectif2017.RData")

week_seq15 <- data_frame("week_seq" = seq(dmy("01/01/2015"), dmy("31/12/2015"), by = "week"))
week_seq16 <- data_frame("week_seq" = seq(dmy("01/01/2016"), dmy("31/12/2016"), by = "week"))
week_seq17 <- data_frame("week_seq" = seq(dmy("01/01/2017"), dmy("31/12/2017"), by = "week"))

effectif_2016 <- Effectifs2016 %>% 
  select(NOM, DATE_ENTREE, DATESORTIE, ETAT, GRADE_2016) %>% 
  crossing(week_seq16) %>% 
  mutate(DATE_ENTREE = ymd(DATE_ENTREE),
         DATESORTIE = ymd(DATESORTIE)) %>% 
  replace_na(list(DATESORTIE = dmy("01/01/2018"))) %>% 
  mutate(present_window = interval(DATE_ENTREE, DATESORTIE)) %>% 
  mutate(is_present = week_seq %within% present_window) %>% 
  mutate(poids_effectif = ifelse(GRADE_2016 == "8-Associé", 2, 
                                 ifelse(GRADE_2016 == "7-Directeur", 1/0.75, 1))) %>% 
  filter(is_present == TRUE) %>% 
  mutate(WEEK = week(week_seq)) %>% 
  group_by(WEEK) %>% 
  summarise(POIDS_EFFECTIF_TOT = sum(poids_effectif)) %>% 
  mutate(YEAR = 2016)


effectif_2015 <- Effectifs2015 %>% 
  select(NOM, DATE_ENTREE, DATESORTIE, ETAT, GRADE_2016) %>% 
  crossing(week_seq15) %>% 
  mutate(DATE_ENTREE = ymd(DATE_ENTREE),
         DATESORTIE = ymd(DATESORTIE)) %>% 
  replace_na(list(DATESORTIE = dmy("01/01/2018"))) %>% 
  mutate(present_window = interval(DATE_ENTREE, DATESORTIE)) %>% 
  mutate(is_present = week_seq %within% present_window) %>% 
  mutate(poids_effectif = ifelse(GRADE_2016 == "8-Associé", 2, 
                                 ifelse(GRADE_2016 == "7-Directeur", 1/0.75, 1))) %>% 
  filter(is_present == TRUE) %>% 
  mutate(WEEK = week(week_seq)) %>% 
  group_by(WEEK) %>% 
  summarise(POIDS_EFFECTIF_TOT = sum(poids_effectif)) %>% 
  mutate(YEAR = 2015)

effectif_2017 <- Effectifs2017 %>% 
  select(NOM, DATE_ENTREE, DATESORTIE, ETAT, GRADE_2016) %>% 
  crossing(week_seq17) %>% 
  mutate(DATE_ENTREE = ymd(DATE_ENTREE),
         DATESORTIE = ymd(DATESORTIE)) %>% 
  replace_na(list(DATESORTIE = dmy("01/01/2018"))) %>% 
  mutate(present_window = interval(DATE_ENTREE, DATESORTIE)) %>% 
  mutate(is_present = week_seq %within% present_window) %>% 
  mutate(poids_effectif = ifelse(GRADE_2016 == "8-Associé", 2, 
                                 ifelse(GRADE_2016 == "7-Directeur", 1/0.75, 1))) %>% 
  filter(is_present == TRUE) %>% 
  mutate(WEEK = week(week_seq)) %>% 
  group_by(WEEK) %>% 
  summarise(POIDS_EFFECTIF_TOT = sum(poids_effectif)) %>% 
  mutate(YEAR = 2017)


p2016 <- pilotage_tot %>% 
  filter(YEAR == 2016) %>% 
  filter(STEP == "4 - Gagnée") %>%
  left_join(effectif_2016, by = c("WEEK", "YEAR")) %>% 
  group_by(YEAR, WEEK) %>% 
  summarise(CA_TOT = sum(CA_BT, na.rm = TRUE),
            POIDS_EFFECTIF_TOT = first(POIDS_EFFECTIF_TOT)) %>%
  mutate(ANNEE=2016)

P2015 <- pilotage_tot %>% 
  filter(YEAR == 2015) %>%
  filter(STEP == "4 - Gagnée") %>%
  left_join(effectif_2015, by = c("WEEK", "YEAR")) %>% 
  group_by(YEAR, WEEK) %>% 
  summarise(CA_TOT = sum(CA_BT, na.rm = TRUE),
            POIDS_EFFECTIF_TOT = first(POIDS_EFFECTIF_TOT)) %>%
  mutate(ANNEE=2015)

P2017 <- pilotage_tot %>% 
  filter(YEAR == 2017) %>% 
  filter(STEP == "4 - Gagnée") %>%
  left_join(effectif_2017, by = c("WEEK", "YEAR")) %>% 
  group_by(YEAR, WEEK) %>% 
  summarise(CA_TOT = sum(CA_BT, na.rm = TRUE),
            POIDS_EFFECTIF_TOT = first(POIDS_EFFECTIF_TOT)) %>%
  mutate(ANNEE=2017)

p <- rbind(P2015,p2016,P2017)

p %>% mutate(ANNEE = factor(ANNEE)) %>%
  mutate(CA_NORM = CA_TOT / POIDS_EFFECTIF_TOT) %>% 
  ggplot(aes(x = WEEK, y = CA_NORM, colour = ANNEE)) +
  geom_line() +
  scale_y_continuous(labels = format_ca) +
  labs(x = "Numéro de semaine",
       y = "CA normalisé") +
  #facet_wrap(~ OFFRE_PRINCIPALE) +
  ggtitle("Evolution du CA BT pondéré par l'effectif à date") + 
  theme_bw()

########### Analyse du pipe

pilotage_tot %>% 
  mutate(YEAR = factor(YEAR)) %>% 
  filter(STEP %in% c("0 - A qualifier", "1 - Qualifiée", "2 - A émettre", "3 - Emise" )) %>%

  group_by(YEAR, WEEK) %>% 
  summarise(CA_TOT = sum(CA_BT, na.rm = TRUE)) %>%
  ungroup() %>% 
  ggplot(aes(x = WEEK, y = CA_TOT, colour = YEAR)) +
  geom_line() +
  geom_vline(xintercept = max_week_2017) +
  coord_cartesian(ylim = c(1000, 15000)) +
  scale_y_continuous(labels = format_ca) +
  labs(x = "Numéro de semaine",
       y = "CA possible") +
  ggtitle("Comparaison du volume du Pipe")
  theme_bw()

p2016 <- pilotage_tot %>% 
  filter(YEAR == 2016) %>% 
  filter(STEP %in% c("0 - A qualifier", "1 - Qualifiée", "2 - A émettre", "3 - Emise" )) %>%
  left_join(effectif_2016, by = c("WEEK", "YEAR")) %>% 
  group_by(YEAR, WEEK) %>% 
  summarise(CA_TOT = sum(CA_BT, na.rm = TRUE),
            POIDS_EFFECTIF_TOT = first(POIDS_EFFECTIF_TOT)) %>%
  mutate(ANNEE=2016)

P2015 <- pilotage_tot %>% 
  filter(YEAR == 2015) %>%
  filter(STEP %in% c("0 - A qualifier", "1 - Qualifiée", "2 - A émettre", "3 - Emise" )) %>%
  left_join(effectif_2015, by = c("WEEK", "YEAR")) %>% 
  group_by(YEAR, WEEK) %>% 
  summarise(CA_TOT = sum(CA_BT, na.rm = TRUE),
            POIDS_EFFECTIF_TOT = first(POIDS_EFFECTIF_TOT)) %>%
  mutate(ANNEE=2015)

P2017 <- pilotage_tot %>% 
  filter(YEAR == 2017) %>% 
  filter(STEP %in% c("0 - A qualifier", "1 - Qualifiée", "2 - A émettre", "3 - Emise" )) %>%
  left_join(effectif_2017, by = c("WEEK", "YEAR")) %>% 
  group_by(YEAR, WEEK) %>% 
  summarise(CA_TOT = sum(CA_BT, na.rm = TRUE),
            POIDS_EFFECTIF_TOT = first(POIDS_EFFECTIF_TOT)) %>%
  mutate(ANNEE=2017)

p <- rbind(P2015,p2016,P2017)

p %>% mutate(ANNEE = factor(ANNEE)) %>%
  mutate(CA_NORM = CA_TOT / POIDS_EFFECTIF_TOT) %>% 
  ggplot(aes(x = WEEK, y = CA_NORM, colour = ANNEE)) +
  geom_line() +
  scale_y_continuous(labels = format_ca) +
  labs(x = "Numéro de semaine",
       y = "CA normalisé") +
  #facet_wrap(~ OFFRE_PRINCIPALE) +
  ggtitle("Evolution du Pipe par l'effectif à date") + 
  theme_bw()
#
# seq(from = dmy("01/01/2015"), to=dmy("31/12/2015"), by = "week")

choix_annee <- function(annee_value) {
  if (annee_value == 2015) {
    my_df <- pilotage_2015
    ca_value <- 12
    ca_bt <- 9
  } else {
    my_df <- pilotage_2016
    ca_value <- 14
    ca_bt <- 11
  }
  
  my_export_list <- list()
  class(my_export_list) <- "mes_super_exports"
  my_export_list["my_df"] <- my_df
  my_export_list["ca_value"] <- ca_value
  my_export_list["ca_bt"] <- ca_bt
  my_export_list
}

select_annee <- function(annee_value) {
  pilotage_tot[pilotage_tot[["YEAR"]] == 2015,]
}


colnames(Effectifs2016) <- format_col_names(colnames(Effectifs2016))
save(Effectifs2016,file="Effectifs2016.RData")
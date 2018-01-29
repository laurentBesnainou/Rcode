# Evolution du staffing
#on va comaperer entre 2 semaines l'évolution avec daff
library(daff)
library(tidyr)
library(DT)
library(readr)
library(dplyr)



#on charge la semaine actuelle et la semaine précedente (ici S11 et S10)

load("data/staffing2017.RData")
load("data/StaffingS10.RData")

#on va regarder par personne le nb de jours par mois 
#Evolution du staffing Ferme entre 2 semaines
#on remplace tous les NA par des 0
#Semaine 10
S11 <- Staffing %>% 
  replace_na(list(JANV=0,FEV=0,MAR=0,AVR=0,MAI=0,JUIN= 0,JUIL= 0,AOUT= 0,SEPT= 0,OCT= 0,NOV= 0,DEC= 0)) %>%
  #filter(TYPE ==1 | TYPE ==2) %>%
  group_by(CONSULTANTS) %>% summarize(TotalVendu=sum(JANV+FEV+MAR+AVR+MAI+JUIN+JUIL+AOUT+SEPT+OCT+NOV+DEC))

#semaine 11
S10 <-  StaffingS10 %>% 
  replace_na(list(JANV=0,FEV=0,MAR=0,AVR=0,MAI=0,JUIN= 0,JUIL= 0,AOUT= 0,SEPT= 0,OCT= 0,NOV= 0,DEC= 0)) %>%
  #filter(TYPE ==1 | TYPE ==2) %>%
  group_by(CONSULTANTS) %>% summarize(TotalVendu=sum(JANV+FEV+MAR+AVR+MAI+JUIN+JUIL+AOUT+SEPT+OCT+NOV+DEC))

#On compare S10 et S11 pour voir les évolutions
ecart <- inner_join(S10, S11, by="CONSULTANTS")
colnames(ecart) <- c("CONSULTANTS","FERME_S1","FERME_S2")
# ecartPositifs <- ecart %>% mutate (DELTA =FERME_S2-FERME_S1 ) %>%
#   filter(DELTA >0)

ecart <- ecart %>% mutate (DELTA =FERME_S2-FERME_S1 ) %>%
  filter(DELTA !=0)

#on regarde les ecarts entre S11 et S10 sur les personnes en ecarts négatifs

S11 <- Staffing %>% 
  #filter(CONSULTANTS %in% ecartNegatifs$CONSULTANTS,TYPE ==1|TYPE==2 ) %>% 
  select(CONSULTANTS,TYPE,MISSIONS,ID_TOTEM,TOTAL)
S10 <- StaffingS10 %>% 
  #filter(CONSULTANTS %in% ecartNegatifs$CONSULTANTS, TYPE ==1|TYPE==2)%>% 
  select(CONSULTANTS,TYPE,MISSIONS,ID_TOTEM,TOTAL)


#il faut regarder les lignes différentes 

  resultatDiff <- diff_data(S10,S11)
  #on recupere dans un dataframe le resultat
  ENegatfis <- as.data.frame(resultatDiff$get_matrix())
  ENegatfis <- ENegatfis %>% 
    mutate(V1= as.character(V1)) %>% 
    mutate(V7= as.character(V6)) %>%
    filter(V1 %in% c("+++","---","->")) %>%
    mutate(V1= ifelse(V1=="->","mod",V1)) %>% 
    separate(V7, into = c("SPrev","S"), sep = "->") %>%
    
    replace_na(list(S=0)) %>%
    mutate(S= ifelse(V1=="+++",as.numeric(SPrev), as.numeric(S))) %>% 
    mutate(SPrev= ifelse(V1=="+++",0,as.numeric(SPrev))) %>%
    
    mutate(DELTA=S-SPrev) %>% filter(DELTA!=0)
  
  colnames(ENegatfis)  <-  c('Type Modif', 'Consultant', 'Type', 'Mission', 'TOTEM', 'Total',"Semaine prev", 'Semaine','DELTA')
  datatable(ENegatfis) %>% 
    formatStyle(
      'Type Modif',
      target = 'row',
      backgroundColor = styleEqual(c("+++", "---","mod","..."), c('#99E8D8', '#FEB7B2','#E3F2FD','red'))
    )
  
#on va regarder les différences entre deux semaines du pipe
load("data/pilotage_data.RData")
pilotage_2017 <- pilotage_data
S10 <- pilotage_2017 %>% filter(WEEK==10) %>% select (COMPTE,ASSOCIE, SUJET,STEP,PROB,CODE_TOTEM,TOTAL_CA_VENDU_N__KE)
S11 <- pilotage_2017 %>% filter(WEEK==11) %>% select (COMPTE,ASSOCIE, SUJET,STEP,PROB,CODE_TOTEM,TOTAL_CA_VENDU_N__KE)
S11 <- S11 %>%  group_by(STEP) %>% summarise(TOTAL = sum(TOTAL_CA_VENDU_N__KE,na.rm = FALSE))
S10 <- S10 %>%  group_by(STEP) %>% summarise(TOTAL = sum(TOTAL_CA_VENDU_N__KE,na.rm = FALSE))
global <- inner_join(S10, S11, by="STEP")
global <-global %>% mutate(Ecart= TOTAL.y -TOTAL.x)

resultatDiff <- diff_data(S10,S11)

#on recupere dans un dataframe le resultat
Ecarts <- as.data.frame(resultatDiff$get_matrix())
Ecarts <- Ecarts %>% 
  mutate(V1= as.character(V1)) %>% 
  filter(V1 %in% c("+++","---","->")) %>%
  mutate(V1= ifelse(V1=="->","mod",V1)) %>%
  mutate(V9= ifelse(grepl("->",V5),as.character(V5),paste(V5,"->",V5))) %>%
  
  separate(V9, into = c("Step","StepNew"), sep = "->") 


Ecarts <- Ecarts %>% 
  mutate(V1= as.character(V1)) %>% 
  filter(V1 %in% c("+++","---","->")) %>%
  mutate(V1= ifelse(V1=="->","mod",V1)) %>%
mutate(V8= ifelse(grepl("->",V5),as.character(V5),paste(V5,"->",V5))) %>%
  separate(V8, into = c("Step","StepNew"), sep = "->")
colnames(Ecarts)  <-  c('Type Modif',"COMPTE","ASSOCIE", "SUJET","STEP","PROB","CODE_TOTEM","CA VENDU KE")
datatable(Ecarts, rownames=FALSE, 
          options = list(autoWidth = TRUE,
                         columnDefs = list(list(width = '400px', targets = 3),list(width = '300px', targets = 4)))) %>% 
  formatStyle(
    'Type Modif',
    target = 'row',
    backgroundColor = styleEqual(c("+++", "---","mod","..."), c('#99E8D8', '#FEB7B2','#E3F2FD','red'))
  )
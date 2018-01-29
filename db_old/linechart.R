library(dplyr)
library(lubridate)
library(timevis)
library(googleVis)

setwd("D:/Data/R sources/data_pilotage")
load("staffing.RData")
  
  #On va constituer des Données par mission (Code TOTEM)
  # Données : id, content, start, end, group, type
  # id : numero de la ligne
  # content : Mission > Nom
  # Start : Date de début
  # End : Date de fin
  # group : Code TOTEM de la mission gagnée
  # type : "range"
  Debut <- c("2016-01-01", "2016-02-01","2016-03-01",
             "2016-04-01", "2016-05-01","2016-06-01",
             "2016-07-01", "2016-08-01","2016-09-01",
             "2016-10-01", "2016-11-01","2016-12-01",
             "2017-01-01", "2017-02-01","2017-03-01")
  Fin <-  c("2016-01-31", "2016-02-29","2016-03-31",
            "2016-04-30", "2016-05-31","2016-06-30",
            "2016-07-31", "2016-08-31","2016-09-30",
            "2016-10-31", "2016-11-30","2016-12-31",
            "2017-01-31", "2017-02-28","2017-03-31")
  Staffing <- Staffing %>% rename(AOUT = AOÛT )
  staff <- Staffing %>% rowwise() %>% mutate(TOT = 
                                           sum(JANV, FEV, MAR, AVR, MAI, JUIN, JUIL, AOUT, SEPT, OCT, NOV, DEC,
                                               JANV_N_PLUS_1_, FEV_N_PLUS_1_, MAR_N_PLUS_1_, na.rm=TRUE))

  dataGroups <- staff %>% filter (!is.na(ID_TOTEM)) %>%
    filter (TYPE %in% c(1,2)) %>% 
    mutate (start = ifelse(! is.na(JANV), Debut[1],
                           ifelse(! is.na(FEV), Debut[2],
                                  ifelse(! is.na(MAR), Debut[3],
                                         ifelse(! is.na(AVR), Debut[4],
                                                ifelse(! is.na(MAI), Debut[5],
                                                       ifelse(! is.na(JUIN), Debut[6],
                                                              ifelse(! is.na(JUIL), Debut[7],
                                                                     ifelse(! is.na(AOUT), Debut[8],
                                                                            ifelse(! is.na(SEPT), Debut[9],
                                                                                   ifelse(! is.na(OCT), Debut[10],
                                                                                          ifelse(! is.na(NOV), Debut[11],
                                                                                                 ifelse(! is.na(DEC), Debut[12],
                                                                                                        ifelse(! is.na(JANV_N_PLUS_1_), Debut[13],
                                                                                                               ifelse(! is.na(FEV_N_PLUS_1_), Debut[14],
                                                                                                                      ifelse(! is.na(MAR_N_PLUS_1_), Debut[15],"")))))))))))))))) %>%
    mutate (end = ifelse(! is.na(JANV_N_PLUS_1_), Fin[15],
                         ifelse(! is.na(FEV_N_PLUS_1_), Fin[14],
                                ifelse(! is.na(MAR_N_PLUS_1_), Fin[13],
              ifelse(! is.na(DEC), Fin[12],
                           ifelse(! is.na(NOV), Fin[11],
                                  ifelse(! is.na(OCT), Fin[10],
                                         ifelse(! is.na(SEPT), Fin[9],
                                                ifelse(! is.na(AOUT), Fin[8],
                                                       ifelse(! is.na(JUIL), Fin[7],
                                                              ifelse(! is.na(JUIN), Fin[6],
                                                                     ifelse(! is.na(MAI), Fin[5],
                                                                            ifelse(! is.na(AVR), Fin[4],
                                                                                   ifelse(! is.na(MAR), Fin[3],
                                                                                          ifelse(! is.na(FEV), Fin[2],
                                                                                                 ifelse(! is.na(JANV), Fin[1],"")))))))))))))))) %>%
  mutate(content = MISSIONS) %>%
  mutate(group = CONSULTANTS) %>%
  mutate(subgroup = ID_TOTEM) %>%
  mutate(type = "range")  %>%
  mutate(style =  ifelse(TYPE==1, "color: black; background-color: #39A0ED;","color: black; background-color: #7AC74F;")) %>%
  mutate(title = paste (CONSULTANTS,MISSIONS,TOT,"jh", sep = " - ")) %>%
    filter(start !="") %>% arrange(group) %>% mutate(id = row_number()) %>%
    select (id,content, start, end, group, subgroup, type, title,style) 
  groups <- dataGroups %>% distinct(group) %>% mutate (content = group)
  colnames(groups) <- c("id","content")
  timevis(data = dataGroups,  group = groups, options = list(showCurrentTime = FALSE, orientation = "top"))

  ################### TOTEM  ################################"
  setwd("D:/Data/R sources/data_pilotage")
  load("TOTEM.RData")
  
  TOTEM %>% filter(AFFAIRE=="O") %>%group_by(ANNEE,RESSOURCE) %>% 
    summarise(NBJ=sum(AFLTPSJREA),
              CA = sum(TotalAFLBUDPUHT_VENTE)) %>%
    filter(RESSOURCE %in% c("BEYLLE","BESNAINOU"))
  colnames(TOTEM) <- format_col_names(colnames(TOTEM))
  
  colnames(TOTEM) <-c("AFAFFACODE", "AFFAIRE", "AFAFFADESIG", "AFLBUDAFPRES_CODE", "AFLBUDQBUD", 
  "AFLBUDPUHT_VENTE", "FETPSRESSOURCE","AFETPSMOIS_ANNEE", "AFLTPSJREA",
  "TotalAFLBUDPUHT_VENTE","AFLTPSNOTES", "RESSOURCE", "GRADE1", 
  "GRADE", "ANNEE", "MOIS")
  
  colnames(TOTEM$Année) <- c("ANNEE")
  
  
  
  ################### Evolution Pipe vs Win et Lots
  setwd("D:/Data/R sources/data_pilotage")
  load("pilotage_data.RData")
  #on va mettre le pipe par semaine (pas en pondéré)
Lost2016 <- pilotage_data %>% filter (STEP %in% c("5 - No follow", "6 - En sommeil",  "7 - Perdue")) %>%
  group_by(WEEK) %>%
  summarize (MontantLost = sum(CA_BT__N__KE,na.rm=TRUE))
  
Pipe2016 <- pilotage_data %>% filter (STEP %in% c("0 - A qualifier", "1 - Qualifiée", "2 - A émettre", "3 - Emise" )) %>%
  group_by(WEEK) %>%
  summarize (MontantPipe = sum(CA_BT__N__KE,na.rm=TRUE))
Win2016 <- pilotage_data %>% filter (STEP %in% c("4 - Gagnée")) %>%
  group_by(WEEK) %>%
  summarize (MontantWin = sum(CA_BT__N__KE,na.rm=TRUE))

#Proposition 
SEM <- c(2:49)
LOST <- diff(Lost2016$MontantLost)
PIPE <- diff(Pipe2016$MontantPipe)
WIN <- diff(Win2016$MontantWin)
LOSTWIN <- LOST+WIN
ECARTPIPE <- PIPE - LOSTWIN

Evolution <- data.frame(SEM,PIPE, WIN, LOST)
Difference <- data.frame(SEM,ECARTPIPE)
#on transforme pour avoir le nom des colonnes en valeurs
Evolution <- Evolution %>% gather(key = "TYPE", value = "Ecart", PIPE, WIN, LOST)
Evolution %>%
  ggplot(aes(x=SEM,y=Ecart)) +
  geom_bar()


Evolution %>% 
  ggplot(aes(x = SEM,
             y = Ecart , fill = TYPE)
  ) +
  geom_bar(stat = "identity", position="stack")
  
Difference$colour <- ifelse(Difference$ECARTPIPE < 0, "negative","positive")

Difference %>% ggplot(aes(x = SEM, y = ECARTPIPE, fill = TYPE)) +

geom_bar(stat="identity",position="identity",aes(fill = colour))+
  scale_fill_manual(values=c(positive="firebrick1",negative="steelblue"))

#On va regarder les propal à 20% en semaine 2
Propal20 <- pilotage_data %>% filter (PROB < 1, WEEK==33,STEP %in% c("0 - A qualifier", "1 - Qualifiée", "2 - A émettre", "3 - Emise" )) %>% 
  mutate (link= paste(COMPTE,SUJET,sep=" > " )) %>%
  select (link, PROB,CA_BT__N__KE)

colnames(Propal20) <- c("link","ProbaS2","CAinit")

PropalFin <- pilotage_data %>% 
  mutate (link= paste(COMPTE,SUJET,sep= " > ")) %>%
  filter (link %in% Propal20$link, WEEK==46) %>% 
  select (link, PROB, CA_BT__N__KE)
demo <- left_join(PropalFin,Propal20, by="link")
demo <- demo %>% replace_na(list(CAinit = 0, CA_BT__N__KE = 0))

datSK <- demo %>% group_by(ProbaS2,PROB) %>% summarise(Weight = sum(CAinit)) 

datSK <- datSK %>%arrange (ProbaS2) %>% rowwise() %>%
  mutate (From=paste(toString(ProbaS2*100),".%")) %>%
  mutate (To=paste(toString(PROB*100),"%")) %>% select (From,To,Weight)

Sankey <- gvisSankey(datSK, from="From", to="To", weight="Weight",
                     options=list(height=400,width=900,
   
                       sankey="{
                        link: {color: { fill: '#d799ae' },
                                  colorMode: 'gradient',
colors: ['#a6cee3', '#b2df8a', '#fb9a99', '#fdbf6f',
                  '#cab2d6', '#ffff99', '#1f78b4', '#33a02c' ]
},
                        node: { colors: ['#a6cee3', '#b2df8a', '#fb9a99', '#fdbf6f',
                  '#cab2d6', '#ffff99', '#1f78b4', '#33a02c' ],
                          label: { color: '#871b47' } 
                              }
                       }"))
plot(Sankey)
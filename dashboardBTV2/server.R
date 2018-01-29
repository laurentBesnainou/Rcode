################################## V2 du dashboard BT ##########################################"
Objectif_BT <-14515 #données pour l'année en cours
Objectif_CA <- 16000
nbMois <- month(today())
#palette de couleurs
group.colors <- c("Data" = "#60D394", "Digital Innovation" = "#AAF683","ETM" ="#FFD97D", "Transformation" = "#FF9B85", "PocLab" = "#ED7D3A")
group.secteurs <- c("Banque" = "#D8DBE2","Assurance" = "#5bc0eb","Energie" = "#fde74c","Industrie & services" = "#9bc53d","Para-public" = "#e55934"," Retail" = "#fa7921")
############################################

#on regarde le nombre de jours ouvrés par mois
nbjours2016 <- c(20,21,22,21,20,22,20,22,22,21,20,22)
nbjours2017 <- c(22,20,23,20,21,22,20,22,21,22,20,20)

#Cette fonction permet de basculer sur l'année désirée
ChoixAnnee <- function(Annee){
  if (Annee==2015) {
    #effectifs <<- Effectifs2015
    pilotage_data <<- pilotage_2015 
    Objectif_BT <<- 11120
    Objectif_CA <<- 13000}
  if (Annee==2016) {
    #effectifs <<- Effectifs2016
    pilotage_data <<- pilotage_2016
    Objectif_BT <<-12500
    Objectif_CA <<- 14500
    nbjours <<- nbjours2016}
  if (Annee==2017) {
    #effectifs <<- Effectifs2017
    pilotage_data <<- pilotage_2017
    Objectif_BT <<-13000
    Objectif_CA <<- 14500
    nbjours <<- nbjours2017}
}

server <- function(input, output, session) {
  
  output$mychart <- function() ({
    dVide1 <-data.frame(WEEK=c(1:52),mnt=rep(0,52))
    dVide2 <-data.frame(WEEK=c(1:52),mnt=rep(0,52))
    dVide3 <-data.frame(WEEK=c(1:52),mnt=rep(0,52))
    
    pil2017 <- pilotage_2017 %>% filter(OFFRE_PRINCIPALE %in% input$uiChk1) %>%
      filter (STEP == "4 - Gagnée") %>%
      group_by (WEEK) %>% summarise(mnt=sum(CA_BT_PONDERE__N__KE, na.rm = TRUE))
    #on constitue un tableau avec les valeurs de 1 à 52 en fonction de ce que l'on a dans pil2017
    for(i in dVide1$WEEK) if(i %in% pil2017$WEEK) dVide1$mnt[i] <- pil2017$mnt[i]
    
    pil2016 <- pilotage_2016 %>% filter(OFFRE_PRINCIPALE %in% input$uiChk1) %>%
      filter (STEP == "4 - Gagnée") %>%
        group_by (WEEK) %>% summarise(mnt=sum(CA_BT_PONDERE__N__KE, na.rm = TRUE))
    #on constitue un tableau avec les valeurs de 1 à 52 en fonction de ce que l'on a dans pil2017
    for(i in dVide2$WEEK) if(i %in% pil2016$WEEK) dVide2$mnt[i] <- pil2016$mnt[i]
      
    pil2015 <- pilotage_2015 %>% filter(OFFRE_PRINCIPALE %in% input$uiChk1) %>%
    filter (STEP == "4 - Gagnée") %>%
      group_by (WEEK) %>% summarise(mnt=sum(CA_BT_PONDERE__N__KE, na.rm = TRUE))
    #on constitue un tableau avec les valeurs de 1 à 52 en fonction de ce que l'on a dans pil2017
    for(i in dVide3$WEEK) if(i %in% pil2015$WEEK) dVide3$mnt[i] <- pil2015$mnt[i]
    
    data.frame(
      "BT 2017" = dVide1$mnt,
      "BT 2016" = dVide2$mnt,
      "BT 2015" = dVide3$mnt)
    
  })

################# Liste des Infobox  
  caBT <- reactive({
    ChoixAnnee(input$Annee)
    
    pilotage_data %>% filter(STEP == "4 - Gagnée", WEEK == input$semaine,
                             OFFRE_PRINCIPALE %in% input$uiChk1) %>%
      summarise(CA_BT__N__KE = sum(CA_BT__N__KE, na.rm = TRUE))
  })  
  
  caSemaineTotal  <- function (semaine) {
    ChoixAnnee(input$Annee)
    pilotage_data %>% filter(STEP == "4 - Gagnée", WEEK == semaine,
                             OFFRE_PRINCIPALE %in% input$uiChk1) %>%
      summarise(TOTAL_CA_VENDU_N__KE = sum(TOTAL_CA_VENDU_N__KE, na.rm = TRUE))
  }
  
output$ca_box <- renderInfoBox({
  n <-  sprintf("%.1f %%",caBT()[1]/Objectif_BT*100) 
  infoBox("CA BT", value = str_c(round(caBT()[1], digits = 0),"k€",sep = " "),
          
          subtitle = str_c(str_c(n," :"),"Semaine",input$semaine,sep = " "),
          icon = icon("eur"),
          color = "aqua")
})

output$ca_production <- renderInfoBox({
  n <- sprintf("%.1f %%",caSemaineTotal(input$semaine)/Objectif_CA*100)
  infoBox("Vente", value = str_c(round(caSemaineTotal(input$semaine), digits = 0),"k€",sep = " "),
          subtitle = str_c(str_c("Semaine",input$semaine,sep = " "),n,sep =" > "),
          icon = icon("bar-chart"),
          color = "aqua")
})

output$nbMission <- renderInfoBox({
  ChoixAnnee(input$Annee)
  nb <-  pilotage_data %>% filter(STEP == "4 - Gagnée", WEEK == input$semaine,OFFRE_PRINCIPALE %in% input$uiChk1) %>%
    summarize(count = n())
  infoBox("Nb Missions gagnées", value = nb,
          subtitle = str_c("Semaine",input$semaine,sep = " "),
          icon = icon("info-circle"),
          color = "aqua")
})

caPipe <- reactive ({
  ChoixAnnee(input$Annee)
  pilotage_data %>%
    filter(WEEK == input$semaine,
           PROB >= input$uiS1[1]/100, PROB <= input$uiS1[2]/100,
           OFFRE_PRINCIPALE %in% input$uiChk1,
           STEP %in% input$uiChk2) %>%
    summarise(TOTAL_CA_VENDU_N__KE = sum(TOTAL_CA_VENDU_N__KE, na.rm = TRUE))
}) 

output$pipe_ca <- renderInfoBox({
  ChoixAnnee(input$Annee)
  infoBox("Pipe CA Total", value = str_c(round(caPipe()[1], digits = 0),"k€",sep = " "),
          subtitle = str_c("Semaine",input$semaine,sep = " "),
          icon = icon("eur"),
          color= "aqua")
})

caPipeBT <- reactive ({
  pilotage_data %>%
    filter(WEEK == input$semaine,
           PROB >= input$uiS1[1]/100,PROB <= input$uiS1[2]/100,
           OFFRE_PRINCIPALE %in% input$uiChk1,
           STEP %in% input$uiChk2) %>%
    summarise(CA_BT__N__KE = sum(CA_BT_PONDERE__N__KE, na.rm = TRUE))
}) 

output$pipe_caBT <- renderInfoBox({
  infoBox("Pipe BT pondéré", width=2,value = str_c(round(caPipeBT()[1], digits = 0),"k€",sep = " "),
          subtitle = str_c("Semaine",input$semaine,sep = " "),
          icon = icon("eur"),
          color= "aqua")
})

nbPipeBT <- reactive ({
  ChoixAnnee(input$Annee)
  pilotage_data %>%
    filter(WEEK == input$semaine,
           PROB >= input$uiS1[1]/100,PROB <= input$uiS1[2]/100,
           OFFRE_PRINCIPALE %in% input$uiChk1,
           STEP %in% input$uiChk2) %>%
    summarise(n = n())
})

output$pipe_nb <- renderInfoBox({
  infoBox("Nb d'opportunités", value = round(nbPipeBT()[1], digits = 0),
          subtitle = str_c("Semaine",input$semaine,sep = " "),
          icon = icon("info-circle"),
          color= "aqua")
})

output$pipe1_plot <- renderPlot({
  pilotage_data %>%
    filter(
      WEEK == input$semaine,
      PROB > input$uiS1[1]/100,
      PROB <= input$uiS1[2]/100,
      OFFRE_PRINCIPALE %in% input$uiChk1,
      STEP %in% input$uiChk2) %>% select (COMPTE,TOTAL_CA_VENDU_N__KE,OFFRE_PRINCIPALE) %>%
    group_by(COMPTE,OFFRE_PRINCIPALE) %>%
    summarise(CA_TOT = sum(TOTAL_CA_VENDU_N__KE, na.rm = TRUE)) %>%
    ggplot(aes(x = reorder(COMPTE,CA_TOT),
               y = CA_TOT, fill= OFFRE_PRINCIPALE )) +
    geom_bar(stat = "identity") + coord_flip() +
    scale_fill_manual(name = "OFFRE",values = group.colors)+
    scale_x_discrete(name="Clients") +
    scale_y_continuous(name="CA TOTAL") +
    theme(legend.direction = "horizontal", legend.position="bottom", legend.box = "horizontal")
})

output$ca_plot <- renderPlot({
  ChoixAnnee(input$Annee)
  pilotage_data %>%
    filter(STEP == "4 - Gagnée", WEEK == input$semaine,
           OFFRE_PRINCIPALE %in% input$uiChk1) %>%
    group_by(GROUPE) %>%
    summarise(CA_GAGNE = sum(TOTAL_CA_VENDU_N__KE, na.rm = TRUE)) %>%
    filter (CA_GAGNE >= input$uiS5[1],CA_GAGNE <= input$uiS5[2]) %>%
    ggplot(aes(x = reorder(GROUPE, CA_GAGNE), 
               y = CA_GAGNE)) +
    geom_bar(stat = "identity", fill="#00A8E8") +
    scale_fill_brewer(palette = "YlOrRd") + # il y a aussi un scale_color_brewer
    labs(x = "Groupe",
         y = "Chiffre d'affaires Total gagné (k€)",
         fill = "") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 75, hjust = 1), legend.position="bottom")
})

output$compare_plot <- renderPlot({
  p2015 <- pilotage_2015  %>% mutate(YEAR=2015) %>%
    rename(CA_BT = CA_BT__N__KE) %>%select(STEP,
                                           CA_BT,
                                           OFFRE_PRINCIPALE,
                                           WEEK,
                                           YEAR)
  p2016 <- pilotage_2016  %>% mutate(YEAR=2016) %>%
    rename(CA_BT = CA_BT__N__KE) %>%select(STEP,
                                           CA_BT,
                                           OFFRE_PRINCIPALE,
                                           WEEK,
                                           YEAR)
  
  p2017 <- pilotage_2017  %>% mutate(YEAR=2017) %>%
    rename(CA_BT = CA_BT__N__KE) %>%select(STEP,
                                           CA_BT,
                                           OFFRE_PRINCIPALE,
                                           WEEK,YEAR)
  pilotage_tot <- bind_rows(p2015, p2016, p2017)
  
  format_ca <- function(x) {
    str_c(format(x, big.mark = " "), " K€")
  }
  
  max_week_2017 <- maxWeek
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
    ggtitle("Comparaison des CA BT 2015 à 2017")+
    theme_bw()
})

output$compare2_plot <- renderPlot({
  p2015 <- pilotage_2015  %>% mutate(YEAR=2015) %>%
    rename(CA_BT = CA_BT__N__KE) %>%select(STEP,
                                           CA_BT,
                                           OFFRE_PRINCIPALE,
                                           WEEK,
                                           YEAR)
  p2016 <- pilotage_2016  %>% mutate(YEAR=2016) %>%
    rename(CA_BT = CA_BT__N__KE) %>%select(STEP,
                                           CA_BT,
                                           OFFRE_PRINCIPALE,
                                           WEEK,
                                           YEAR)
  
  p2017 <- pilotage_2017  %>% mutate(YEAR=2017) %>%
    rename(CA_BT = CA_BT__N__KE) %>%select(STEP,
                                           CA_BT,
                                           OFFRE_PRINCIPALE,
                                           WEEK,YEAR)
  pilotage_tot <- bind_rows(p2015, p2016, p2017)
  
  format_ca <- function(x) {
    str_c(format(x, big.mark = " "), " K€")
  }
  
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
})

CATableau <- reactive ({
  #Dans un premier temps on regarde quels sont les Groupes qui ont un CA compris entre 2 valeurs
  DD <- pilotage_data%>% filter(STEP == "4 - Gagnée",
                                WEEK == input$semaine,
                                OFFRE_PRINCIPALE %in% input$uiChk1) %>% group_by(GROUPE) %>%
    summarise(CA_GAGNE = sum(TOTAL_CA_VENDU_N__KE, na.rm = TRUE)) %>%
    filter (CA_GAGNE >input$uiS5[1],CA_GAGNE < input$uiS5[2]) %>% select(GROUPE)
  
  pilotage_data%>% filter(STEP == "4 - Gagnée") %>% filter (GROUPE %in% DD$GROUPE) %>%
    select (GROUPE, COMPTE, SUJET, OFFRE_PRINCIPALE, CA_BT__N__KE, TOTAL_CA_VENDU_N__KE)
  
}) 

output$DTCABT = DT::renderDataTable(rownames = TRUE, 
                                    CATableau()
)


StaffTableau <- reactive ({
  #on va regarder par personne le nb de jours par mois 
  #Evolution du staffing Ferme entre 2 semaines
  #on remplace tous les NA par des 0
  #Semaine 10
  S11 <- Staffing %>% 
    replace_na(list(JANV=0,FEV=0,MAR=0,AVR=0,MAI=0,JUIN= 0,JUIL= 0,AOUT= 0,SEPT= 0,OCT= 0,NOV= 0,DEC= 0)) %>%
    #filter(TYPE %in% input$Staff)  %>%
    group_by(CONSULTANTS) %>% summarize(TotalVendu=sum(JANV+FEV+MAR+AVR+MAI+JUIN+JUIL+AOUT+SEPT+OCT+NOV+DEC))
  
  #semaine 11
  S10 <-  StaffingS10 %>% 
    replace_na(list(JANV=0,FEV=0,MAR=0,AVR=0,MAI=0,JUIN= 0,JUIL= 0,AOUT= 0,SEPT= 0,OCT= 0,NOV= 0,DEC= 0)) %>%
    #filter(TYPE %in% input$Staff) %>%
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
    #filter(TYPE %in% input$Staff)  %>% 
    select(CONSULTANTS,TYPE,MISSIONS,ID_TOTEM,TOTAL)
  S10 <- StaffingS10 %>% 
    #filter(TYPE %in% input$Staff) %>% 
    select(CONSULTANTS,TYPE,MISSIONS,ID_TOTEM,TOTAL)
  
  
  #il faut regarder les lignes différentes 
  
  resultatDiff <- diff_data(S10,S11)
  #on recupere dans un dataframe le resultat
  ENegatfis <- as.data.frame(resultatDiff$get_matrix())
  #on remplace le texte "NA" par 0
  ENegatfis[ENegatfis$V6=="NA",6] <- 0
  ENegatfis <- ENegatfis %>% 
    mutate(V1= as.character(V1))%>% 
    replace_na(list(V6=0))%>%
    mutate(V7= ifelse(grepl("->",V6),as.character(V6),paste(V6,"->0"))) %>%
    filter(V1 %in% c("+++","---","->")) %>% #,V3 %in% input$Staff) %>%
    mutate(V1= ifelse(V1=="->","mod",V1)) %>% 
    
    separate(V7, into = c("SPrev","S"), sep = "->",convert=TRUE) %>%
    replace_na(list(S=0,SPrev=0)) %>%
    # mutate(S= as.numeric(S)) %>% 
    mutate(S=ifelse(V1=="+++",SPrev,S))%>%
    mutate(SPrev= ifelse(V1=="+++",0,SPrev))%>%
    
    mutate(DELTA=S-SPrev) %>% filter(DELTA!=0)
  
  colnames(ENegatfis)  <-  c('Changement', 'Consultant', 'Type', 'Mission', 'TOTEM', 'Total',"Semaine prev", 'Semaine','DELTA')
  ENegatfis <-ENegatfis %>% 
    mutate(Mission=ifelse(grepl("->",Mission),paste("<font color=#BC0E1F>",Mission,"</font>"),paste(Mission,""))) %>%
    mutate(TOTEM=ifelse(grepl("->",TOTEM),paste("<font color=#BC0E1F>",TOTEM,"</font>"),paste(TOTEM,""))) %>%
    mutate(Total=ifelse(grepl("->",Total),paste("<font color=#BC0E1F>",Total,"</font>"),paste(Total,""))) 
  
  datatable(ENegatfis, 
            escape = FALSE, #permet de mettre du HTML
    rownames = FALSE,
    options = list(
      columnDefs = list(list(className = 'dt-center', targets = 5)),
      pageLength = 50))  %>%
    formatStyle(
      'Changement',
      target = 'row',
      backgroundColor = styleEqual(c("+++", "---","mod"), c('#99E8D8', '#FEB7B2','#E3F2FD'))
    )
}) 

output$DTSTAFF = DT::renderDataTable( StaffTableau()
 
)

PipeTableau <- reactive ({
  S10 <- pilotage_2017 %>% filter(WEEK==max(pilotage_data$WEEK)-1) %>% 
  select (COMPTE,ASSOCIE, SUJET,STEP,PROB,CODE_TOTEM,TOTAL_CA_VENDU_N__KE)
  S11 <- pilotage_2017 %>% filter(WEEK==max(pilotage_data$WEEK)) %>% select (COMPTE,ASSOCIE, SUJET,STEP,PROB,CODE_TOTEM,TOTAL_CA_VENDU_N__KE)
  
  resultatDiff <- diff_data(S10,S11)
  
  my_fun <- function(x){
    res_X = 
      do(my_fun(x=.$x))
    return(data.frame(res_x, res_y))
  }
  

  
  #on recupere dans un dataframe le resultat
  Ecarts <- as.data.frame(resultatDiff$get_matrix())
  Ecarts <- Ecarts %>% 
    mutate(V1= as.character(V1)) %>% 
    filter(V1 %in% c("+++","---","->")) %>%
    mutate(V1= ifelse(V1=="->","mod",V1)) %>%
    mutate(V9= ifelse(grepl("->",V5),as.character(V5),paste(V5,"->",V5))) %>%
    separate(V9, into = c("Step","StepNew"), sep = "->") %>%
    mutate(StepNew=trimws(StepNew))
  
  Ecarts <- Ecarts %>% filter(V1 %in% input$uiModifEcart, StepNew %in% input$uiStepEcart)
  colnames(Ecarts)  <-  c('Modif',"COMPTE","ASSOCIE", "SUJET","STEP","PROB","CODE_TOTEM","CA_VENDU_KE","Step","StepNew")
  Ecarts <- Ecarts %>%
    mutate(COMPTE=ifelse(grepl("->",COMPTE),paste("<font color=#BC0E1F>",COMPTE,"</font>"),paste(COMPTE,""))) %>%
    mutate(ASSOCIE=ifelse(grepl("->",ASSOCIE),paste("<font color=#BC0E1F>",ASSOCIE,"</font>"),paste(ASSOCIE,""))) %>%
    mutate(SUJET=ifelse(grepl("->",SUJET),paste("<font color=#BC0E1F>",SUJET,"</font>"),paste(SUJET,""))) %>%
    mutate(STEP=ifelse(grepl("->",STEP),paste("<font color=#BC0E1F>",STEP,"</font>"),paste(STEP,""))) %>%
    mutate(PROB=ifelse(grepl("->",PROB),paste("<font color=#BC0E1F>",PROB,"</font>"),paste(PROB,""))) %>%
    mutate(CODE_TOTEM=ifelse(grepl("->",CODE_TOTEM),paste("<font color=#BC0E1F>",CODE_TOTEM,"</font>"),paste(CODE_TOTEM,""))) %>%
    mutate(CA_VENDU_KE=ifelse(grepl("->",CA_VENDU_KE),paste("<font color=#BC0E1F>",CA_VENDU_KE,"</font>"),paste(CA_VENDU_KE,"")))
   
    

    
  datatable(Ecarts, escape = FALSE, rownames=FALSE, 
            options = list(autoWidth = TRUE,
                           columnDefs = list(list(width = '400px', targets = 3),list(width = '300px', targets = 4)))) %>% 
    formatStyle(
      'Modif',
      target = 'row',
      backgroundColor = styleEqual(c("+++", "---","mod","..."), c('#99E8D8', '#FEB7B2','#E3F2FD','red'))
    )
  
})

output$DTPIPEEcarts = DT::renderDataTable( PipeTableau()
                                      
)
output$chartCABT <- renderGvis({
  ChoixAnnee(input$Annee)

  tmp <- pilotage_2016 %>% select (SUJET,  PROB, OFFRE_PRINCIPALE, 
                                   GROUPE, TOTAL_CA_VENDU_N__KE, CA_BT_PONDERE__N__KE, 
                                   SECTEUR, LEAD__RESP, WEEK, STEP,DATE_REF)
  tmp2 <- pilotage_2015 %>% select (SUJET,  PROB, OFFRE_PRINCIPALE, 
                                    GROUPE, TOTAL_CA_VENDU_N__KE, CA_BT_PONDERE__N__KE, SECTEUR, 
                                    LEAD__RESP, WEEK, STEP,DATE_REF)
  tmp3 <- pilotage_2017 %>% select (SUJET,  PROB, OFFRE_PRINCIPALE, 
                                   GROUPE, TOTAL_CA_VENDU_N__KE, CA_BT_PONDERE__N__KE, 
                                   SECTEUR, LEAD__RESP, WEEK, STEP,DATE_REF)
  
  Pilotage <- rbind(tmp, tmp2,tmp3)
  
  evol_pipe <- Pilotage %>% 
    filter(STEP == "4 - Gagnée",
           !is.na(TOTAL_CA_VENDU_N__KE)) %>% 
    select (SUJET,  PROB, OFFRE_PRINCIPALE, GROUPE, TOTAL_CA_VENDU_N__KE, CA_BT_PONDERE__N__KE, SECTEUR, LEAD__RESP, WEEK, 
            STEP,DATE_REF)
  # on regarde le montant ponderee dans le temps par semaine
  data_ts <- evol_pipe %>% filter(!is.na(GROUPE)) %>% group_by (GROUPE,DATE_REF ) %>% 
    summarise (CA_TOTAL=sum(TOTAL_CA_VENDU_N__KE , na.rm = TRUE)) %>%
    select(GROUPE,CA_TOTAL,DATE_REF) 
  df <- as_data_frame(data_ts)
  df <- df %>% mutate(GROUPE=factor(GROUPE))
  myState <- '
  {"xAxisOption":"_TIME",
  "playDuration":15000,
  "showTrails":false,
  "orderedByX":false,"xLambda":1,
  "colorOption":"_UNIQUE_COLOR"}'
  ex <- gvisMotionChart(df, idvar="GROUPE", timevar = "DATE_REF", 
                  xvar = "CA_TOTAL", yvar = "CA_TOTAL",
                  options=list(title="Evolution du pipe", gvis.editor="Edit me!",width=850,
                               state=myState))  
})

pipeTableau <- reactive ({
  
  cpt <- pilotage_data %>%
    filter(
      WEEK == input$semaine,
      PROB > input$uiS1[1]/100,
      PROB <= input$uiS1[2]/100,
      OFFRE_PRINCIPALE %in% input$uiChk1,
      STEP %in% input$uiChk2) %>% select (COMPTE,TOTAL_CA_VENDU_N__KE,OFFRE_PRINCIPALE) %>%
    group_by(COMPTE,OFFRE_PRINCIPALE) %>%
    summarise(CA_TOT = sum(TOTAL_CA_VENDU_N__KE, na.rm = TRUE)) %>%
    select (COMPTE)
  
  d <- pilotage_data %>%
    filter(COMPTE %in% cpt$COMPTE,
      WEEK == input$semaine,
      PROB >= input$uiS1[1]/100,
      PROB <= input$uiS1[2]/100,
      OFFRE_PRINCIPALE %in% input$uiChk1,
      STEP %in% input$uiChk2) %>%
    
    select(COMPTE, PROB, SUJET, OFFRE_PRINCIPALE, STEP, CA_BT__N__KE, CA_BT_PONDERE__N__KE, TOTAL_CA_VENDU_N__KE)
  colnames(d) <- c("COMPTE", "PROB", "SUJET", "OFFRE", "STEP", "CA BT", "CA BT PONDERE", "TOTAL")
  d
}) 
output$DTtable = DT::renderDataTable(rownames = FALSE, 
  pipeTableau()
)

output$chartPipe3 <- renderGvis({
  weekChoix <- ifelse(input$uiS6[2]>=maxWeek,maxWeek,input$uiS6[2])
  ChoixAnnee(input$Annee)
  PropalDebut <- pilotage_data %>% 
            filter (PROB < 1, WEEK== input$uiS6[1],
                    OFFRE_PRINCIPALE %in% input$uiChk1,
                    STEP %in% c("0 - A qualifier", "1 - Qualifiée", "2 - A émettre", "3 - Emise" )
          ) %>% 
    mutate (link= paste(COMPTE,SUJET,sep=" > " )) %>%
    select (link, PROB,CA_BT__N__KE)
  colnames(PropalDebut) <- c("link","ProbaS2","CAinit")
  
  PropalFin <- pilotage_data %>% 
    mutate (link= paste(COMPTE,SUJET,sep= " > ")) %>%
    filter (link %in% PropalDebut$link, WEEK== weekChoix) %>% 
    select (link, PROB, CA_BT__N__KE)
  demo <- left_join(PropalFin,PropalDebut, by="link")
  demo <- demo %>% replace_na(list(CAinit = 0, CA_BT__N__KE = 0))
  
  datSK <- demo %>% group_by(ProbaS2,PROB) %>% summarise(CA = sum(CA_BT__N__KE)) 
  
  datSK <- datSK %>%arrange (ProbaS2) %>% rowwise() %>%
    mutate (From=paste(toString(ProbaS2*100),".%")) %>%
    mutate (To=paste(toString(PROB*100),"%")) %>% arrange(From) %>% select (From,To,CA) 
  
  Sankey <- gvisSankey(datSK, from="From", to="To", weight="CA",
                       options=list(height=400,width=900,
                                    
                                    sankey="{
                                    link: {color: { fill: '#d799ae' },
                                    colorMode: 'gradient',
                                    colors: ['#a6cee3', '#b2df8a', '#fb9a99', '#fdbf6f',
                                    '#cab2d6', '#ffff99', '#1f78b4', '#33a02c' ]
                                    },
                                    node: { colors: ['#a6cee3', '#b2df8a', '#fb9a99', '#fdbf6f',
                                    '#cab2d6', '#ffff99', '#1f78b4', '#33a02c' ],
                                    label: { color: '#871b47' },
                                    nodePadding: 20
                                    }
}"))

  })

# output$chartPipe3bis <- renderGvis({
#   weekChoix <- ifelse(input$uiS6[2]>=maxWeek,maxWeek,input$uiS6[2])
#   ChoixAnnee(input$Annee)
#   PropalDebut <- pilotage_data %>% 
#     filter (PROB < 1, WEEK== input$uiS6[1],
#             OFFRE_PRINCIPALE %in% input$uiChk1,
#             STEP %in% c("0 - A qualifier", "1 - Qualifiée", "2 - A émettre", "3 - Emise" )
#     ) %>% group_by(PROB) %>%  summarise(CA = sum(CA_BT__N__KE)) %>%
#     select (PROB,CA_BT__N__KE)
#   colnames(PropalDebut) <- c("ProbaS2","CAinit")
#   
#   PropalFin <- pilotage_data %>% filter (link %in% PropalDebut$link, WEEK== weekChoix)
#   %>% group_by(PROB) %>%  summarise(CA = sum(CA_BT__N__KE)) %>%
#   select ( PROB, CAFin)
#   demo <- left_join(PropalFin,PropalDebut, by="PROB")
#   demo <- demo %>% replace_na(list(CAinit = 0, CAFin = 0))
#   
#   datSK <- demo %>% group_by(ProbaS2,PROB) %>% summarise(CA = sum(CA_BT__N__KE)) 
#   
#   datSK <- datSK %>%arrange (ProbaS2) %>% rowwise() %>%
#     mutate (From=paste(toString(ProbaS2*100),".%")) %>%
#     mutate (To=paste(toString(PROB*100),"%")) %>% arrange(From) %>% select (From,To,CA) 
#   
#   Sankey <- gvisSankey(datSK, from="From", to="To", weight="CA",
#                        options=list(height=400,width=900,
#                                     
#                                     sankey="{
#                                     link: {color: { fill: '#d799ae' },
#                                     colorMode: 'gradient',
#                                     colors: ['#a6cee3', '#b2df8a', '#fb9a99', '#fdbf6f',
#                                     '#cab2d6', '#ffff99', '#1f78b4', '#33a02c' ]
#                                     },
#                                     node: { colors: ['#a6cee3', '#b2df8a', '#fb9a99', '#fdbf6f',
#                                     '#cab2d6', '#ffff99', '#1f78b4', '#33a02c' ],
#                                     label: { color: '#871b47' },
#                                     nodePadding: 20
#                                     }
# }"))
# 
#   })

output$lost1 <- renderInfoBox({
  ChoixAnnee(input$Annee)
  nb <- pilotage_data %>%  filter(STEP %in% input$uiChk3,
                                  OFFRE_PRINCIPALE %in% input$uiChk1,
                                  WEEK == input$semaine,
                                  TOTAL_CA_VENDU_N__KE >input$uiS2[1],
                                  TOTAL_CA_VENDU_N__KE <= input$uiS2[2]
  ) %>%
    summarize(count = n())
  
  infoBox("nb mission", value = nb,
          
          icon = icon("info"),
          color = "orange")
})

output$lost2 <- renderInfoBox({
  ChoixAnnee(input$Annee)
  nb <- pilotage_data %>%  filter(STEP %in% input$uiChk3, 
                                  WEEK == input$semaine,
                                  OFFRE_PRINCIPALE %in% input$uiChk1,
                                  TOTAL_CA_VENDU_N__KE >input$uiS2[1],
                                  TOTAL_CA_VENDU_N__KE <= input$uiS2[2]
  ) %>%
    summarize(sum(TOTAL_CA_VENDU_N__KE, na.rm = TRUE))
  
  infoBox("Montants perdus", value = nb,
          
          icon = icon("eur"),
          color = "orange")
}) 


output$lost_plot <- renderPlot({
  ChoixAnnee(input$Annee)
  pilotage_data %>%
    filter(STEP %in% input$uiChk3,
           WEEK == input$semaine,
           OFFRE_PRINCIPALE %in% input$uiChk1,
           TOTAL_CA_VENDU_N__KE >input$uiS2[1],
           TOTAL_CA_VENDU_N__KE <= input$uiS2[2]) %>%
    select (COMPTE,TOTAL_CA_VENDU_N__KE,OFFRE_PRINCIPALE) %>%
    group_by(COMPTE,OFFRE_PRINCIPALE) %>%
    summarise(CA_TOT = sum(TOTAL_CA_VENDU_N__KE, na.rm = TRUE)) %>%
    ggplot(aes(x = reorder(COMPTE,CA_TOT),
               y = CA_TOT, fill= OFFRE_PRINCIPALE )) +
    scale_fill_manual(name = "OFFRE_PRINCIPALE",
                      values = group.colors)+
    geom_bar(stat = "identity")+theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.position="bottom",legend.title = element_blank())
})

lostTableau <- reactive ({
  ChoixAnnee(input$Annee)
  pilotage_data %>%
    filter(STEP %in% input$uiChk3,
           WEEK == input$semaine,
           TOTAL_CA_VENDU_N__KE > input$uiS2[1],
           TOTAL_CA_VENDU_N__KE <= input$uiS2[2],
           OFFRE_PRINCIPALE %in% input$uiChk1) %>%
    select(COMPTE, SUJET, OFFRE_PRINCIPALE, STEP, CA_BT__N__KE, TOTAL_CA_VENDU_N__KE)
}) 
output$DTlost = DT::renderDataTable(rownames = FALSE,
                                    lostTableau()
)


output$saff1_box <- renderInfoBox({
  listMois <-  my_cols[input$uiS3[1]:input$uiS3[2]]
  nb <- staffFerme %>% filter( GRADE %in% input$uiChk4 ) %>% 
    select ( one_of(listMois)) %>%
    summarise_each(funs(sum)) %>%
    rowSums()
  
  infoBox("Jours fermes", value = nb,
          subtitle = "sur la periode séléctionnée",
          icon = icon("male"),
          color = "aqua")
}) 
output$saff2_box <- renderInfoBox({
  listMois <-  my_cols[input$uiS3[1]:input$uiS3[2]]
  nb <- staffProvisiore %>% filter( GRADE %in% input$uiChk4 ) %>% 
    select ( one_of(listMois)) %>%
    summarise_each(funs(sum)) %>%
    rowSums()
  
  infoBox("Jours Prévi.", value = nb,
          subtitle = "sur la periode séléctionnée",
          icon = icon("search-plus"),
          color = "aqua")
})   

output$saff3_box <- renderInfoBox({
  listMois <-  my_cols[input$uiS3[1]:input$uiS3[2]]
  nb <- staffconges %>% filter( GRADE %in% input$uiChk4 ) %>% 
    select ( one_of(listMois)) %>%
    summarise_each(funs(sum)) %>%
    rowSums()
  
  infoBox("Jours de congés", value = nb,
          subtitle = "nb jours sur la periode",
          icon = icon("plane"),
          color = "aqua")
})   

observeEvent(input$btn1, {
  updateCheckboxGroupInput(session=session, inputId="uiChk4", selected=c("2-C", "3-CC", "4-CS"))
})

observeEvent(input$btn2, {
  updateCheckboxGroupInput(session=session, inputId="uiChk4", selected=c("5-MNG", "6-SM", "7-DIR", "8-ASS"))
})
observeEvent(input$btn3, {
  updateCheckboxGroupInput(session=session, inputId="uiChk4", selected=c("A1-AE", "A2-AES"))
})

observeEvent(input$btn4, {
  updateCheckboxGroupInput(session=session, inputId="uiChk4", selected=c("D1-DS1", "D3-DS3"))
})



output$staff4_plot <- renderPlot({
 
  #cumul des jh en incluant les congÃ¨s
  Staffing %>% 
    filter(TYPE %in% c(1,3), GRADE %in% input$uiChk4) %>% 
    group_by(CONSULTANTS,GRADE)  %>%
    summarise(Janvier= sum(JANV, na.rm = TRUE),
              Fevrier= sum(FEV, na.rm = TRUE),
              Mars= sum(MAR, na.rm = TRUE),
              Avril= sum(AVR, na.rm = TRUE),
              Mai= sum(MAI, na.rm = TRUE),
              Juin= sum(JUIN, na.rm = TRUE),
              Juillet= sum(JUIL, na.rm = TRUE),
              Aout= sum(AOUT, na.rm = TRUE),
              Septembre= sum(SEPT, na.rm = TRUE),
              Octobre= sum(OCT, na.rm = TRUE),
              Novembre= sum(NOV, na.rm = TRUE),
              Decembre= sum(DEC, na.rm = TRUE)) -> staffTemp 
  
  #on regroupe les données
  staffTemp %>% 
    gather(key = "month_staff", value = "staff_value", Janvier, Fevrier, Mars, Avril, 
           Mai, Juin, Juillet, Aout, Septembre, Octobre, Novembre, Decembre) -> staffCumul 
  
  #on calcule le taux de staffing en fonction des jours ouvrés par mois
  staffTemp %>% 
    mutate (Jan_Taux = Janvier/nbjours[1],
            Fev_Taux = Fevrier/nbjours[2],
            Mar_Taux = Mars/nbjours[3],
            Avr_Taux = Avril/nbjours[4],
            Mai_Taux = Mai/nbjours[5],
            Juin_Taux = Juin/nbjours[6],
            Juil_Taux = Juillet/nbjours[7],
            Aou_Taux = Aout/nbjours[8],
            Sep_Taux = Septembre/nbjours[9],
            Oct_Taux = Octobre/nbjours[10],
            Nov_Taux = Novembre/nbjours[11],
            Dec_Taux = Decembre/nbjours[12]) %>%
    select (CONSULTANTS,GRADE,Jan_Taux, Fev_Taux, Mar_Taux, Avr_Taux, 
            Mai_Taux, Juin_Taux, Juil_Taux,Aou_Taux, Sep_Taux, Oct_Taux, Nov_Taux, Dec_Taux)-> staffTemp2
  
  staffTemp2 %>%
    gather(key = "month_staff", value = "staff_value", Jan_Taux, Fev_Taux, Mar_Taux, Avr_Taux, 
           Mai_Taux, Juin_Taux, Juil_Taux,Aou_Taux, Sep_Taux, Oct_Taux, Nov_Taux, Dec_Taux) -> staffTaux 
  mois <- c("Jan_Taux", "Fev_Taux", "Mar_Taux", "Avr_Taux", 
            "Mai_Taux", "Juin_Taux", "Juil_Taux","Aou_Taux", "Sep_Taux", "Oct_Taux", "Nov_Taux", "Dec_Taux")
  nomMois <- mois[nbMois]
  
  #on recupere les mois a prendre en compte ainsi que les grades
  st <-  staffTaux
  valeurmois <- seq(1, 12, by = 1)
  moisTaux <- data.frame(mois,valeurmois)
  
  st <- left_join(st, moisTaux, by=c("month_staff" = "mois"))
  mois <- mois[input$uiS3[1]:input$uiS3[2]]
  st <- st %>% filter(month_staff %in% mois) 
  
  #on ne garde que les personnes présente à date dans le graphe
  #PErsonnes qui doivent encore partir d'ici la fin de l'année
  Nom1 <- effectifs %>% filter (!is.na(DATESORTIE)) %>% 
    filter (DATESORTIE>  today()) %>% 
    mutate(NOM=toupper(NOM)) %>%
    select(NOM)
  #Pésents à date 
  Nom2 <- effectifs %>% 
    filter (is.na(DATESORTIE)) %>% 
    mutate(NOM=toupper(NOM)) %>%
    select(NOM)
  
  #Personnes ayant changé de grade dans l'année
  NOM <- c("FAIBIS2","GIRARD2")
  Nom3 <- data.frame(NOM)
  Noms <- full_join(Nom1, Nom2)
  Noms <- full_join(Noms, Nom3)             
  st <- st %>% filter(toupper(CONSULTANTS) %in% Noms$NOM)  
  filtreNom <- st %>% filter(valeurmois == nbMois, staff_value <=  input$uiS4)
  
  
  st$valeurmois <- factor(st$valeurmois)
  
  #on plot le resultat
  st %>%  filter(CONSULTANTS %in% filtreNom$CONSULTANTS) %>%
    ggplot( aes(valeurmois, CONSULTANTS, fill = staff_value)) + 
    geom_tile(colour="white")+
    scale_fill_gradient(low = "white", high = "#00A8E8") +
    ggtitle("Taux de staffing ") + 
    geom_text( aes(valeurmois, CONSULTANTS, label = percent(staff_value)),
               color="black", 
               size=rel(4))+
    geom_vline(xintercept=nbMois, color = "red", alpha = 0.2, size=12) +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          legend.direction = "horizontal", legend.position="bottom", legend.box = "horizontal") 
})


output$timelineGroups <- renderTimevis({
  Debut <- c("2017-01-01", "2017-02-01","2017-03-01",
             "2017-04-01", "2017-05-01","2017-06-01",
             "2017-07-01", "2017-08-01","2017-09-01",
             "2017-10-01", "2017-11-01","2017-12-01",
             "2018-01-01", "2018-02-01","2018-03-01")
  Fin <-  c("2017-01-31", "2017-02-28","2017-03-31",
            "2017-04-30", "2017-05-31","2017-06-30",
            "2017-07-31", "2017-08-31","2017-09-30",
            "2017-10-31", "2017-11-30","2017-12-31",
            "2017-01-31", "2018-02-28","2018-03-31")
  
  Staffing <- mutate(Staffing, id = row_number())
  staff <- Staffing %>% 
    arrange(CONSULTANTS,MISSIONS) %>% rowwise() %>% 
            mutate(TOT = sum(JANV, FEV, MAR, AVR, MAI, JUIN, JUIL, AOUT, SEPT, OCT, NOV, DEC,
                                                                 JANV_N_PLUS_1_, FEV_N_PLUS_1_, MAR_N_PLUS_1_, na.rm=TRUE))
  
  dataGroups <- staff %>% filter (GRADE %in% input$uiChk4) %>%
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
    mutate(type = "range")  %>%
    mutate(style =  ifelse(TYPE==1, "color: black; background-color: #39A0ED;","color: black; background-color: #7AC74F;")) %>%
    mutate(title = paste (CONSULTANTS,MISSIONS,TOT,"jh", sep = " - ")) %>%
    filter(start !="", group %in% input$selUI1) %>% 
    select (id, content, start, end, group, type, title,style) 
  groups <- dataGroups %>% distinct(group) %>% mutate (content = group)
  colnames(groups) <- c("id","content")
  timevis(data = dataGroups,  group = groups, options = list(selectable=TRUE, showCurrentTime = FALSE, orientation = "top"))
  
})


output$timelineGroupsTOTEM <- renderTimevis({
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
  
  Staffing <- mutate(Staffing, id = row_number())
  staff <- Staffing %>% 
    arrange(CONSULTANTS,MISSIONS) %>% rowwise() %>% mutate(TOT = 
                                                             sum(JANV, FEV, MAR, AVR, MAI, JUIN, JUIL, AOUT, SEPT, OCT, NOV, DEC,
                                                                 JANV_N_PLUS_1_, FEV_N_PLUS_1_, MAR_N_PLUS_1_, na.rm=TRUE))
  
  dataGroups <- staff %>% filter (GRADE %in% input$uiChk4) %>%
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
    mutate(content = CONSULTANTS) %>%
    mutate(group = ID_TOTEM) %>%
    mutate(type = "range")  %>%
    mutate(style =  ifelse(TYPE==1, "color: black; background-color: #39A0ED;","color: black; background-color: #7AC74F;")) %>%
    mutate(title = paste (CONSULTANTS,MISSIONS,TOT,"jh", sep = " - ")) %>%
    filter(start !="", group %in% input$selUI2) %>% 
    select (id, content, start, end, group, type, title,style) 
  groups <- dataGroups %>% distinct(group) %>% mutate (content = group)
  colnames(groups) <- c("id","content")
  timevis(data = dataGroups,  group = groups, options = list(selectable=TRUE, showCurrentTime = FALSE, orientation = "top"))
  
})


output$dygraph1 <- renderDygraph({
  ChoixAnnee(input$Annee)
  selected_pilotage <- pilotage_data %>% filter(STEP == "4 - Gagnée",
                                                OFFRE_PRINCIPALE %in% input$uiChk1) %>%
    select(WEEK, CA_BT__N__KE, TOTAL_CA_VENDU_N__KE, DATE_REF) %>%
    group_by(DATE_REF) %>%
    summarise(CA_TOT = sum(CA_BT__N__KE, na.rm = TRUE), CA_PROD = sum(TOTAL_CA_VENDU_N__KE, na.rm = TRUE)) %>%
    select(DATE_REF, CA_TOT, CA_PROD)
  
  #on construit les 2 séries temporelle pour les afficher dans un graphe dynamique
  ca_ts <- xts(selected_pilotage$CA_TOT,order.by = selected_pilotage$DATE_REF)
  prod_ts <- xts(selected_pilotage$CA_PROD,order.by = selected_pilotage$DATE_REF)
  colnames(ca_ts) <- "CA BT"
  colnames(prod_ts) <- "Production"
  
  #on fusionne les 2 series temporelles
  data_ts <- cbind(ca_ts, prod_ts)
  
  dygraph(data_ts) %>%
    dyRangeSelector(height = 20) %>%
    dyHighlight(highlightCircleSize = 2,
                highlightSeriesBackgroundAlpha = 0.2,
                hideOnMouseOut = FALSE) %>%
    dyOptions( fillGraph = TRUE, fillAlpha = 0.7,colors =c("#B2DBBF","#00A8E8")) %>%
    dyLegend(labelsDiv = "legendDivID", labelsSeparateLines = TRUE) %>%
    dyLimit(12500, label ="Objectif vente",
            color = "black", strokePattern = "dashed") %>%
    dyLimit(12500, label ="Objectif Prod",
            color = "red", strokePattern = "dotdash")
})
 #################### RADAR semaine

output$radar_CA <- renderPlot({

pilotage_2016 <- pilotage_2016 %>% mutate(OFFRE = ifelse(OFFRE_PRINCIPALE == "Data" | OFFRE_PRINCIPALE =="Digital Innovation"|
                                                             OFFRE_PRINCIPALE == "ETM" | OFFRE_PRINCIPALE =="Transformation", OFFRE_PRINCIPALE,"Autre" ))
pilotage_2015 <- pilotage_2015 %>% mutate(OFFRE = ifelse(OFFRE_PRINCIPALE == "Data" | OFFRE_PRINCIPALE =="Digital Innovation"|
                                                           OFFRE_PRINCIPALE == "ETM" | OFFRE_PRINCIPALE =="Transformation",OFFRE_PRINCIPALE,"Autre" ))
pilotage_2017 <- pilotage_2017 %>% mutate(OFFRE = ifelse(OFFRE_PRINCIPALE == "Data" | OFFRE_PRINCIPALE =="Digital Innovation"|
                                                           OFFRE_PRINCIPALE == "ETM" | OFFRE_PRINCIPALE =="Transformation",OFFRE_PRINCIPALE,"Autre" ))

  data_2016 <- pilotage_2016 %>% filter(STEP == "4 - Gagnée", WEEK==input$semaine, OFFRE !="Autre" ) %>% group_by(OFFRE) %>%
    summarise(CABT = sum(CA_BT__N__KE, na.rm = TRUE)) %>% 
    mutate(OFFRE = factor(OFFRE)) 
  data_2015 <- pilotage_2015 %>% filter(STEP == "4 - Gagnée", WEEK==input$semaine , OFFRE !="Autre") %>% group_by(OFFRE) %>%
    summarise(CABT = sum(CA_BT__N__KE, na.rm = TRUE)) %>% 
    mutate(OFFRE = factor(OFFRE)) 
  p2107_2016 <- pilotage_2017 %>% filter(STEP == "4 - Gagnée", WEEK==input$semaine, OFFRE !="Autre") %>% group_by(OFFRE) %>%
    summarise(CABT = sum(CA_BT__N__KE, na.rm = TRUE)) %>% 
    mutate(OFFRE = factor(OFFRE)) %>%
    ggplot(aes(x = OFFRE, y = CABT)) +
    geom_col(alpha = 0.5, 
             width = 1, show.legend = FALSE, color = "white", aes(fill = factor(OFFRE))) +
    geom_text(aes(label = format(CABT, digits=0)),
              position = position_stack(vjust = .5)) +
    geom_col(aes(x = OFFRE, y = CABT), data = data_2016, alpha = 0.2,
             width = 1, show.legend = FALSE, color = "blue") + 
    geom_text(aes(label = format(data_2016$CABT, digits=0)),
                  position = position_stack(vjust = 1),color = "blue")+
    geom_col(aes(x = OFFRE, y = CABT), data = data_2015, alpha = 0.2,
             width = 1, show.legend = FALSE, color = "red") +
    
    
    coord_polar() +
    scale_fill_manual(values = group.colors) +
     
    labs(x = "", y = "",
         title = "CA BT Par Offre Comparaison avec 2016 (bleu) et 2015 (rouge)") + 
    theme(
      panel.background = element_rect(fill = "#FFFFFF"),
      plot.background = element_rect(fill = "#FFFFFF"),
      strip.background = element_rect(fill = "#FFFFFF"),
      strip.text = element_text(size = 15),
      panel.grid = element_blank(),
      axis.ticks = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_text(size = 8),
      panel.spacing = grid::unit(2, "lines"))
    
  p2107_2015 <- pilotage_2017 %>% filter(STEP == "4 - Gagnée", WEEK==input$semaine) %>% group_by(OFFRE) %>%
    summarise(CABT = sum(CA_BT__N__KE, na.rm = TRUE)) %>% 
    mutate(OFFRE = factor(OFFRE)) %>%
    ggplot(aes(x = OFFRE, y = CABT)) +
    geom_col(alpha = 0.5, 
             width = 1, show.legend = FALSE, color = "white", aes(fill = factor(OFFRE))) +
    geom_col(aes(x = OFFRE, y = CABT), data = data_2015, alpha = 0.2,
             width = 1, show.legend = FALSE, color = "black") +
    coord_polar() +
    scale_fill_manual(values = group.colors) +
    
    labs(x = "", y = "",
         title = "CA BT Par Offre Comparaison avec 2015") + 
    theme(
      panel.background = element_rect(fill = "#FFFFFF"),
      plot.background = element_rect(fill = "#FFFFFF"),
      strip.background = element_rect(fill = "#FFFFFF"),
      strip.text = element_text(size = 15),
      panel.grid = element_blank(),
      axis.ticks = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_text(size = 8),
      panel.spacing = grid::unit(2, "lines")) 
  
   grid.arrange(p2107_2016, p2107_2015,ncol=2, nrow=1)
})



output$plotDiag <- renderPlot ({
  pilotage_2017 <- pilotage_data
  tmp <- pilotage_2017 %>% filter( WEEK ==input$semaine, 
                                   PROB >= input$uiS1[1]/100, PROB <= input$uiS1[2]/100,
                                   STEP %in% input$uiChk2) %>% select(GROUPE,OFFRE_PRINCIPALE,CA_BT__N__KE)
  
  
  matriceConnais <- xtabs(CA_BT__N__KE~ GROUPE + OFFRE_PRINCIPALE, na.omit(tmp))
  
  to <- paste(unique(colnames(matriceConnais)),sep = ",")
  from <- paste(rownames(matriceConnais),sep = ",")
  mat <- matrix(0, nrow = length(unique(from)), ncol = length(unique(to)))
  col <- matrix(0, nrow = length(unique(from)), ncol = length(unique(to)))
  
  rownames(mat) = unique(from)
  colnames(mat) = unique(to)
  noms <- c(from,to)
  group.colors
  # names(gripCol) <- noms
  
  for (i in 1:length(from)) {
    
    for (j in 1:length(to)) { 
      mat[i,j] <- matriceConnais[i,j]
      col[,j] <- group.colors[i]
      
    }
  }

  #= = = = = initialize = = = = = #
  par(mar = c(1, 1, 1, 1))
  circos.par(gap.degree = c(rep(2, nrow(mat)-1), 10, rep(2, ncol(mat)-1), 10))
  gripCol <- c(rep("#E69F00",length(unique(from))),rep("#56B4E9",length(unique(to))))
  # = = = = = plot 'circlize' = = = = = #
  c <- chordDiagram(mat, annotationTrack = "grid", transparency = 0.8,
               preAllocateTracks = list(track.height = 0.1),
               col = matrix(rainbow(nrow(mat)),nrow=nrow(mat),ncol=ncol(mat)),
               
               grid.col=gripCol)
  
  # = = = = = add labels = = = = = #
  circos.trackPlotRegion(track.index = 1,
                         panel.fun = function(x, y) {
                           xrange = get.cell.meta.data("xlim")
                           labels = get.cell.meta.data("sector.index")
                           circos.text(mean(xrange), 0,
                                       labels = labels, niceFacing = TRUE)
                         },
                         bg.border = NA)
  circos.clear()
  print(c)
})
########### sunburst
combo_output <- function(){
  # if (input$radio == "Par Offre") {
  #   seq_Asso <- pilotage_data %>% filter (STEP == "4 - Gagnée",WEEK==maxWeek) %>%
  #   group_by(OFFRE_PRINCIPALE,ASSOCIE,SECTEUR,GROUPE) %>%
  #   summarize(CA = sum(CA_BT__N__KE,na.rm=TRUE)) %>% mutate (DEBUT = "CA BT")
  #   
  # } else {
  #   if (input$radio == "Par groupe") {
        seq_Asso <- pilotage_data %>% filter (STEP == "4 - Gagnée",WEEK==maxWeek) %>%
        group_by(GROUPE, OFFRE_PRINCIPALE,ASSOCIE,SECTEUR,SUJET) %>%
      summarize(CA = sum(CA_BT__N__KE,na.rm=TRUE)) %>% mutate (DEBUT = "CA BT")
    
  #  } else {
  #     seq_Asso <- pilotage_data %>% filter (STEP == "4 - Gagnée",WEEK==maxWeek) %>%
  #     group_by(GROUPE, ASSOCIE,OFFRE_PRINCIPALE,SECTEUR) %>%
  #     summarize(CA = sum(CA_BT__N__KE,na.rm=TRUE)) %>% mutate (DEBUT = "CA BT")
  #  
  # }
  # }
        input$action
        

  nom <- list()
  nb_1 <- length(seq_Asso$ASSOCIE)
  
  for (i in 1:nb_1) {
      nom[[i]] <- c(seq_Asso$DEBUT[i], seq_Asso$ASSOCIE[i],seq_Asso$OFFRE_PRINCIPALE[i] ,seq_Asso$SECTEUR[i],seq_Asso$GROUPE[i],seq_Asso$SUJET[i])
    # } else {
    #   nom[[i]] <- c(seq_Asso$DEBUT[i],seq_Asso$ASSOCIE[i],seq_Asso$OFFRE_PRINCIPALE[i] ,seq_Asso$SECTEUR[i],seq_Asso$GROUPE[i])
    # }
    
  }
  CA_TOTAL <- c(seq_Asso$CA)
  combo_output <- list(path = nom, value = CA_TOTAL)
}

# output$D3Part1 <- renderD3partitionR(
#   D3partitionR( data=list(path=combo_output()$path,value=combo_output()$value),type="sunburst", 
#                 tooltipOptions = list(showAbsolutePercent = T, showRelativePercent = T),
#                 legend=list(type="sequential",no_show = FALSE, 
#                             color=list("ORE"="#E589B4", "JSO"="#C9C5E5", "OGR" = "#50E5D6", 
#                                        "BES" = "#99F8FF", "MMO"= "#FF9770", "UHE"="#E9FF70", "Transformation"="#0BA","ETM"="#AA1","Data"="#ECC")), trail = TRUE,
#                 Input=list(enabled=T,Id="D3Part1",clickedStep=T,
#                            currentPath=T,visiblePaths=T,visibleLeaf=T,visibleNode=T),
#                 
#                 width = 600,height = 600)
# )

output$D3Part2 <- renderD3partitionR(
  
  D3partitionR( data=list(path=combo_output()$path,value=combo_output()$value),type="treeMap", 
                tooltipOptions = list(showAbsolutePercent = T, showRelativePercent = T),
                legend=list(type="sequential",no_show = FALSE, 
                            color=list("ORE"="#E589B4", "JSO"="#C9C5E5", "OGR" = "#50E5D6", 
                                       "BES" = "#99F8FF", "MMO"= "#FF9770", "UHE"="#E9FF70", "Transformation"="#0BA","ETM"="#AA1","Data"="#ECC")), trail = TRUE,
                Input=list(enabled=T,Id="D3Part1",clickedStep=T,
                           currentPath=T,visiblePaths=T,visibleLeaf=T,visibleNode=T),
                
                width = 1200,height = 600)
)


diff13 <- reactive({
  p2015 <- pilotage_2015  %>% mutate(YEAR=2015) %>%
    rename(CA_BT = CA_BT__N__KE) %>%select(STEP,
                                           CA_BT,
                                           OFFRE_PRINCIPALE,
                                           WEEK,
                                           YEAR)
  p2016 <- pilotage_2016  %>% mutate(YEAR=2016) %>%
    rename(CA_BT = CA_BT__N__KE) %>%select(STEP,
                                           CA_BT,
                                           OFFRE_PRINCIPALE,
                                           WEEK,
                                           YEAR)
  
  p2017 <- pilotage_2017  %>% mutate(YEAR=2017) %>%
    rename(CA_BT = CA_BT__N__KE) %>%select(STEP,
                                           CA_BT,
                                           OFFRE_PRINCIPALE,
                                           WEEK,YEAR)
  pilotage_tot <- bind_rows(p2015, p2016, p2017)
  
  
  
  max_week_2017 <- maxWeek
  
  pilotage_tot %>% 
    mutate(YEAR = factor(YEAR)) %>% 
    filter(STEP == "4 - Gagnée") %>% 
    group_by(YEAR, WEEK) %>% 
    summarise(CA_TOT = sum(CA_BT, na.rm = TRUE))
})

output$hcontainer <- renderHighchart({
  p2015 <- pilotage_2015  %>% mutate(YEAR=2015) %>%
    rename(CA_BT = CA_BT__N__KE) %>%select(STEP,
                                           CA_BT,
                                           OFFRE_PRINCIPALE,
                                           WEEK,
                                           YEAR)
  p2016 <- pilotage_2016  %>% mutate(YEAR=2016) %>%
    rename(CA_BT = CA_BT__N__KE) %>%select(STEP,
                                           CA_BT,
                                           OFFRE_PRINCIPALE,
                                           WEEK,
                                           YEAR)
  
  p2017 <- pilotage_2017  %>% mutate(YEAR=2017) %>%
    rename(CA_BT = CA_BT__N__KE) %>%select(STEP,
                                           CA_BT,
                                           OFFRE_PRINCIPALE,
                                           WEEK,YEAR)
  pilotage_tot <- bind_rows(p2015, p2016, p2017)
  # On va aouter une régression lineaire sur l'année 2017
  # Fit a linear model called on the linkedin views per day: linkedin_lm
  p2017_lm <- p2017  %>% 
    mutate(YEAR = factor(YEAR)) %>% 
    filter(STEP == "4 - Gagnée") %>% 
    group_by(YEAR, WEEK) %>% 
    summarise(CA_TOT = sum(CA_BT, na.rm = TRUE))
  # regression lineaire CA en fonction de la semaine
  p2017_lm <- lm(CA_TOT ~ WEEK,  p2017_lm)
  
  # on calcule les points en fonction des coefficients obtenus
  p2017_lm_Graphe <- data.frame(WEEK=c(1:52), CA_TOT = p2017_lm$coefficients[2] * c(1:52) +p2017_lm$coefficients[1])
  
  
  plot_chat <- pilotage_tot %>% 
    mutate(YEAR = factor(YEAR)) %>% 
    filter(STEP == "4 - Gagnée") %>% 
    group_by(YEAR, WEEK) %>% 
    summarise(CA_TOT = sum(CA_BT, na.rm = TRUE))
  
  hc <-  highchart() %>% 
    
    hc_add_series (p2017_lm_Graphe, name = "forecasts 2017",lineWidth = 1, type = "line", marker = list(enabled = FALSE),
                   color= 'orange', hcaes(x = WEEK, y = CA_TOT) ) %>% 
    hc_add_series (plot_chat, type = "line",marker = list(enabled = FALSE), #C ,marker = list(symbol = fa_icon_mark("eur")),
                   color= c('#E2FF0A', '#FF5151', '#57B8FF'), hcaes(x = WEEK, y = CA_TOT, group = YEAR) ) %>% 
    hc_subtitle(text = "Comparaison du CA") %>%
    hc_title(text = "Evolution du CA par semaine",
             style = list(fontWeight = "bold")) %>% 
    hc_tooltip(crosshairs = TRUE, valueDecimals = 0,
               pointFormat = "Semaine: {point.x} <br> CA e keuros: {point.y}") %>%
    hc_credits(enabled = TRUE, 
               text = "Sources: COBIZ 2015 - 2017",
               style = list(fontSize = "10px")) %>%
    hc_yAxis(title = list(text = "Chiffre d'affaire"),
             labels = list(format = "{value}")) %>% 
    hc_xAxis(title = list(text = "Numéro de semaine"),
             labels = list(format = "{value}")) %>% 
    hc_exporting(enabled = TRUE) # enable exporting option
  
  # # Determine theme and apply to highchart ------------------------
   
     theme <- switch(input$hc_theme,
                    chalk = hc_theme_chalk(),
                    darkunica = hc_theme_darkunica(),
                    fivethirtyeight = hc_theme_538(),
                    gridlight = hc_theme_gridlight(),
                    handdrawn = hc_theme_handdrawn(),
                    economist = hc_theme_economist(),
                    financialTimes =hc_theme_ft(),
                    Dotabuff=hc_theme_db(),
                    flat=hc_theme_flat(),
                    sandsignika = hc_theme_sandsignika()
     )
  hc <- hc %>%
    hc_add_theme(theme)
  
  hc
   })
  
 
  
  output$contents <- renderTable({
    inFile <- input$file1
    
    if(is.null(inFile))
      return(NULL)
    file.rename(inFile$datapath,
                paste(inFile$datapath, ".xlsx", sep=""))
    read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
  })
  output$contentss <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    pilotage_data_tmp <- read_excel(inFile$datapath, col_types = c(
      rep("text", 6),
      "numeric",
      rep("text", 2),
      rep("numeric", 6),
      "text",
      "date",
      rep("text", 3),
      rep("numeric", 2),
      rep("date", 5)))
    nb <- str_sub(fichier,22)
    fin <- str_locate(nb,".xlsx")[1]
    nb <-  as.numeric(str_sub(nb,1,fin-1))
    #on ajoute la semaine et aussi la date de début de la semaine
    date_ref <- data_frame(
      date_ref = seq(from = dmy("01/01/2017"), to = dmy("31/12/2017"), by = "weeks"),
      week = week(date_ref)
    
    )
    pilotage_data_tmp %>% mutate(week=nb) -> pilotage_data_tmp
    pilotage_data_tmp %>% 
      inner_join(date_ref, by = "week")
    
    pilotage_data_tmp
  })
  
  output$cityControls <- renderUI({
    
    checkboxGroupInput("cities", "Choose Cities", cities)
  })
  
  
  

}
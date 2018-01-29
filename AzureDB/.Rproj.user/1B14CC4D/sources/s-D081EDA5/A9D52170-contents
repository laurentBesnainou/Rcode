
# Define server logic
shinyServer(function(input, output, session) {
  
  chargement <- reactiveValues()
  chargement$HistoriqueCA <- HistoriqueCA
  output$TotalBox <- renderValueBox({
    
    data <- QueryCA %>% filter (etape %in% c("0 - A qualifier", "1 - Qualifiée", "2 - A émettre", "3 - Emise")) %>%
      replace_na(list(ca_sst_interne = 0, ca_sst_externe = 0, ca_entite=0,ca_markup=0 )) %>%
      mutate (totalCA = ca_entite+ca_sst_interne+ca_sst_externe+ca_markup) %>%
      mutate (totalCAPondere = totalCA * probabilite) %>%
      group_by(etape) %>% summarise(total=sum(totalCA))
    total <-   round(sum(data$total,na.rm =TRUE ),digits=0)
    valueBox(
      paste0(total, " k€"), "TOTAL PIPE", icon = icon("money"),
      color = "purple"
    )
  })
  
  
  output$TotalBoxS <- renderValueBox({
    semaine <- week(now())-1
    data <- HistoriqueCA %>% filter (week==semaine,etape %in% c("0 - A qualifier", "1 - Qualifiée", "2 - A émettre", "3 - Emise")) %>%
      replace_na(list(ca_sst_interne = 0, ca_sst_externe = 0, ca_entite=0,ca_markup=0 )) %>%
      mutate (totalCA = ca_entite+ca_sst_interne+ca_sst_externe+ca_markup) %>%
      mutate (totalCAPondere = totalCA * probabilite) %>%
      group_by(etape) %>% summarise(total=sum(totalCA))
    total <-   round(sum(data$total,na.rm =TRUE ),digits=0)
    valueBox(
      paste0(total, " k€"), "TOTAL PIPE", icon = icon("money"),
      color = "purple"
    )
  })
  
  output$TotalPondereBox <- renderValueBox({
    
    data <- QueryCA %>% filter (etape %in% c("0 - A qualifier", "1 - Qualifiée", "2 - A émettre", "3 - Emise")) %>%
      replace_na(list(ca_sst_interne = 0, ca_sst_externe = 0, ca_entite=0,ca_markup=0 )) %>%
      mutate (totalCA = ca_entite+ca_sst_interne+ca_sst_externe+ca_markup) %>%
      mutate (totalCAPondere = totalCA * probabilite) %>%
      group_by(etape) %>% summarise(total=sum(totalCAPondere))
    total <-   round(sum(data$total,na.rm =TRUE ),digits=0)
    valueBox(
      paste0(total, " k€"), "TOTAL Pondéré", icon = icon("money"),
      color = "yellow"
    )
  })
  
  
  output$TotalPondereBoxS <- renderValueBox({
    semaine <- week(now())-1
    data <- HistoriqueCA %>% filter (week==semaine,etape %in% c("0 - A qualifier", "1 - Qualifiée", "2 - A émettre", "3 - Emise")) %>%
      replace_na(list(ca_sst_interne = 0, ca_sst_externe = 0, ca_entite=0,ca_markup=0 )) %>%
      mutate (totalCA = ca_entite+ca_sst_interne+ca_sst_externe+ca_markup) %>%
      mutate (totalCAPondere = totalCA * probabilite) %>%
      group_by(etape) %>% summarise(total=sum(totalCAPondere))
    total <-   round(sum(data$total,na.rm =TRUE ),digits=0)
    valueBox(
      paste0(total, " k€"), "TOTAL Pondéré", icon = icon("money"),
      color = "yellow"
    )
  })
  
  output$Total0 <- renderValueBox({
    
    data <- QueryCA %>% filter (etape %in% c("0 - A qualifier")) %>%
      replace_na(list(ca_sst_interne = 0, ca_sst_externe = 0, ca_entite=0,ca_markup=0 )) %>%
      mutate (totalCA = ca_entite+ca_sst_interne+ca_sst_externe+ca_markup) %>%
      mutate (totalCAPondere = totalCA * probabilite) %>%
      group_by(etape) %>% summarise(total=sum(totalCA))
    total <-   round(sum(data$total,na.rm =TRUE ),digits=0)
    valueBox(
      paste0(total, " k€"), "TOTAL A qualifier", icon = icon("money"),
      color = "green"
    )
  })
  output$Total0S <- renderValueBox({
    semaine <- week(now())-1
    data <- HistoriqueCA %>% filter (week==semaine,etape %in% c("0 - A qualifier")) %>%
      replace_na(list(ca_sst_interne = 0, ca_sst_externe = 0, ca_entite=0,ca_markup=0 )) %>%
      mutate (totalCA = ca_entite+ca_sst_interne+ca_sst_externe+ca_markup) %>%
      mutate (totalCAPondere = totalCA * probabilite) %>%
      group_by(etape) %>% summarise(total=sum(totalCA))
    total <-   round(sum(data$total,na.rm =TRUE ),digits=0)
    valueBox(
      paste0(total, " k€"), "TOTAL A qualifier", icon = icon("money"),
      color = "green"
    )
  })
  
  output$Total1 <- renderValueBox({
    
    data <- QueryCA %>% filter (etape %in% c("1 - Qualifiée")) %>%
      replace_na(list(ca_sst_interne = 0, ca_sst_externe = 0, ca_entite=0,ca_markup=0 )) %>%
      mutate (totalCA = ca_entite+ca_sst_interne+ca_sst_externe+ca_markup) %>%
      mutate (totalCAPondere = totalCA * probabilite) %>%
      group_by(etape) %>% summarise(total=sum(totalCA))
    total <-   round(sum(data$total,na.rm =TRUE ),digits=0)
    valueBox(
      paste0(total, " k€"), "TOTAL QUALIFIEE", icon = icon("money"),
      color = "orange"
    )
  })
  
  output$Total1S <- renderValueBox({
    semaine <- week(now())-1
    data <- HistoriqueCA %>% filter (week==semaine,etape %in% c("1 - Qualifiée")) %>%
      replace_na(list(ca_sst_interne = 0, ca_sst_externe = 0, ca_entite=0,ca_markup=0 )) %>%
      mutate (totalCA = ca_entite+ca_sst_interne+ca_sst_externe+ca_markup) %>%
      mutate (totalCAPondere = totalCA * probabilite) %>%
      group_by(etape) %>% summarise(total=sum(totalCA))
    total <-   round(sum(data$total,na.rm =TRUE ),digits=0)
    valueBox(
      paste0(total, " k€"), "TOTAL QUALIFIEE", icon = icon("money"),
      color = "orange"
    )
  })
 
  output$Total2 <- renderValueBox({
    
    data <- QueryCA %>% filter (etape %in% c("2 - A émettre")) %>%
      replace_na(list(ca_sst_interne = 0, ca_sst_externe = 0, ca_entite=0,ca_markup=0 )) %>%
      mutate (totalCA = ca_entite+ca_sst_interne+ca_sst_externe+ca_markup) %>%
      mutate (totalCAPondere = totalCA * probabilite) %>%
      group_by(etape) %>% summarise(total=sum(totalCA))
    total <-   round(sum(data$total,na.rm =TRUE ),digits=0)
    valueBox(
      paste0(total, " k€"), "TOTAL A EMETTRE", icon = icon("money"),
      color = "blue"
    )
  }) 
  
  
  output$Total2S <- renderValueBox({
    semaine <- week(now())-1
    data <- HistoriqueCA %>% filter (week==semaine,etape %in% c("2 - A émettre")) %>%
      replace_na(list(ca_sst_interne = 0, ca_sst_externe = 0, ca_entite=0,ca_markup=0 )) %>%
      mutate (totalCA = ca_entite+ca_sst_interne+ca_sst_externe+ca_markup) %>%
      mutate (totalCAPondere = totalCA * probabilite) %>%
      group_by(etape) %>% summarise(total=sum(totalCA))
    total <-   round(sum(data$total,na.rm =TRUE ),digits=0)
    valueBox(
      paste0(total, " k€"), "TOTAL A EMETTRE", icon = icon("money"),
      color = "blue"
    )
  }) 
  output$Total3 <- renderValueBox({
    
    data <- QueryCA %>% filter (etape %in% c( "3 - Emise")) %>%
      replace_na(list(ca_sst_interne = 0, ca_sst_externe = 0, ca_entite=0,ca_markup=0 )) %>%
      mutate (totalCA = ca_entite+ca_sst_interne+ca_sst_externe+ca_markup) %>%
      mutate (totalCAPondere = totalCA * probabilite) %>%
      group_by(etape) %>% summarise(total=sum(totalCA))
    total <-   round(sum(data$total,na.rm =TRUE ),digits=0)
    valueBox(
      paste0(total, " k€"), "TOTAL EMISE", icon = icon("money"),
      color = "red"
    )
  })
  output$Total3S <- renderValueBox({
    semaine <- week(now())-1
    data <- HistoriqueCA %>% filter (week==semaine,etape %in% c( "3 - Emise")) %>%
      replace_na(list(ca_sst_interne = 0, ca_sst_externe = 0, ca_entite=0,ca_markup=0 )) %>%
      mutate (totalCA = ca_entite+ca_sst_interne+ca_sst_externe+ca_markup) %>%
      mutate (totalCAPondere = totalCA * probabilite) %>%
      group_by(etape) %>% summarise(total=sum(totalCA))
    total <-   round(sum(data$total,na.rm =TRUE ),digits=0)
    valueBox(
      paste0(total, " k€"), "TOTAL EMISE", icon = icon("money"),
      color = "red"
    )
  })
  dTableau <- function() {
    
    d <- QueryCA %>% mutate (url=paste0("<a target=_self href='https://pilotage.airmis.com/admin/bt/pipebt/",
                                        id,
                                        "/change/#/tab/inline_0/' >",id,"</a>"))
    d
  }
  
  output$DT_win <-DT::renderDataTable(   
    
    datatable(escape = FALSE,dTableau()) %>% formatStyle(
      'ca_entite',
      backgroundColor = styleInterval(100, c('gray', 'green')))
    
  )

  
  observeEvent(input$button, {
    #on va regarder sauvegarder les données
    cat("Showing", input$x, "rows\n")
    
    conn <- odbcDriverConnect('driver={SQL Server Native Client 11.0};server=airmis-colmngt-db.database.windows.net,1433;database=airmis-pilotage-prod;Uid=pwbi;Pwd=4:j45_XcN4;Encrypt=yes;TrustServerCertificate=no;Connection Timeout=30;')
    
    #on sauvegarde les données historique
    load("HistoriqueCA.RData")
    progress <- shiny::Progress$new(style = 'notification')
    progress$set(message = "Chargement des PipeBT", value = 10)
 
    #on récupere les tables Pipe CA_Pipe et Staffing BT
    bt_pipebt <- sqlQuery(conn, "SELECT * FROM bt_pipebt;")
    progress$set(message = "Chargement des CA", value = 20)
    bt_capipebt  <- sqlQuery(conn, "SELECT * FROM bt_capipebt;")
    progress$set(message = "Chargement des Groupes", value = 80)
    groupe_totem  <- sqlQuery(conn, "SELECT * FROM groupe_totem;")
    progress$set(message = "Chargement des Staffings", value = 90)
    bt_staffingbt  <- sqlQuery(conn, "SELECT * FROM bt_staffingbt;")
    
    QueryCA <-  sqlQuery(conn, "SELECT bt_pipebt.id, annee,mission, ca_entite,ca_sst_interne,ca_sst_externe,ca_markup,probabilite, 
                     etape, offre_id, compte_id, responsable_id 
                     FROM bt_pipebt, bt_capipebt WHERE pipe_id=bt_pipebt.id AND annee=2018;")
    bt_pipeTotem <- left_join(bt_pipebt, groupe_totem, by = c("totem_id" = "id"))
    bt_pipeTotemCA <- left_join(bt_capipebt, bt_pipeTotem, by = c("pipe_id" = "id"))
    #bt_pipeTotemCAStaffing <- left_join(bt_pipeTotemCA, bt_staffingbt, by = c("totem_id" = "totem_id"))
    progress$set(message = "Fermeture de la connexion", value = 100)
    close(conn)
    on.exit(progress$close())

    nouvelleSemaine <- QueryCA %>% mutate(week=week(now())) %>% mutate(year=year(now()))
    
    chargement$HistoriqueCA <- chargement$HistoriqueCA %>% filter(week !=week(now()) )

    chargement$HistoriqueCA <- rbind(chargement$HistoriqueCA, nouvelleSemaine)

    HistoriqueCA <- chargement$HistoriqueCA
    #on efface la semaine courante si elle existe
    save(HistoriqueCA,file="HistoriqueCA.RData")
    
  })
  
  #Graphe pour le pipe
  output$Pipe_Repartition <- renderPlot({
    #on va regarder les données historique vs la base SQL Azure
    data1 <- chargement$HistoriqueCA %>% filter( etape %in% c ( "0 - A qualifier", "1 - Qualifiée", "2 - A émettre",
                                                                       "3 - Emise")) %>%
      replace_na(list(ca_sst_interne = 0, ca_sst_externe = 0, ca_entite=0,ca_markup=0 )) %>%
      mutate (totalCA = ca_entite+ca_sst_interne+ca_sst_externe+ca_markup) %>%
      mutate (totalCAPondere = totalCA * probabilite) %>%
      group_by(week,etape) %>% summarise(total=sum(totalCA),totalPondere = sum(totalCAPondere))
    
    g <- ggplot(data1,aes(x=factor(week),y=total)) + 
      geom_bar(stat = "identity",
               aes(fill = etape ))+facet_wrap(~etape)+
      geom_text(aes(label=total), vjust=.90)+
      theme(legend.position = 'top')+
      theme_wsj()+ scale_colour_wsj("colors6")+
      labs(
        title = "Pipe BT",
        subtitle = "Comparaison Semanie actuelle vs Semaine passée"
      )
    g
  })
  
  
  #Graphe pour le win
  output$Win_Repartition <- renderPlot({
    #on va regarder les données historique vs la base SQL Azure
    data1 <- chargement$HistoriqueCA %>% filter( etape == "4 - Gagnée") %>%
      replace_na(list(ca_sst_interne = 0, ca_sst_externe = 0, ca_entite=0,ca_markup=0 )) %>%
      mutate (totalCA = ca_entite+ca_sst_interne+ca_sst_externe+ca_markup) %>%
      mutate (totalCAPondere = totalCA * probabilite) 
    data1 <- left_join(data1, groupe_offre, by = c("offre_id" = "id"))
    data1 <- data1 %>%
      group_by(week,offre) %>% summarise(total=sum(totalCA),totalPondere = sum(totalCAPondere))
    
    
    
    g <- ggplot(data1,aes(x=factor(week),y=total)) + 
      geom_bar(stat = "identity",
               aes(fill = offre ))+facet_wrap(~offre)+
      theme(legend.position = 'top')+
      geom_text(aes(label=round(total,0)), vjust=.90) +
      theme_wsj()+ scale_colour_wsj("colors6")+
      labs(
        title = "Mission gagnées",
        subtitle = "Comparaison Semanie actuelle vs Semaine passée"
      )
    g
  })

  #Graphe pour le win
  output$Staffing <- renderPlot({
    #on va regarder les données historique vs la base SQL Azure pour le staffing
    
    bt_staffingbt <- bt_staffingbt %>% mutate (Mois=month(date_staffing)) %>% mutate(Annee=year(date_staffing))
    bt_staffing3mois <- bt_staffingbt %>% filter (Mois %in% c(mois,moisPlus1,moisPlus2), Annee %in% c(annee, anneePlus1)) 
    
    #On calcul le nombre de jours possibles par mois
    staff3mois <- bt_staffing3mois %>% 
      filter(activite %in% c("0. Jours possibles","1. Mission ferme","2. Mission prévue","4. Congés","7. Arrêt Maladie")) %>% group_by(date_staffing,activite) %>%
      summarise(jhTotal= 1*sum(jh, na.rm=TRUE)) %>% spread(activite,jhTotal)
    colnames(staff3mois) <- c("Mois","jhPossible","Ferme","Provisoire","Conges","Maladie")
    staff3mois <- staff3mois %>% replace_na(list(Provisoire = 0, Ferme = 0, Conges=0,Maladie=0 ))
    
    #On calcule le % Ferme et % avec provisoire
    staff3mois <- staff3mois %>% mutate(PFerme= (-(Ferme)/(jhPossible+Conges+Maladie))) %>%
      mutate(PProvisoire= (-(Ferme+Provisoire)/(jhPossible+Conges+Maladie)))
    
    #on remet les données en tableau pour faire l'histograme
    staffGraph <- staff3mois %>% ungroup() %>%select(Mois,PFerme,PProvisoire) %>% 
      gather(pourcentage,Calculs,PFerme:PProvisoire)
    
    #meme calcul pour les données historique

    HistoriqueStaffing3mois <- HistoriqueStaffing %>% filter (Mois %in% c(mois,moisPlus1,moisPlus2), Annee %in% c(annee, anneePlus1)) 
    
    #On calcul le nombre de jours possibles par mois
    Histo3mois <- HistoriqueStaffing3mois %>% 
      filter(activite %in% c("0. Jours possibles","1. Mission ferme","2. Mission prévue","4. Congés","7. Arrêt Maladie")) %>% group_by(date_staffing,activite) %>%
      summarise(jhTotal= 1*sum(jh, na.rm=TRUE)) %>% spread(activite,jhTotal)
    colnames(Histo3mois) <- c("Mois","jhPossible","Ferme","Provisoire","Conges","Maladie")
    H3mois <- Histo3mois %>% replace_na(list(Provisoire = 0, Ferme = 0, Conges=0,Maladie=0 ))
    
    #On calcule le % Ferme et % avec provisoire
    H3mois <- H3mois %>% mutate(PFerme= (-(Ferme)/(jhPossible+Conges+Maladie))) %>%
      mutate(PProvisoire= (-(Ferme+Provisoire)/(jhPossible+Conges+Maladie)))
    
    #on remet les données en tableau pour faire l'histograme
    HGraph <- H3mois %>% ungroup() %>%select(Mois,PFerme,PProvisoire) %>% 
      gather(pourcentage,Calculs,PFerme:PProvisoire)
    
    g <- staffGraph %>% ggplot(aes( x=Mois,y=Calculs,fill = pourcentage,label = Calculs)) +
      scale_fill_manual(labels = c("% Ferme", "% Provisoire"),values=c("#999999", "#E69F00", "#56B4E9"))+
      geom_col(position = "dodge") + scale_y_continuous(breaks=seq(0, 1, by=0.05), labels = scales::percent)+
      geom_col(data =HGraph,position = "dodge", colour = "red", binwidth = 0.5)+
      geom_hline(yintercept = .80, colour ="red",linetype="dotted", size=2)+
      labs(
        title = "Taux de staffing sur les 3 mois à venir",
        subtitle = "Calcul (Ferme+Conges+Maladie)/jhPossible",
        caption = "En rouge les données de la semaine précédente"
      )
    g
  })
  
  output$StaffingGrade <- renderPlot({
    #on va regarder les données historique vs la base SQL Azure pour le staffing
    
    bt_staffingbt <- bt_staffingbt %>% mutate (Mois=month(date_staffing)) %>% mutate(Annee=year(date_staffing))
    bt_staffing3mois <- bt_staffingbt %>% filter (Mois %in% c(mois,moisPlus1,moisPlus2), Annee %in% c(annee, anneePlus1)) 
    bt_staffing3mois <- bt_staffing3mois %>% inner_join(bt_effectifGrade,by = c(effectif_id="id"))
    
    #On calcul le nombre de jours possibles par mois
    staff3mois <- bt_staffing3mois %>% 
      filter(activite %in% c("0. Jours possibles","1. Mission ferme","2. Mission prévue","4. Congés","7. Arrêt Maladie")) %>%
      group_by(date_staffing,activite,grade) %>%
      summarise(jhTotal= 1*sum(jh, na.rm=TRUE)) %>% spread(activite,jhTotal)
    colnames(staff3mois) <- c("Mois","Grade", "jhPossible","Ferme","Provisoire","Conges","Maladie")
    staff3mois <- staff3mois %>% replace_na(list(Provisoire = 0, Ferme = 0, Conges=0,Maladie=0 ))
    
    #On calcule le % Ferme et % avec provisoire
    staff3mois <- staff3mois %>% mutate(PFerme= (-(Ferme)/(jhPossible+Conges+Maladie))) %>%
      mutate(PProvisoire= (-(Ferme+Provisoire)/(jhPossible+Conges+Maladie)))
    
    #on remet les données en tableau pour faire l'histograme
    staffGraph <- staff3mois %>% ungroup() %>%select(Mois,Grade, PFerme,PProvisoire) %>% 
      gather(pourcentage,Calculs,PFerme:PProvisoire)
    
    g <- staffGraph %>% ggplot(aes( x=Mois,y=Calculs,fill = pourcentage,label = Calculs)) +
      scale_fill_manual(labels = c("% Ferme", "% Provisoire"),values=c("#999999", "#E69F00", "#56B4E9"))+
      geom_col(position = "dodge") + scale_y_continuous(labels = scales::percent)+
      facet_wrap(~Grade)+
      theme(legend.position="bottom")+
      geom_hline(yintercept = .80, colour ="red",size=1)+
      labs(
        title = "Taux de staffing sur les 3 mois à venir",
        subtitle = "Calcul (Ferme+Conges+Maladie)/jhPossible"
        
      )
    g
  })
  


output$StaffingHeat <- renderPlot({
  
  #on va regarder les données historique vs la base SQL Azure pour le staffing
  bt_staffingbt <- bt_staffingbt %>% mutate (Mois=month(date_staffing)) %>% mutate(Annee=year(date_staffing))
  bt_staffing3mois <- bt_staffingbt %>% filter (Mois %in% c(mois,moisPlus1,moisPlus2), Annee %in% c(annee, anneePlus1)) 
  bt_staffing3mois <- bt_staffing3mois %>% inner_join(bt_effectifGrade,by = c(effectif_id="id"))
  
  #On calcul le nombre de jours possibles par mois
  staff3mois <- bt_staffing3mois %>% 
    filter(activite %in% c("0. Jours possibles","1. Mission ferme","2. Mission prévue","4. Congés","7. Arrêt Maladie")) %>%
    group_by(date_staffing,activite,grade) %>%
    summarise(jhTotal= 1*sum(jh, na.rm=TRUE)) %>% spread(activite,jhTotal)
  colnames(staff3mois) <- c("Mois","Grade", "jhPossible","Ferme","Provisoire","Conges","Maladie")
  staff3mois <- staff3mois %>% replace_na(list(Provisoire = 0, Ferme = 0, Conges=0,Maladie=0 ))
  
  #On calcule le % Ferme et % avec provisoire
  staff3mois <- staff3mois %>% mutate(PFerme= (-(Ferme+Conges+Maladie)/jhPossible)) %>%
    mutate(PProvisoire= (-(Ferme+Provisoire)/(jhPossible+Conges+Maladie)))
  
  #on remet les données en tableau pour faire l'histograme
  staffGraph <- staff3mois %>% ungroup() %>%select(Mois,Grade, PFerme,PProvisoire) %>% 
    gather(pourcentage,Calculs,PFerme:PProvisoire)
  
  #on va calculer l'ensemble 
  staffTotal3mois <- bt_staffing3mois %>% 
    filter(activite %in% c("0. Jours possibles","1. Mission ferme","2. Mission prévue","4. Congés","7. Arrêt Maladie")) %>%
    group_by(date_staffing,activite) %>%
    summarise(jhTotal= 1*sum(jh, na.rm=TRUE)) %>% spread(activite,jhTotal) %>%
    mutate(Grade="TOTAL")
  colnames(staffTotal3mois) <- c("Mois","jhPossible","Ferme","Provisoire","Conges","Maladie","Grade")
  staffTotal3mois <- staffTotal3mois %>% replace_na(list(Provisoire = 0, Ferme = 0, Conges=0,Maladie=0 ))
  
  #On calcule le % Ferme et % avec provisoire
  staffTotal3mois <- staffTotal3mois %>% mutate(PFerme= (-(Ferme+Conges+Maladie)/jhPossible)) %>%
    mutate(PProvisoire= (-(Ferme+Provisoire)/(jhPossible+Conges+Maladie)))
  
  #on remet les données en tableau pour faire l'histograme
  staffTotalGraph <- staffTotal3mois %>% ungroup() %>%select(Mois,Grade, PFerme,PProvisoire) %>% 
    gather(pourcentage,Calculs,PFerme:PProvisoire)
  
  staffGraph <- rbind(staffGraph,staffTotalGraph)
  
  
  base_size <- 9
  p <- staffGraph%>%  ggplot(aes(Mois, Grade)) + geom_tile(aes(fill = Calculs), colour = "white") + 
    scale_fill_gradientn(limits = c(0,1),breaks=c(0,0.8,1),colors=c("red","orange","green")) +
    geom_text(aes(label = round(Calculs,2)))+
    facet_wrap(~pourcentage)+
    labs(
      title = "Taux de staffing Ferme sur les 3 mois à venir",
      subtitle = "Calcul (Ferme+Conges+Maladie)/jhPossible"
      
    )
  p
})  

output$StaffingHeatEcart <- renderPlot({
  #on va calculer les écarts entre la semaine présente et la seaine passée
  
  #on va regarder les données historique vs la base SQL Azure pour le staffing
  Histo_staffing3mois <- HistoriqueStaffing %>% filter (Mois %in% c(mois,moisPlus1,moisPlus2), Annee %in% c(annee, anneePlus1)) 
  Histo_staffing3mois <- Histo_staffing3mois %>% inner_join(bt_effectifGrade,by = c(effectif_id="id"))
  
  #On calcul le nombre de jours possibles par mois
  Histo3mois <- Histo_staffing3mois %>% 
    filter(activite %in% c("0. Jours possibles","1. Mission ferme","2. Mission prévue","4. Congés","7. Arrêt Maladie")) %>%
    group_by(date_staffing,activite,grade) %>%
    summarise(jhTotal= 1*sum(jh, na.rm=TRUE)) %>% spread(activite,jhTotal)
  colnames(Histo3mois) <- c("Mois","Grade", "jhPossible","Ferme","Provisoire","Conges","Maladie")
  Histo3mois <- Histo3mois %>% replace_na(list(Provisoire = 0, Ferme = 0, Conges=0,Maladie=0 ))
  
  
  #on va regarder les données historique vs la base SQL Azure pour le staffing
  bt_staffingbt <- bt_staffingbt %>% mutate (Mois=month(date_staffing)) %>% mutate(Annee=year(date_staffing))
  bt_staffing3mois <- bt_staffingbt %>% filter (Mois %in% c(mois,moisPlus1,moisPlus2), Annee %in% c(annee, anneePlus1)) 
  bt_staffing3mois <- bt_staffing3mois %>% inner_join(bt_effectifGrade,by = c(effectif_id="id"))
  
  #On calcul le nombre de jours possibles par mois
  staff3mois <- bt_staffing3mois %>% 
    filter(activite %in% c("0. Jours possibles","1. Mission ferme","2. Mission prévue","4. Congés","7. Arrêt Maladie")) %>%
    group_by(date_staffing,activite,grade) %>%
    summarise(jhTotal= 1*sum(jh, na.rm=TRUE)) %>% spread(activite,jhTotal)
  colnames(staff3mois) <- c("SMois","SGrade", "jhPossible","Ferme","Provisoire","Conges","Maladie")
  staff3mois <- staff3mois %>% replace_na(list(Provisoire = 0, Ferme = 0, Conges=0,Maladie=0 ))
  
  
  #on regarde les ecarts Ferme et provisoire
  EcartSemaine <- merge(staff3mois, Histo3mois, by.x=c("SMois", "SGrade"), by.y=c("Mois", "Grade"))
  EcartSemaine <-   EcartSemaine %>% mutate(jhPossible=jhPossible.x-jhPossible.y) %>%
    mutate(jhFerme=Ferme.x-Ferme.y) %>%
    mutate(jhProvisoire=Provisoire.x-Provisoire.y) %>%
    mutate(jhConges=Conges.x-Conges.y) %>%
    mutate(jhMaladie=Maladie.x-Maladie.y) %>%
    select(SMois,SGrade,jhPossible,jhFerme,jhProvisoire,jhConges,jhMaladie)
  EcartSemaine <- gather(EcartSemaine,Nature,Calculs,jhPossible:jhMaladie)
  
  p <- EcartSemaine%>%  ggplot(aes(SMois, SGrade)) + geom_tile(aes(fill = Calculs), colour = "white") + 
    scale_fill_gradientn(colors=c("Red","white","green"), breaks=c(-50,0,100),limits=c(-100,100)) +
    geom_text(aes(label = round(Calculs,2)))+
    facet_wrap(~Nature)+
    labs(
      title = "Ecart jh sur les 3 mois ",
      subtitle = "(Actuel  - Semaine passée)"
    )
  p
})  


StaffTableau <- reactive ({
  #on va regarder par personne le nb de jours par mois 
  #Evolution du staffing Ferme entre 2 semaines
  #on remplace tous les NA par des 0
  #Semaine 10
  
  
  #il faut regarder les lignes différentes 
  histo <- HistoriqueStaffing %>% filter (Mois %in% c(mois,moisPlus1,moisPlus2), Annee %in% c(annee, anneePlus1))  %>% select (id,date_staffing,activite,jh,effectif_id,saisie_staffing_id)
  Staff <- bt_staffingbt  %>% mutate (Mois=month(date_staffing)) %>% mutate(Annee=year(date_staffing)) %>%
    filter (Mois %in% c(mois,moisPlus1,moisPlus2), Annee %in% c(annee, anneePlus1)) %>%
    select (id,date_staffing,activite,jh,effectif_id,saisie_staffing_id)
  resultatDiff <- diff_data(histo,Staff)
  #on recupere dans un dataframe le resultat
  ENegatfis <- as.data.frame(resultatDiff$get_matrix())
  
  ENegatfis <- ENegatfis %>% 
    filter(V1 %in% c("+++","---","->")) %>%
    mutate(V1= ifelse(V1=="->","mod",V1)) 
    
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


})

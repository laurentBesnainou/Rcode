#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Define server logic
shinyServer(function(input, output, session) {
  
  output$Jumbo<-renderText({
    paste("DATA Dashboard BT Excel semaine :", toString(max(pilotage_data$WEEK)))
   
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
    p2017_Objectif <- data.frame(WEEK=c(1:53),
                              CA=objectif_Vente)

    pilotage_tot <- p2017
    couleur <- c('#57B8FF')
    if (2015 %in% input$Courbes) {
      pilotage_tot <- bind_rows(p2015,p2017)
      couleur <- c('#71A33C', '#57B8FF')
    }
    if (2016 %in% input$Courbes) {
      pilotage_tot <- bind_rows(p2016,p2017)
      couleur <- c( '#FF5151', '#57B8FF')
    }
    if (sum(c(2015,2016) %in% input$Courbes)==2) {
      pilotage_tot <- bind_rows(p2015,p2016,p2017)
      couleur <- c( '#71A33C','#FF5151', '#57B8FF')
    }
 
    # On va aouter une régression lineaire sur l'année 2017
    p2017_lm <- p2017  %>% 
      mutate(YEAR = factor(YEAR)) %>% 
      filter(STEP == "4 - Gagnée") %>% 
      group_by(YEAR, WEEK) %>% 
      summarise(CA_TOT = sum(CA_BT, na.rm = TRUE)) %>% ungroup() %>% select(WEEK,CA_TOT)
    
    # regression lineaire CA en fonction de la semaine ns pour regression polinomyale
    #sim1_mod <- glm(CA_TOT ~ ns(WEEK,4)  , data = p2017_lm)
    #sim1_mod <- glm(CA_TOT ~ WEEK  , data = p2017_lm,family=binomial)
    sim1_mod <- lm(CA_TOT ~ WEEK  , data = p2017_lm)
    p2017_lm <- p2017_lm %>% 
      add_residuals(sim1_mod)
    
    ggplot(p2017_lm, aes(resid)) + 
      geom_freqpoly(binwidth = 0.5)
    ggplot(p2017_lm, aes(CA_TOT , resid)) + 
      geom_ref_line(h = 0) +
      geom_point() 
    
    grid <- p2017_lm %>%
      data_grid(WEEK) 
    grid <- data.frame(WEEK=c(1:53))
    #on ajoute des predictions avec une regression lineaire
    grid <- grid %>% 
      add_predictions(sim1_mod) 

    
    plot_chat <- pilotage_tot %>% 
      mutate(YEAR = factor(YEAR)) %>% 
      filter(STEP == "4 - Gagnée") %>% 
      group_by(YEAR, WEEK) %>% 
      summarise(CA_TOT = sum(CA_BT, na.rm = TRUE))
    
    hc <-  highchart() %>% 
      
      hc_add_series (p2017_Objectif, name = "Objectif 2017",lineWidth = 1, type = "line", marker = list(enabled = FALSE),
                     color= 'orange', hcaes(x = WEEK, y = CA) ) %>% 
      hc_add_series (plot_chat, type = "line",marker = list(enabled = FALSE),
                     color= couleur, hcaes(x = WEEK, y = CA_TOT, group = YEAR) ) %>% 
      hc_add_series (grid, name = "Simulation 2017",lineWidth = 1, type = "line", marker = list(enabled = FALSE),
                      color= 'red', hcaes(x = WEEK, y = pred) ) %>% 
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
    theme <-hc_theme_economist()
    hc <- hc %>%
      hc_add_theme(theme)
    
    hc
  })
  
  output$hcPie1 <- renderHighchart({ 

  df <- pilotage_data %>% 
    filter(STEP == "4 - Gagnée",WEEK==max(pilotage_data$WEEK)) %>% 
    group_by(OFFRE_PRINCIPALE) %>%
    summarise(Montant= round(sum(TOTAL_CA_VENDU_N__KE,na.rm =TRUE ),digits=0)) %>%
    select(OFFRE_PRINCIPALE,Montant)
  Total <- sum(df$Montant)
  hc <-  highchart() %>% 
    hc_chart(type = "pie") %>% 
    hc_add_series_labels_values(name="Vente", labels = df$OFFRE_PRINCIPALE, values = df$Montant) %>%
   
    hc_subtitle(text = "Vente BT en k€") %>%
    hc_title(text = paste("Situation à date en k€: ",Total,sep=" "),
             style = list(fontWeight = "bold")) 
  
  # # Determine theme and apply to highchart ------------------------
  theme <-hc_theme_economist()
  hc <- hc %>%
    hc_add_theme(theme)
  
  hc
})
  
  
  output$hcPie2 <- renderHighchart({ 
    df <- pilotage_data %>% 
      filter(STEP == "4 - Gagnée",WEEK==max(pilotage_data$WEEK)) %>% 
      group_by(OFFRE_PRINCIPALE) %>%
      summarise(Montant= round(sum(CA_BT_PONDERE__N__KE,na.rm =TRUE ),digits=0)) %>%
      select(OFFRE_PRINCIPALE,Montant)
    Total <- sum(df$Montant)
    hc <-  highchart() %>% 
      hc_chart(type = "pie") %>% 
      hc_add_series_labels_values(name="Vente", labels = df$OFFRE_PRINCIPALE, values = df$Montant) %>%
      
      hc_subtitle(text = "Production BT en k€") %>%
      hc_title(text = paste("Situtation à date en k€: ",Total,sep=" "),
               style = list(fontWeight = "bold")) 
    
    # # Determine theme and apply to highchart ------------------------
    theme <-hc_theme_economist()
    hc <- hc %>%
      hc_add_theme(theme)
    
    hc
  })

  dataTableau <- function() {
    
   pilotage_data %>% 
      filter(STEP %in% c("2 - A émettre", "3 - Emise"),
             WEEK==max(pilotage_data$WEEK)) %>% 
      select(OFFRE_PRINCIPALE,COMPTE,SUJET, STEP, CA_BT_PONDERE__N__KE) %>% 
      arrange(desc(CA_BT_PONDERE__N__KE))
  }
  
  output$distPlot <- renderPlot({
    color <-  grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
    nbColor <- nrow( pilotage_data %>% 
                       filter(STEP %in% c("2 - A émettre", "3 - Emise"),
                              WEEK==max(pilotage_data$WEEK)))
    
    c <- colorRampPalette(c("#3794bf", "#FFFFFF", "#df8640"))(nbColor)
    
    pilotage_data %>% 
      filter(STEP %in% c("2 - A émettre", "3 - Emise"),
             WEEK==max(pilotage_data$WEEK)) %>% 

    ggplot(aes(x=OFFRE_PRINCIPALE,y=CA_BT_PONDERE__N__KE,fill=ASSOCIE)) + geom_col() +
      scale_fill_pander() + facet_grid( . ~ STEP ) + ggtitle("Ventillation du Pipe ")
    
  })
  output$tbl <-  DT::renderDataTable(
    
    
    datatable(dataTableau(),options = list(
      
      dom = 'Bfrtip', buttons = list('colvis','print',list(extend='collection',text='Download',buttons = list('copy','csv','excel','pdf'))),
      
      pageLength = 10

      
      ))  %>%
      formatRound('CA_BT_PONDERE__N__KE', digits = 0) %>%
      formatStyle(
      'STEP',
      backgroundColor = styleEqual(c("2 - A émettre", "3 - Emise"), c('orange', 'green'))) %>%
        
      formatStyle('CA_BT_PONDERE__N__KE',
                    background = styleColorBar(dataTableau()$CA_BT_PONDERE__N__KE,'lightblue'),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center') %>%
      formatStyle(
        'OFFRE_PRINCIPALE',
        #transform = 'rotateX(45deg) rotateY(20deg) rotateZ(30deg)',
        backgroundColor = styleEqual(c("Data", "Digital Innovation", "ETM", "PocLab", "Sécurité", "Transformation"),
         c('#1D3557', '#999DA3',   '#E63946', '#F1FAEE', '#A8DADC','#457B9D')
        ))
      
    
  )
  output$hcPie3 <- renderHighchart({ 
    
    df <- pilotage_data %>% 
      filter(STEP %in% c("0 - A qualifier", "1 - qualifiée", "1 - Qualifiée", "2 - A émettre", "3 - Emise 4"),WEEK==max(pilotage_data$WEEK)) %>% 
      group_by(OFFRE_PRINCIPALE) %>%
      summarise(Montant= round(sum(CA_BT_PONDERE__N__KE,na.rm =TRUE ),digits=0))
    
   
    Total <- sum(df$Montant)
    hc <-  highchart() %>% 
      hc_chart(type = "pie") %>% 
      hc_add_series_labels_values(name="Vente", labels = df$OFFRE_PRINCIPALE, values = df$Montant) %>%
      
      hc_subtitle(text = "Pipe en k€") %>%
      hc_title(text = paste("Situation à date en k€: ",Total,sep=" "),
               style = list(fontWeight = "bold")) 
    
    # # Determine theme and apply to highchart ------------------------
    theme <-hc_theme_economist()
    hc <- hc %>%
      hc_add_theme(theme)
    
    hc
  })
  
  
  output$myGauge <- renderHighchart({

    #on va calculer le nb de j pour le mois en cours
    Mois <- month(today())
    NomMois <- c("JANV","FEV","MAR","AVR","MAI","JUIN","JUIL","AOUT","SEPT","OCT","NOV","DEC")
    MoisActuel <- NomMois[Mois]
    #nombr de jours fermes au global
    Ferme <- nbFERME %>% filter (MOIS == MoisActuel) %>% group_by(MOIS) %>% summarise(total=sum(nb))
    FermePrevi  <- nbPREVI %>% filter (MOIS == MoisActuel) %>% group_by(MOIS) %>% summarise(total=sum(nb))
    #nombre de jours global possible
    Possible <- nbEffecti %>% filter (MOIS == MoisActuel) %>% group_by(MOIS) %>% summarise(total=sum(volume))
    #nombre de jours de congès
    conges <- nbCONGES  %>% filter (MOIS == MoisActuel) %>% group_by(MOIS) %>% summarise(total=sum(nb))
    #calcul
    PourcentFerme <- Ferme$total/(Possible$total-conges$total) *100
    PourcentFermePrevi <- (Ferme$total+FermePrevi$total)/(Possible$total-conges$total) *100

    highchart() %>% 
      hc_chart(type = "solidgauge",backgroundColor = "#F0F0F0",marginTop = 40) %>% 
      hc_title(text = paste("Taux du mois en cours", MoisActuel, sep= ": "),style = list(fontSize = "24px")) %>% 
      hc_tooltip(borderWidth = 1,backgroundColor = 'none',shadow = TRUE,style = list(fontSize = '16px'),
                 pointFormat = '{series.name}<br><span style="font-size:2em; color: {point.color}; font-weight: bold">{point.y}%</span>',
                 positioner = JS("function (labelWidth, labelHeight) {return {x: labelWidth ,y: 180};}")) %>% 
      hc_pane(startAngle = 0,endAngle = 360,
              background = list(
                list(outerRadius = '112%',innerRadius = '88%',backgroundColor = JS("Highcharts.Color('#F62366').setOpacity(0.1).get()"),borderWidth =  0),
                list(outerRadius = '87%',innerRadius = '63%',backgroundColor = JS("Highcharts.Color('#9DFF02').setOpacity(0.1).get()"),borderWidth = 0),
                list(outerRadius = '62%',innerRadius =  '20%',backgroundColor = JS("Highcharts.Color('#0CCDD6').setOpacity(0.1).get()"),borderWidth = 0))) %>% 
      hc_yAxis(min = 0,max = 100,lineWidth = 0,tickPositions = list()) %>% 
      hc_plotOptions(solidgauge = list(borderWidth = '25px',dataLabels = list(enabled = FALSE),linecap = 'round',stickyTracking = TRUE)) %>% 
      hc_add_series(name = "Objectif",borderColor = "red",data = list(list(color = "red",radius = "100%",innerRadius = "100%",y = 80))) %>% 
      hc_add_series(name = "Ferme + Prévi",borderColor = "#4877FE",data = list(list(color = "#4877FE",radius = "75%",innerRadius = "75%",y = round(PourcentFermePrevi,digits=0)))) %>% 
      hc_add_series(name = "Ferme",borderColor = "#00B2FF",data = list(list(color =  "#00B2FF",radius = "50%",innerRadius = "20%",y = round(PourcentFerme,digits=0))))
  })
  
  output$CA_plot_Vente <- renderPlot({
   
    ## On regarde pour le mois en cours la répartition des dépenses en comparaison avec 2015 et 2016
    
    data <- pilotage_data %>% 
      filter(STEP == "4 - Gagnée",WEEK==max(pilotage_data$WEEK)) %>% 
      group_by(OFFRE_PRINCIPALE) %>%
    summarise(Budget= round(sum(TOTAL_CA_VENDU_N__KE,na.rm =TRUE ),digits=0))
    # PlotFlower(lengths=data$Budget, labels=data$Type,widths=data$Budget, disk=0.3, main="Répartition",
    #            center="Détail",main=data$Budget, lty=2)
    couleur <-  brewer.pal(6, 'Spectral')
    NomCol <- c("Data", "Digital Innovation", "ETM", "PocLab", "Sécurité", "Transformation"  )
    couleurP <- data.frame(OFFRE_PRINCIPALE=NomCol,Coul=couleur)
    #on ajoute la couleur au type
    data <- merge(x = data, y = couleurP, by = "OFFRE_PRINCIPALE", all.x = TRUE)
    data <- data %>% mutate (Coul= as.character(Coul)) %>% mutate(OFFRE_PRINCIPALE = paste(OFFRE_PRINCIPALE,Budget,sep=" "))
    PlotFlower(lengths=data$Budget, labels=data$OFFRE_PRINCIPALE,widths=data$Budget, fill.col =data$Coul,
               disk=0.4, label.cex=0.9, label.offset=0.1, cex=1, cex.main=1, center=round(sum(data$Budget),digits=0))
    
    #   geom_line(data=CB_Mean,aes(group=ANNEE, colour=ANNEE),size=1) + scale_fill_manual(values=c("#CC6666", "#9999CC"))
    # 
    
  })
  
  output$CA_plot_Prod <- renderPlot({
    
    ## On regarde pour le mois en cours la répartition des dépenses en comparaison avec 2015 et 2016
    
    data <- pilotage_data %>% 
      filter(STEP == "4 - Gagnée",WEEK==max(pilotage_data$WEEK)) %>% 
      group_by(OFFRE_PRINCIPALE) %>%
      summarise(Budget= round(sum(CA_BT_PONDERE__N__KE,na.rm =TRUE ),digits=0))
    
    # PlotFlower(lengths=data$Budget, labels=data$Type,widths=data$Budget, disk=0.3, main="Répartition",
    #            center="Détail",main=data$Budget, lty=2)
    couleur <-  brewer.pal(6, 'Spectral')
    NomCol <- c("Data", "Digital Innovation", "ETM", "PocLab", "Sécurité", "Transformation"  )
    couleurP <- data.frame(OFFRE_PRINCIPALE=NomCol,Coul=couleur)
    #on ajoute la couleur au type
    data <- merge(x = data, y = couleurP, by = "OFFRE_PRINCIPALE", all.x = TRUE)
    data <- data %>% mutate (Coul= as.character(Coul)) %>% mutate(OFFRE_PRINCIPALE = paste(OFFRE_PRINCIPALE,Budget,sep=" "))
    PlotFlower(lengths=data$Budget, labels=data$OFFRE_PRINCIPALE,widths=data$Budget, fill.col =data$Coul,
               disk=0.4, label.cex=0.9, label.offset=0.1, cex=1.2, cex.main=1.5, center=round(sum(data$Budget),digits=0))
    
    #   geom_line(data=CB_Mean,aes(group=ANNEE, colour=ANNEE),size=1) + scale_fill_manual(values=c("#CC6666", "#9999CC"))
    # 
    
  })
  
  output$CA_plot_Pipe <- renderPlot({

    ## On regarde pour le mois en cours la répartition des dépenses en comparaison avec 2015 et 2016
    
    data <- pilotage_data %>% 
      filter(STEP %in% c("0 - A qualifier", "1 - qualifiée", "1 - Qualifiée", "2 - A émettre", "3 - Emise 4"),WEEK==max(pilotage_data$WEEK)) %>% 
      group_by(OFFRE_PRINCIPALE) %>%
      summarise(Budget= round(sum(CA_BT_PONDERE__N__KE,na.rm =TRUE ),digits=0))
    
    # PlotFlower(lengths=data$Budget, labels=data$Type,widths=data$Budget, disk=0.3, main="Répartition",
    #            center="Détail",main=data$Budget, lty=2)
    couleur <-  brewer.pal(6, 'Spectral')
    NomCol <- c("Data", "Digital Innovation", "ETM", "PocLab", "Sécurité", "Transformation"  )
    couleurP <- data.frame(OFFRE_PRINCIPALE=NomCol,Coul=couleur)
    #on ajoute la couleur au type
    data <- merge(x = data, y = couleurP, by = "OFFRE_PRINCIPALE", all.x = TRUE)
    data <- data %>% mutate (Coul= as.character(Coul)) %>% mutate(OFFRE_PRINCIPALE = paste(OFFRE_PRINCIPALE,Budget,sep=" "))
    PlotFlower(lengths=data$Budget, labels=data$OFFRE_PRINCIPALE,widths=data$Budget, fill.col =data$Coul,
               disk=0.4, label.cex=0.9, label.offset=0.1, cex=1.2, cex.main=1.5, center=round(sum(data$Budget),digits=0))
    
    #   geom_line(data=CB_Mean,aes(group=ANNEE, colour=ANNEE),size=1) + scale_fill_manual(values=c("#CC6666", "#9999CC"))
    # 
    
  })
  output$Residuals_plot <- renderPlot({
    p2017 <- pilotage_2017  %>% mutate(YEAR=2017) %>%
      rename(CA_BT = CA_BT__N__KE) %>%select(STEP,
                                             CA_BT,
                                             OFFRE_PRINCIPALE,
                                             WEEK,YEAR)

    p2017_lm <- p2017  %>% 
      mutate(YEAR = factor(YEAR)) %>% 
      filter(STEP == "4 - Gagnée") %>% 
      group_by(YEAR, WEEK) %>% 
      summarise(CA_TOT = sum(CA_BT, na.rm = TRUE)) %>% ungroup() %>% select(WEEK,CA_TOT)
    
    # regression lineaire CA en fonction de la semaine
    sim1_mod <- glm(CA_TOT ~ ns(WEEK, 5)  , data = p2017_lm)
    sim1_mod <- lm(CA_TOT ~ WEEK  , data = p2017_lm)
    p2017_lm <- p2017_lm %>% 
      add_residuals(sim1_mod) %>% mutate()
    
    ggplot(p2017_lm,aes(x=WEEK,y=resid)) + 
      geom_bar(stat = "identity",
               aes(fill = resid > 25 ))+
      geom_text(aes(x=WEEK,
                    y=resid + 90 * sign(resid),
                    label=format(resid, digits=2)),
                hjust=0.5, 
                size=5) +
      scale_fill_manual(values = c("red", "green"),name= "Ecart des prédictions",labels = c("Négatif", "Positif"))+
      theme_economist() + scale_colour_economist() +
      guides(color=guide_legend("my title"))
    
  }) 
  output$Staff_plot <- renderPlot({
    #on recupere le pipe Emis par semaine et on regarde les gains par semaine
    Pipe_Emis <- pilotage_data %>% 
      filter(STEP %in% c("3 - Emise","2 - A émettre"), OFFRE_PRINCIPALE %in%c("Transformation",	"ETM","Digital Innovation","Sécurité")) %>%
             group_by(WEEK,OFFRE_PRINCIPALE) %>% 
      summarize (CA_EMIS=sum(CA_BT__N__KE,na.rm =TRUE))
    Mission_Semaine <- pilotage_data %>% 
      filter(WEEK==max(pilotage_data$WEEK), STEP %in% c("4 - Gagnée"),year(D_GAIN)==2017,  
             OFFRE_PRINCIPALE %in%c("Transformation",	"ETM","Digital Innovation","Sécurité"))  %>% mutate (week_Gain = week(D_GAIN)) %>%
      filter(!is.na(week_Gain),week_Gain!=1)%>%
      group_by(week_Gain,OFFRE_PRINCIPALE) %>% 
      summarize (CA_Gain=sum(CA_BT__N__KE,na.rm =TRUE))
    Pipe_Emis %>% 
    ggplot(aes(x=WEEK,y=CA_EMIS,fill=OFFRE_PRINCIPALE)) + geom_bar(stat = "identity") +
      geom_bar(data=Mission_Semaine, aes(x=week_Gain,y=CA_Gain),stat = "identity",fill="red")+
      scale_fill_pander() + facet_grid( . ~ OFFRE_PRINCIPALE ) + ggtitle("Pipe & Gagné ")
    
  })
    
  output$Staff_plot <- renderPlot({
    
    #Calcul des effectifs moyens sur le mois
    nbM1 <- Staffing %>% filter (TYPE == 0, !is.na(JANV)) %>% group_by(GRADE) %>% count(GRADE) %>% mutate(MOIS = "JANV")
    nbM2 <- Staffing %>% filter (TYPE == 0, !is.na(FEV)) %>% group_by(GRADE) %>% count(GRADE)  %>% mutate(MOIS = "FEV")
    nbM3 <- Staffing %>% filter (TYPE == 0, !is.na(MAR)) %>% group_by(GRADE) %>% count(GRADE)  %>% mutate(MOIS = "MAR")
    nbM4 <- Staffing %>% filter (TYPE == 0, !is.na(AVR)) %>% group_by(GRADE) %>% count(GRADE)  %>% mutate(MOIS = "AVR")
    nbM5 <- Staffing %>% filter (TYPE == 0, !is.na(MAI)) %>% group_by(GRADE) %>% count(GRADE)  %>% mutate(MOIS = "MAI")
    nbM6 <- Staffing %>% filter (TYPE == 0, !is.na(JUIN)) %>% group_by(GRADE) %>% count(GRADE)  %>% mutate(MOIS = "JUIN")
    nbM7 <- Staffing %>% filter (TYPE == 0, !is.na(JUIL)) %>% group_by(GRADE) %>% count(GRADE)  %>% mutate(MOIS = "JUIL")
    nbM8 <- Staffing %>% filter (TYPE == 0, !is.na(AOUT)) %>% group_by(GRADE) %>% count(GRADE)  %>% mutate(MOIS = "AOUT")
    nbM9 <- Staffing %>% filter (TYPE == 0, !is.na(SEPT)) %>% group_by(GRADE) %>% count(GRADE)  %>% mutate(MOIS = "SEPT")
    nbM10 <- Staffing %>% filter (TYPE == 0, !is.na(OCT)) %>% group_by(GRADE) %>% count(GRADE)  %>% mutate(MOIS = "OCT")
    nbM11 <- Staffing %>% filter (TYPE == 0, !is.na(NOV)) %>% group_by(GRADE) %>% count(GRADE)  %>% mutate(MOIS = "NOV")
    nbM12 <- Staffing %>% filter (TYPE == 0, !is.na(DEC)) %>% group_by(GRADE) %>% count(GRADE)  %>% mutate(MOIS = "DEC")
    #CALCUL DU NOMBRE DE JOURS MAX POSSIBLE
    nbM1$volume <- nbM1$n * nbJourMois[1] 
    nbM2$volume <- nbM2$n * nbJourMois[2] 
    nbM3$volume <- nbM3$n * nbJourMois[3] 
    nbM4$volume <- nbM4$n * nbJourMois[4] 
    nbM5$volume <- nbM5$n * nbJourMois[5] 
    nbM6$volume <- nbM6$n * nbJourMois[6] 
    nbM7$volume <- nbM7$n * nbJourMois[7] 
    nbM8$volume <- nbM8$n * nbJourMois[8] 
    nbM9$volume <- nbM9$n * nbJourMois[9] 
    nbM10$volume <- nbM10$n * nbJourMois[10] 
    nbM11$volume <- nbM11$n * nbJourMois[11] 
    nbM12$volume <- nbM12$n * nbJourMois[12] 
    
    
    nbEffecti <- rbind(nbM1, nbM2, nbM3, nbM4, nbM5, nbM6, nbM7, nbM8, nbM9, nbM10, nbM11, nbM12)
    
    #on calcul le nombre de jours vendus par grade et par mois  FERME
    nbF1 <- Staffing %>% filter (TYPE ==1) %>% group_by(GRADE) %>% summarise(nb = sum(JANV,na.rm =TRUE )) %>% mutate(MOIS = "JANV")
    nbF2 <- Staffing %>% filter (TYPE ==1) %>% group_by(GRADE) %>% summarise(nb = sum(FEV,na.rm =TRUE )) %>% mutate(MOIS = "FEV")
    nbF3 <- Staffing %>% filter (TYPE ==1) %>% group_by(GRADE) %>% summarise(nb = sum(MAR,na.rm =TRUE )) %>% mutate(MOIS = "MAR")
    nbF4 <- Staffing %>% filter (TYPE ==1) %>% group_by(GRADE) %>% summarise(nb = sum(AVR,na.rm =TRUE ))  %>% mutate(MOIS = "AVR")
    nbF5 <- Staffing %>% filter (TYPE ==1) %>% group_by(GRADE) %>% summarise(nb = sum(MAI,na.rm =TRUE )) %>% mutate(MOIS = "MAI")
    nbF6 <- Staffing %>% filter (TYPE ==1) %>% group_by(GRADE) %>% summarise(nb = sum(JUIN,na.rm =TRUE ))  %>% mutate(MOIS = "JUIN")
    nbF7 <- Staffing %>% filter (TYPE ==1) %>% group_by(GRADE) %>% summarise(nb = sum(JUIL,na.rm =TRUE ))  %>% mutate(MOIS = "JUIL")
    nbF8 <- Staffing %>% filter (TYPE ==1) %>% group_by(GRADE) %>% summarise(nb = sum(AOUT,na.rm =TRUE )) %>% mutate(MOIS = "AOUT")
    nbF9 <- Staffing %>% filter (TYPE ==1) %>% group_by(GRADE) %>% summarise(nb = sum(SEPT,na.rm =TRUE )) %>% mutate(MOIS = "SEPT")
    nbF10 <- Staffing %>% filter (TYPE ==1) %>% group_by(GRADE) %>% summarise(nb = sum(OCT,na.rm =TRUE )) %>% mutate(MOIS = "OCT")
    nbF11 <- Staffing %>% filter (TYPE ==1) %>% group_by(GRADE) %>% summarise(nb = sum(NOV,na.rm =TRUE )) %>% mutate(MOIS = "NOV")
    nbF12 <- Staffing %>% filter (TYPE ==1) %>% group_by(GRADE) %>% summarise(nb = sum(DEC,na.rm =TRUE )) %>% mutate(MOIS = "DEC")
    nbFERME <- rbind(nbF1, nbF2, nbF3, nbF4, nbF5, nbF6, nbF7, nbF8, nbF9, nbF10, nbF11, nbF12)
    
    
    #on calcul le nombre de jours de congès + inactivité par grade et par mois
    nbC1 <- Staffing %>% filter (TYPE %in% c(3,7)) %>% group_by(GRADE) %>% summarise(nb = sum(JANV,na.rm =TRUE )) %>% mutate(MOIS = "JANV")
    nbC2 <- Staffing %>% filter (TYPE%in% c(3,7)) %>% group_by(GRADE) %>% summarise(nb = sum(FEV,na.rm =TRUE )) %>% mutate(MOIS = "FEV")
    nbC3 <- Staffing %>% filter (TYPE%in% c(3,7)) %>% group_by(GRADE) %>% summarise(nb = sum(MAR,na.rm =TRUE )) %>% mutate(MOIS = "MAR")
    nbC4 <- Staffing %>% filter (TYPE%in% c(3,7)) %>% group_by(GRADE) %>% summarise(nb = sum(AVR,na.rm =TRUE ))  %>% mutate(MOIS = "AVR")
    nbC5 <- Staffing %>% filter (TYPE%in% c(3,7)) %>% group_by(GRADE) %>% summarise(nb = sum(MAI,na.rm =TRUE )) %>% mutate(MOIS = "MAI")
    nbC6 <- Staffing %>% filter (TYPE%in% c(3,7)) %>% group_by(GRADE) %>% summarise(nb = sum(JUIN,na.rm =TRUE ))  %>% mutate(MOIS = "JUIN")
    nbC7 <- Staffing %>% filter (TYPE%in% c(3,7)) %>% group_by(GRADE) %>% summarise(nb = sum(JUIL,na.rm =TRUE ))  %>% mutate(MOIS = "JUIL")
    nbC8 <- Staffing %>% filter (TYPE%in% c(3,7)) %>% group_by(GRADE) %>% summarise(nb = sum(AOUT,na.rm =TRUE )) %>% mutate(MOIS = "AOUT")
    nbC9<- Staffing %>% filter (TYPE%in% c(3,7)) %>% group_by(GRADE) %>% summarise(nb = sum(SEPT,na.rm =TRUE )) %>% mutate(MOIS = "SEPT")
    nbC10 <- Staffing %>% filter (TYPE%in% c(3,7)) %>% group_by(GRADE) %>% summarise(nb = sum(OCT,na.rm =TRUE )) %>% mutate(MOIS = "OCT")
    nbC11 <- Staffing %>% filter (TYPE%in% c(3,7)) %>% group_by(GRADE) %>% summarise(nb = sum(NOV,na.rm =TRUE )) %>% mutate(MOIS = "NOV")
    nbC12 <- Staffing %>% filter (TYPE%in% c(3,7)) %>% group_by(GRADE) %>% summarise(nb = sum(DEC,na.rm =TRUE )) %>% mutate(MOIS = "DEC")
    nbCONGES <- rbind(nbC1, nbC2, nbC3, nbC4, nbC5, nbC6, nbC7, nbC8, nbC9, nbC10, nbC11, nbC12)
    
    
    #on calcul les pourcentages
    tmp <- merge(nbEffecti,nbCONGES, by=c("GRADE","MOIS"), all.x=TRUE, all.y=FALSE)
    #on supprime tous les NA du tableau
    tmp[is.na(tmp)] <- 0
    
    tmp <- tmp %>% mutate(Possible = volume - nb) %>% select (GRADE,MOIS,Possible)
    tmp <- merge(nbFERME,tmp, by=c("GRADE","MOIS"))
    tmp <- tmp %>% mutate (Pourcentage = nb/Possible)
    tmpTOTAL <-  tmp %>% group_by(MOIS) %>% summarise(Pourcentage = sum(nb)/sum(Possible)) %>% mutate(GRADE="0-Global")
    tmp <- tmp %>% select(MOIS,Pourcentage,GRADE)
    tmp <-rbind(tmp, tmpTOTAL)
    
    tmp <- tmp %>% filter(GRADE %in% input$GRADE)
    # set offset
    offset <- 0.5
    # numeric version of the levels to be bound by a box
    xmin <- "JANV"
    xmax <- "DEC"
    
    ymin <- "0-Global"
    ymax <- "0-Global"
    ggplot(data = tmp, aes(x=MOIS, y=GRADE, fill=Pourcentage)) + 
      ggtitle("Taux de staffing Ferme (congés et inactivité déduits)") +
      geom_tile(color = "white")+
      geom_rect(aes(xmin = 1- 0.5, xmax =  12+ 0.5, ymin =  1- 0.5, ymax = 1+ 0.5),
                fill = "transparent", color = "#00B2FF", size = 1.5)+
      geom_text(aes(label = round(Pourcentage, 2)))+
      scale_fill_gradient2(low = "red", high = "#0C872E", mid = "yellow", 
                           midpoint = 0.6, limit = c(0,1), space = "Lab",
                           name="Staffing Ferme") +
      xlim(c("JANV","FEV","MAR","AVR","MAI","JUIN","JUIL","AOUT","SEPT","OCT","NOV","DEC")) +
      theme_minimal()+ 
      theme(axis.text.x = element_text( 
        size = 9))
  })
  
  output$Previ_plot <- renderPlot({ 
    
    p2015 <- pilotage_2015 %>% filter(STEP == "4 - Gagnée",WEEK==max(WEEK)) %>% 
      select (D_GAIN, CA_BT__N__KE) %>% group_by(D_GAIN) %>% summarise(CA=sum(CA_BT__N__KE, na.rm = TRUE))
    p2016 <- pilotage_2016 %>% filter(STEP == "4 - Gagnée",WEEK==max(WEEK)) %>% 
      select (D_GAIN, CA_BT__N__KE) %>% group_by(D_GAIN) %>% summarise(CA=sum(CA_BT__N__KE, na.rm = TRUE))
    p2017 <- pilotage_2017 %>% filter(STEP == "4 - Gagnée",WEEK==max(WEEK)) %>% 
      select (D_GAIN, CA_BT__N__KE) %>% group_by(D_GAIN) %>% summarise(CA=sum(CA_BT__N__KE, na.rm = TRUE))
    pilotage <-  rbind(p2016,p2017)
    pilotage <- pilotage %>% filter(CA > 0, !is.na(D_GAIN))
    pilotage %>% ggplot(aes(D_GAIN,CA))+
      geom_line()+
      geom_point()+
      geom_ma(ma_fun= SMA, n=12) +
      coord_trans(y = "log10")
    
    data <- pilotage %>% as_xts(date_col = D_GAIN,CA)
    
    data <- apply.monthly(data,sum)
    
    #Augmentation des donnees
    data_AUG <- fortify(data)  %>% tk_augment_timeseries_signature()
    
    #Modelisation
    fit_lm <- lm(CA~., data = select(data_AUG,-c(Index,diff)))


    #previsions
    data_idx <-data %>%tk_index()

    #données futures
    futur_idx <- data_idx %>% tk_make_future_timeseries(n_future = 3)
    
    new_data <- futur_idx %>% tk_get_timeseries_signature() 
    
    #faire les prédictions
    pred <-  predict(fit_lm,newdata = select(new_data,-c(index,diff)))
    predictions <- tibble(date=futur_idx, value=pred)
    
    data <- fortify(data)
    data$Index <- as.Date(data$Index) 
    data %>% 
      ggplot(aes(x=Index, y=CA))+
      # Training data
      geom_line(color = palette_light()[[1]]) +scale_x_date(date_breaks = "1 month")+
      geom_point(color = palette_light()[[1]]) +
      # Predictions
      #geom_line(aes(x=date, y = value), color = palette_light()[[2]], data = predictions) +
      #geom_point(aes(x=date, y = value), color = palette_light()[[2]], data = predictions) +
      # Actuals
      geom_line(color = palette_light()[[1]], data = data) +
      geom_point(color = palette_light()[[1]], data = data) +
      # Aesthetics
      theme_tq() + scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week", date_labels = "%B")+
      theme(text = element_text(size=8),
            axis.text.x = element_text(angle=45, hjust=1)) +

      labs(title = "CA BT dans le temps",
           subtitle = "Using basic multivariate linear regression can yield accurate results")
    })
  
})

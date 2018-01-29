server <- function(input, output) {
  
  #Vaues Box sur le Staffing & le Pipe
  output$TotalSylvie <- renderValueBox({
    data <- CB_Data %>% filter(Qui=="Sylvie", year(Date)==input$an, month(Date) ==input$mois) %>%  select (Montant ) 
    
    valueBox(
      sum(data$Montant),
      "Sylvie", icon = icon("plus", lib = "glyphicon"),
      color = "green"
    )
  })
  
  output$TotalLaurent <- renderValueBox({
    
    data <- CB_Data %>% filter(Qui=="Laurent", year(Date)==input$an, month(Date) ==input$mois) %>%  select (Montant ) 
    
    valueBox(
      sum(data$Montant),
      "Laurent", icon = icon("plus", lib = "glyphicon"),
      color = "green"
    )
  })
  
  output$CB_plot_Nature <- renderPlot({
    #browser()
    ## On regarde pour le mois en cours la répartition des dépenses en comparaison avec 2015 et 2016

     data <- CB_Data %>% filter(Qui=="Sylvie", year(Date)==input$an, month(Date) ==input$mois) %>%  select (Type,Montant ) %>% group_by(Type) %>%
       summarise(Budget= sum(Montant))
     
     # PlotFlower(lengths=data$Budget, labels=data$Type,widths=data$Budget, disk=0.3, main="Répartition",
     #            center="Détail",main=data$Budget, lty=2)
     couleur <-  brewer.pal(7, 'Spectral')
     NomCol <- c( "Ecole", "Habits", "Loisirs", "Maison", "Nourriture", "Santé", "Transport")
     couleurP <- data.frame(Type=NomCol,Coul=couleur)
     #on ajoute la couleur au type
     data <- merge(x = data, y = couleurP, by = "Type", all.x = TRUE)
     data <- data %>% mutate (Coul= as.character(Coul)) %>% mutate(Type = paste(Type,Budget,sep=" "))

  PlotFlower(lengths=data$Budget, labels=data$Type,widths=data$Budget, fill.col =data$Coul,
             disk=0.4, label.cex=0.9, label.offset=0.1, cex=1.2, cex.main=1.5, center="Sylvie")

    #   geom_line(data=CB_Mean,aes(group=ANNEE, colour=ANNEE),size=1) + scale_fill_manual(values=c("#CC6666", "#9999CC"))
    # 
   
  })
  
  
  output$CB_plot_Nature2 <- renderPlot({
    #browser()
    ## On regarde pour le mois en cours la répartition des dépenses en comparaison avec 2015 et 2016
    #browser()
    data <- CB_Data %>% filter(Qui=="Laurent", year(Date)==input$an, month(Date) ==input$mois) %>%  select (Type,Montant ) %>% group_by(Type) %>%
      summarise(Budget= sum(Montant))
    
    # PlotFlower(lengths=data$Budget, labels=data$Type,widths=data$Budget, disk=0.3, main="Répartition",
    #            center="Détail",main=data$Budget, lty=2)
    couleur <-  brewer.pal(7, 'Spectral')
    NomCol <- c( "Ecole", "Habits", "Loisirs", "Maison", "Nourriture", "Santé", "Transport")
    couleurP <- data.frame(Type=NomCol,Coul=couleur)
    #on ajoute la couleur au type
    data <- merge(x = data, y = couleurP, by = "Type", all.x = TRUE)
    data <- data %>% mutate (Coul= as.character(Coul)) %>% mutate(Type = paste(Type,Budget,sep=" "))
    
    PlotFlower(lengths=data$Budget, labels=data$Type,widths=data$Budget, fill.col =data$Coul,
               disk=0.4, label.cex=0.9, label.offset=0.1, cex=1.2, cex.main=1.5, center="Laurent")
    
    #   geom_line(data=CB_Mean,aes(group=ANNEE, colour=ANNEE),size=1) + scale_fill_manual(values=c("#CC6666", "#9999CC"))
    # 
    
  })
  
  output$CB_plot_Commercant <- renderPlot({
    
    # # On regarde pour la nourriture la comparaison par année
    dataPlot <- data.frame(ANNEE=c(rep("2015",12),rep("2016",12),rep("2017",12)),MOIS=c(1:12,1:12,1:12))
    dataPlot <- dataPlot %>% mutate(ANNEE = factor(ANNEE)) %>% mutate(MOIS = factor(MOIS))
    Data_Raw <- CB_Data %>% filter (Client %in% input$mychooser$right) %>% group_by(ANNEE,MOIS) %>%
      summarise(Budget= sum(Montant)) 
    #on fait une jointure sur ANNEE MOIS puis un cumsum
    plotCA <- left_join(dataPlot, Data_Raw, by = c("ANNEE" = "ANNEE", "MOIS" = "MOIS"))
    plotCA <- plotCA %>% replace_na(list(Budget = 0))
    
    plotCA <- plotCA%>%  group_by(ANNEE) %>% mutate(cumsum=cumsum(Budget)) 
    
    plotCA %>%
      ggplot(aes(x=MOIS, y=cumsum, group=ANNEE, colour=ANNEE)) +
      geom_line() +
      geom_point() +
      scale_fill_brewer(palette = "YlOrRd") + # il y a aussi un scale_color_brewer
      labs(x = "Mois",
           y = "Dépenses CB(€)",
           fill = "") +
      theme_bw() +
      theme(axis.text.x = element_text(hjust = 1), legend.position="bottom")
    #   geom_line(data=CB_Mean,aes(group=ANNEE, colour=ANNEE),size=1) + scale_fill_manual(values=c("#CC6666", "#9999CC"))
    # 
    
  })
  output$Categorie <- renderUI({
    cat <- CB_Data %>% filter (Type %in% input$Type1) %>% distinct(Categorie)
    cat <- sort(cat$Categorie)
    
    checkboxGroupInput("Cat1", "Choississez", cat,inline = TRUE,selected=cat)
  })
  
  output$CB_plot_Categorie <- renderPlot({
    
    # # On regarde pour la nourriture la comparaison par année
    CB_Data %>% filter (Type == input$Type1, Categorie %in% input$Cat1) %>% group_by(ANNEE, Categorie, MOIS) %>%
      summarise(Budget= sum(Montant)) %>% 
      ggplot(aes(x=MOIS, y=Budget)) +
      geom_bar(aes(group=ANNEE, fill= ANNEE), stat="identity",position="dodge")+
      #scale_fill_brewer(palette = "YlOrRd") + # il y a aussi un scale_color_brewer
      labs(x = "Mois",
           y = "Dépenses CB(€)",
           fill = "") +
      theme_solarized() +
      theme(axis.text.x = element_text(hjust = 1), legend.position="bottom")  + facet_grid(. ~ Categorie)
    #   geom_line(data=CB_Mean,aes(group=ANNEE, colour=ANNEE),size=1) + scale_fill_manual(values=c("#CC6666", "#9999CC"))
    # 
  })
  
  
  output$tbl <- renderTable({
    datasetInput  <- CB_Data %>% 
      filter (Type == input$Type1, Categorie %in% input$Cat1) %>% 
      group_by(ANNEE,Client,Type, Categorie) %>% 
     summarise(Depenses = sum(Montant))  %>% spread(ANNEE , Depenses, fill=" ") %>% 
      arrange(Client) %>% select(-Type) 
    titre <- c("Type",	"Client",	"Categorie",	"a2015",	"a2016",	"a2017")
    nb <- length(colnames(datasetInput))
    
    colnames(datasetInput) <- titre[1:nb]
    if (nb == length(titre)) {
      datasetInput  <- datasetInput  %>% 
        mutate(Projection2017 = as.double(a2017)*12/(month(now())-1)) %>%
        mutate(Ecart2017_2016 = as.double(Projection2017)-as.double(a2016))
    }
    
    # %>% 
      # mutate(Ecart2016 = as.double(Projection2017) - as.double(a2016))
    datasetInput}
  )
  
  
  output$tbl_TOTAL <- renderTable({
    datasetInput  <- CB_Data %>% filter (Type %in% input$Type, ANNEE=="2017") %>% group_by(Type) %>% 
      summarise(Depenses = sum(Montant)) %>% arrange(Type)
    datasetInput}
  )
  
  
  output$hcontainer <- renderHighchart({
    
    # # On regarde pour la nourriture la comparaison par année
    plot_chart <-   CB_Data %>% 
      filter (Type %in% input$Type) %>% 
      group_by(ANNEE,MOIS) %>%
      summarise(Budget= sum(Montant)) 
    
    plot_chart2015 <- plot_chart %>% filter (ANNEE == 2015)
    plot_chart2016 <- plot_chart %>% filter (ANNEE == 2016)
    plot_chart2017 <- plot_chart %>% filter (ANNEE == 2017)
    nbRow <- nrow(plot_chart2017)
    plot_Mois <- data.frame(MOIS=factor(c(1:12)),Budget=rep(0,12))
    plot_chart2015 <- left_join(plot_Mois,plot_chart2015, by="MOIS")
    plot_chart2016 <- left_join(plot_Mois,plot_chart2016, by="MOIS")
    plot_chart2017 <- left_join(plot_Mois,plot_chart2017, by="MOIS")
    
    mean2015 <- data.frame(Mois=c(1:12),Budget=rep(sum(plot_chart[plot_chart$ANNEE=="2015","Budget"], na.rm = TRUE)/12,12))
    mean2016 <- data.frame(Mois=c(1:12),Budget=rep(sum(plot_chart[plot_chart$ANNEE=="2016","Budget"], na.rm = TRUE)/12,12))
    mean2017 <- data.frame(Mois=c(1:12),Budget=rep(sum(plot_chart[plot_chart$ANNEE=="2017","Budget"], na.rm = TRUE)/nbRow,12))
    highchart() %>%
       hc_xAxis(categories =  c(	"janvier",	"février",	"mars",	"avril",	"mai",	"juin",	"juillet",	"août",
                                 "septembre",	"octobre",	"novembre",	"décembre"),
                title = list(text = "Mois")) %>% 
      hc_yAxis(title = list(text = "Montant")) %>% 
      hc_add_series(data = plot_chart2015$Budget.y, name = "CB 2015",
                    type = "column", color = "#e5b13a") %>% 
      hc_add_series(data = plot_chart2016$Budget.y, name = "CB 2016",
                    type = "column", color = "#4bd5ee") %>% 
      hc_add_series(data = plot_chart2017$Budget.y, name = "CB 2017",
                    type = "column", color = "#076AB6") %>% 
      hc_add_series(data = mean2015$Budget, name = "Moyenne 2015",
                    type = "line", color = "#e5b13a") %>%
      hc_add_series(data = mean2016$Budget, name = "moyenne 2016",
                    type = "line", color = "#4bd5ee") %>%
      hc_add_series(data = mean2017$Budget, name = "moyenne 2017",
                    type = "line", color = "#A0010F") %>%
      
      # hc_yAxis(title = list(text = "Montant Cartes"), allowDecimals = FALSE) %>%
      # hc_xAxis(categories = c(	"janvier",	"février",	"mars",	"avril",	"mai",	"juin",	"juillet",	"août",
      #                          "septembre",	"octobre",	"novembre",	"décembre"),
      #          tickmarkPlacement = "on",
      #          opposite = TRUE) %>%
      hc_title(text = "Montant CB depuis 2015",
               style = list(fontWeight = "bold")) %>% 
      hc_subtitle(text = "Données Relevés") %>%
      # hc_tooltip(valueDecimals = 4,
      #            pointFormat = "Day: {point.x} <br> Diff: {point.y}") %>%
      hc_credits(enabled = TRUE, 
                 text = "Sources: LCL",
                 style = list(fontSize = "10px")) %>%
      hc_plotOptions(zoomtype = "y") %>%
      hc_add_theme(hc_theme_538()) %>%
      hc_tooltip(pointFormat =  "Value: {point.y:.0f} Euros")
        
      
  })
  
  
  output$hc <- renderHighchart({
 
    CB_Data_LB_Plot <- CB_Data  %>% filter(ANNEE==2017) %>% group_by(Qui,Mois) %>% summarise(CB=sum(Montant)) 
    CB_Data_ALL_Plot <- CB_Data  %>% filter(ANNEE==2017) %>% group_by(Mois) %>% summarise(CB=sum(Montant)) %>%
      mutate(Mois = factor(Mois))

    CB_Data_LB_Plot <- CB_Data_LB_Plot %>% 
      mutate(Mois = factor(Mois))
      
    
    hc <-  highchart() %>% 
      hc_add_series (name = "Total Carte Sur le mois", CB_Data_ALL_Plot, type = "column",marker = list(enabled = FALSE), 
                     color= c('#20A4F3'), hcaes(x = Mois, y = CB) ) %>%
      hc_add_series (CB_Data_LB_Plot, type = "line",marker = list(enabled = FALSE), 
                     color= c('#E2FF0A', '#FF5151'), hcaes(x = Mois, y = CB,group=Qui) ) %>% 
      
      hc_title(text = "Evolution par mois",
               style = list(fontWeight = "bold")) %>% 
      hc_add_theme(hc_theme_flat()) %>%
      hc_xAxis(categories=c("janvier","février","mars","avril","mai","juin","juillet","aout","septembre","octobre","novembre","décembre"),labels = list(rotation=-45)) #%>%
      # hc_tooltip(crosshairs = TRUE, valueDecimals = 0,
      #            pointFormat = "Mois: {point.x} <br> montant en € : {point.y}") 
      # 
    
    # # Determine theme and apply to highchart ------------------------
    
    # theme <- switch(input$hc_theme,
    #                 chalk = hc_theme_chalk(),
    #                 darkunica = hc_theme_darkunica(),
    #                 fivethirtyeight = hc_theme_538(),
    #                 gridlight = hc_theme_gridlight(),
    #                 handdrawn = hc_theme_handdrawn(),
    #                 economist = hc_theme_economist(),
    #                 financialTimes =hc_theme_ft(),
    #                 Dotabuff=hc_theme_db(),
    #                 flat=hc_theme_flat(),
    #                 sandsignika = hc_theme_sandsignika()
    # )
    # hc <- hc %>%
    #   hc_add_theme(theme)
    
    hc
  })
  
  
  output$hcontainer2 <- renderHighchart({
    
    # # On regarde pour la nourriture la comparaison par année
    Data_Raw <- CB_Data %>% 
      filter (Client %in% input$mychooser$right) %>% 
      group_by(ANNEE,MOIS) %>%
      summarise(Budget= sum(Montant)) 
    nomsMois <- c(	"janvier",	"février",	"mars",	"avril",	"mai",	"juin",	"juillet",	"août",
       "septembre",	"octobre",	"novembre",	"décembre")
    #Data_Raw$MOIS <- nomsMois[Data_Raw$MOIS]
    plot_chat2015 <- Data_Raw %>% filter (ANNEE == 2015)
    plot_chat2016 <- Data_Raw %>% filter (ANNEE == 2016)
    plot_chat2017 <- Data_Raw %>% filter (ANNEE == 2017)
    plot_Mois <- data.frame(MOIS=factor(c(1:12)),Budget=rep(0,12))
    plot_chat2015 <- left_join(plot_Mois,plot_chat2015, by="MOIS")
    plot_chat2016 <- left_join(plot_Mois,plot_chat2016, by="MOIS")
    plot_chat2017 <- left_join(plot_Mois,plot_chat2017, by="MOIS")
    plot_chat <-   CB_Data %>% 
      filter (ANNEE == 2017) %>% 
      group_by(ANNEE,MOIS) %>%
      summarise(Budget= sum(Montant)) 
    mean2015 <- data.frame(Mois=c(1:12),Budget=rep(sum(plot_chat2015$Budget.y, na.rm = TRUE)/12,12))
    mean2016 <- data.frame(Mois=c(1:12),Budget=rep(sum(plot_chat2016$Budget.y, na.rm = TRUE)/12,12))
    mean2017 <- data.frame(Mois=c(1:12),Budget=rep(sum(plot_chat2017$Budget.y, na.rm = TRUE)/nrow(plot_chat),12))
    highchart() %>%
      hc_xAxis(categories=nomsMois, title = list(text = "Mois")) %>% 
      hc_yAxis(title = list(text = "Montant")) %>% 
      hc_add_series(data = plot_chat2015$Budget.y, name = "CB 2015",
                    type = "column", color = "#e5b13a") %>% 
      hc_add_series(data = plot_chat2016$Budget.y, name = "CB 2016",
                    type = "column", color = "#4bd5ee") %>% 
      hc_add_series(data = plot_chat2017$Budget.y, name = "CB 2017",
                    type = "column", color = "#076AB6") %>% 
      hc_add_series(data = mean2015$Budget, name = "Moyenne 2015",
                    type = "line", color = "#e5b13a") %>%
      hc_add_series(data = mean2016$Budget, name = "Moyenne 2016",
                    type = "line", color = "#4bd5ee") %>%
      hc_add_series(data = mean2017$Budget, name = "Moyenne 2017",
                    type = "line", color = "#A0010F") %>%

      # hc_yAxis(title = list(text = "Montant Cartes"), allowDecimals = FALSE) %>%
      # hc_xAxis(categories = c(	"janvier",	"février",	"mars",	"avril",	"mai",	"juin",	"juillet",	"août",
      #                          "septembre",	"octobre",	"novembre",	"décembre"),
      #          tickmarkPlacement = "on",
      #          opposite = TRUE) %>%
      hc_title(text = "Montant CB depuis 2015",
               style = list(fontWeight = "bold")) %>% 
      hc_subtitle(text = "Données Relevés") %>%
      # hc_tooltip(valueDecimals = 4,
      #            pointFormat = "Day: {point.x} <br> Diff: {point.y}") %>%
      hc_credits(enabled = TRUE, 
                 text = "Sources: LCL",
                 style = list(fontSize = "10px")) %>%
      hc_plotOptions(zoomtype = "y") %>%
      hc_add_theme(hc_theme_538()) %>%
      hc_tooltip(pointFormat =  "Value: {point.y:.0f} Euros")
    
  })
  
  output$hcontainer3 <- renderHighchart({
    
    # # On regarde pour la nourriture la comparaison par année
    plot_chart <-   CB_Data %>% 
      filter (Date >= as.POSIXct(input$dateRange[1]),Date <= as.POSIXct(input$dateRange[2]),Type %in% input$Type2) %>% 
      select(Client,Montant) %>% group_by(Client) %>%
      summarize(Budget=sum(Montant))%>% arrange(Client)
    plot_Pie <-   CB_Data %>% 
      filter (Date >= as.POSIXct(input$dateRange[1]),Date <= as.POSIXct(input$dateRange[2]), Type %in% input$Type2) %>% 
      select(Type,Montant) %>% group_by(Type) %>%
      summarize(Budget=sum(Montant))
    noms <- as.vector(plot_chart$Client)
    highchart() %>%
      hc_xAxis( categories= noms,
               title = list(text = "Commerçant")) %>% 
      hc_yAxis(title = list(text = "Montant")) %>% 
      hc_add_series(data = plot_chart$Budget, name = "Depenses sur la période",
                    type = "column") %>%
      hc_add_series_labels_values(plot_Pie$Type, plot_Pie$Budget,
                                  type = "pie",
                                  name = "Bar", colorByPoint = TRUE, center = c('80%', '25%'),
                                  size = 100, dataLabels = list(enabled = FALSE)) %>%
      hc_tooltip(pointFormat =   "TOTAL:  {point.y:.0f} Euros") %>%
      
      hc_title(text = paste("Detail sur la période ",input$dateRange[1]," au ", input$dateRange[2]),
                style = list(fontWeight = "bold")) %>% 
       hc_subtitle(text = "Données Relevés") %>%
       
       hc_credits(enabled = TRUE, 
                  text = "Sources: LCL",
                  style = list(fontSize = "10px")) %>%
       hc_plotOptions(zoomtype = "y") %>%
       hc_add_theme(hc_theme_538()) 
      # hc_tooltip(pointFormat =  "Montant: {point.y:.0f} Euros")
    
  })
  
  
  output$Comapraison2015 <- renderValueBox({
    #browser()
    date1 <- gsub("2017","2015",input$dateRange[1])
    date1 <- gsub("2016","2015",date1)
    date2 <- gsub("2017","2015",input$dateRange[2])
    date2 <- gsub("2016","2015",date2)
    Total <- CB_Data %>% 
      filter (Date >= date1,Date <= as.POSIXct(paste(date2," 23:59:59")),Type %in% input$Type2) %>% 
      select(Montant)
    
   
    valueBox(
      paste(sum(Total$Montant) ,"Euros"),
      paste("2015",date1,date2,sep = "-"),
      icon = icon("credit-card")
    )
  })
  
  output$Comapraison2016 <- renderValueBox({
    date1 <- gsub("2017","2016",input$dateRange[1])
    date1 <- gsub("2015","2016",date1)
    date2 <- gsub("2017","2016",input$dateRange[2])
    date2 <- gsub("2015","2016",date2)
    Total <- CB_Data %>% 
      filter (Date >= as.POSIXct(date1),Date <= as.POSIXct(paste(date2," 23:59:59")),Type %in% input$Type2) %>% 
      select(Montant)
    valueBox(
      paste(sum(Total$Montant) ,"Euros"),
      paste("2016",date1,date2,sep = "-"),
      color = "green",
      icon = icon("credit-card")
    )
  
  })
  
  output$Comapraison2017 <- renderValueBox({
    date1 <- gsub("2015","2017",input$dateRange[1])
    date1 <- gsub("2016","2017",date1)
    date2 <- gsub("2015","2017",input$dateRange[2])
    date2 <- gsub("2016","2017",date2)
    Total <- CB_Data %>% 
      filter (Date >= as.POSIXct(date1),Date <= as.POSIXct(paste(date2," 23:59:59")),Type %in% input$Type2) %>% 
      select(Montant)
    valueBox(
      paste(sum(Total$Montant) ,"Euros"),
      paste("Période du : ",date1,date2,sep = " "),
      color = "orange",
      icon = icon("credit-card")
    )
  })
  
  output$Commerce2015 <- renderValueBox({
    #browser()
    Total <- CB_Data %>% 
      filter (Client %in% input$mychooser$right,ANNEE=="2015") %>% 
      group_by(ANNEE) %>%
      summarise(Budget= sum(Montant))
    
    
    valueBox(
      paste(Total$Budget ," Euros"),
      "2015",
      icon = icon("credit-card")
    )
  })
  output$Commerce2016 <- renderValueBox({
    #browser()
    Total <- CB_Data %>% 
      filter (Client %in% input$mychooser$right,ANNEE=="2016") %>% 
      group_by(ANNEE) %>%
      summarise(Budget= sum(Montant))
    
    
    valueBox(
      paste(Total$Budget ," Euros"),
      "2016",
      color = "green",
      icon = icon("credit-card")
    )
  })
  
  output$Commerce2017 <- renderValueBox({
    #browser()
    Total <- CB_Data %>% 
      filter (Client %in% input$mychooser$right,ANNEE=="2017") %>% 
      group_by(ANNEE) %>%
      summarise(Budget= sum(Montant))
    
    
    valueBox(
      paste(Total$Budget ," Euros"),
      "2017",
      color = "orange",
      icon = icon("credit-card")
    )
  })
  
  
}
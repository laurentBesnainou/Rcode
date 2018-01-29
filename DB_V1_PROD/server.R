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
  
  dTableau <- function() {
    
   d <- pilotage_data  %>% filter(WEEK==max(WEEK), STEP== "4 - Gagnée",ASSOCIE %in% input$Associe, OFFRE_PRINCIPALE %in% input$Offre)%>%
      select(ASSOCIE,COMPTE,OFFRE_PRINCIPALE, SUJET,TOTAL_CA_VENDU_N__KE,GROUPE,D_GAIN)

   colnames(d) <- c("Asso", "Compte", "Offre","Mission","CA","Groupe","Dte Gain")
   d
  }
  
  output$DT_win <-DT::renderDataTable(   
    
    datatable(escape = FALSE,dTableau()) %>% formatStyle(
      'CA',
      backgroundColor = styleInterval(100, c('gray', 'green')))
    
  )
#Graphe pour mettre 
  output$PROD_Repartition <- renderPlot({
   
    #on va regarder le montant de gain par mois
    parMois <- pilotage_data %>% filter(WEEK==max(WEEK), STEP== "4 - Gagnée",ASSOCIE %in% input$Associe, OFFRE_PRINCIPALE %in% input$Offre) %>%
      mutate(Mois = month(D_GAIN)) %>%
      mutate(Annee = year(D_GAIN)) %>% 
      mutate(dte=dmy(paste("01",Mois,Annee,sep="/") )) %>%group_by(OFFRE_PRINCIPALE, dte) %>%
      summarise(Budget= round(sum(TOTAL_CA_VENDU_N__KE,na.rm =TRUE ),digits=0))
    # nom <- c("Janv", "F", "Mars", "Avr", "M","Juin","Jui","A","S","O","N","D")
    # parMois$Mois <- nom[parMois$Mois]

    g <- ggplot(parMois,aes(x=dte,y=Budget)) + 
      geom_bar(stat = "identity",
               aes(fill = OFFRE_PRINCIPALE ))+
      theme(axis.text.x=element_text(angle=45,hjust=1))+
      scale_x_date(date_labels="%b %y",date_breaks  ="1 month")+
      scale_colour_brewer(type = 'qual', palette = 'Dark2') +
      scale_fill_manual(values=couleurs)+
      theme(legend.position = 'top')
    g
  })
  
  observe({
    annee<-input$annee
    
   asso <- pilotage_data %>% filter(WEEK==max(WEEK), STEP== "4 - Gagnée", year(DATE_REF)==annee,!is.na(TOTAL_CA_VENDU_N__KE)) %>% distinct(ASSOCIE)
    
   updateCheckboxGroupInput(session,"Associe",choices=asso$ASSOCIE)

  })

  output$Pipe_Repartition <- renderPlot({
    
    
    data <- pilotage_data %>% 
      filter(WEEK==max(WEEK),STEP %in% c("4 - Gagnée"), ASSOCIE %in% input$Associe, OFFRE_PRINCIPALE %in% input$Offre) %>% 
      mutate(GROUPE=toupper(GROUPE)) %>%
      group_by(OFFRE_PRINCIPALE, GROUPE) %>%
      summarise(Budget= round(sum(TOTAL_CA_VENDU_N__KE,na.rm =TRUE ),digits=0))
   
    is.na(data$GROUPE) <- data$GROUPE == 'a'
    if (nrow(data)!=0) {
    ggplot(data,aes(x=OFFRE_PRINCIPALE,y=Budget)) + 
      geom_bar(stat = "identity",
               aes(fill = OFFRE_PRINCIPALE ))+
        scale_fill_manual(values=couleurs)+
      geom_text(aes(label = Budget),size = 4) +
      #theme_economist() + scale_colour_economist() +
      facet_wrap(c("GROUPE"))+
        
        theme_solarized()+theme(legend.position = 'top')
    }
  })

  
  output$sankey <- renderSankeyNetwork({
    sankeyData <- pilotage_data %>% filter(WEEK==max(WEEK),STEP %in% c("4 - Gagnée"), ASSOCIE %in% input$Associe, OFFRE_PRINCIPALE %in% input$Offre) %>%
      filter(WEEK==max(WEEK),STEP %in% c("4 - Gagnée"))
    levelsOFFRES <- levels(factor(sankeyData$OFFRE_PRINCIPALE))
    levelsCompte <- levels(factor(sankeyData$GROUPE))
    levelsAssocie <- levels(factor(sankeyData$ASSOCIE))
    ListNode <- data.frame(name=c(levelsOFFRES,levelsCompte,levelsAssocie))
    nb <- length (ListNode[[1]])-1
    indiceNode <-data.frame(Node=c(levelsOFFRES,levelsCompte,levelsAssocie),
                            Index=c(0:nb))
    src2 <- sankeyData %>% group_by(OFFRE_PRINCIPALE,ASSOCIE) %>% summarize(Budget=round(sum(TOTAL_CA_VENDU_N__KE,na.rm =TRUE ),digits=0))
    src1 <- sankeyData %>% group_by(GROUPE,OFFRE_PRINCIPALE) %>% summarize(Budget=round(sum(TOTAL_CA_VENDU_N__KE,na.rm =TRUE ),digits=0))
    #jointure sur les champs 
    l1 <- merge(src1, indiceNode,  by.x = c("GROUPE"), by.y = c("Node"), all.x=TRUE)
    l2 <- merge(l1, indiceNode,  by.x = c("OFFRE_PRINCIPALE"), by.y = c("Node"), all.x=TRUE)
    liens <- data.frame(source=l2$Index.x,target=l2$Index.y,value=l2$Budget)
    l1 <- merge(src2, indiceNode,  by.x = c("OFFRE_PRINCIPALE"), by.y = c("Node"), all.x=TRUE)
    l2 <- merge(l1, indiceNode,  by.x = c("ASSOCIE"), by.y = c("Node"), all.x=TRUE)
    liens2 <- data.frame(source=l2$Index.x,target=l2$Index.y,value=l2$Budget)
    liens <- rbind(liens,liens2)
    sankeyNetwork(Links = liens, Nodes = ListNode, Source = "source",
                                 Target = "target", Value = "value", NodeID = "name",
                                fontSize = 12, nodeWidth = 30,sinksRight = TRUE)
    
  })
  
  
  output$sankey2 <- renderSankeyNetwork({
    sankeyData <- pilotage_data %>% filter(WEEK==max(WEEK),STEP %in% c("4 - Gagnée"), ASSOCIE %in% input$Associe, OFFRE_PRINCIPALE %in% input$Offre) %>%
      filter(WEEK==max(WEEK),STEP %in% c("4 - Gagnée"))
    levelsOFFRES <- levels(factor(sankeyData$OFFRE_PRINCIPALE))
    levelsGroupe <- levels(factor(sankeyData$GROUPE))
    levelsCompte <- paste(levels(factor(sankeyData$COMPTE)),".")
    ListNode <- data.frame(name=c(levelsOFFRES,levelsCompte,levelsGroupe))
    nb <- length (ListNode[[1]])-1
    indiceNode <-data.frame(Node=c(levelsOFFRES,levelsCompte,levelsGroupe),
                            Index=c(0:nb))
    src2 <- sankeyData %>% mutate(COMPTE=paste(COMPTE,".")) %>% group_by(COMPTE,OFFRE_PRINCIPALE) %>% summarize(Budget=round(sum(TOTAL_CA_VENDU_N__KE,na.rm =TRUE ),digits=0))
    src1 <- sankeyData %>% mutate(COMPTE=paste(COMPTE,".")) %>% group_by(GROUPE,COMPTE) %>% summarize(Budget=round(sum(TOTAL_CA_VENDU_N__KE,na.rm =TRUE ),digits=0))
    #jointure sur les champs 
    l1 <- merge(src1, indiceNode,  by.x = c("GROUPE"), by.y = c("Node"), all.x=TRUE)
    l2 <- merge(l1, indiceNode,  by.x = c("COMPTE"), by.y = c("Node"), all.x=TRUE)
    liens <- data.frame(source=l2$Index.x,target=l2$Index.y,value=l2$Budget)
    l1 <- merge(src2, indiceNode,  by.x = c("COMPTE"), by.y = c("Node"), all.x=TRUE)
    l2 <- merge(l1, indiceNode,  by.x = c("OFFRE_PRINCIPALE"), by.y = c("Node"), all.x=TRUE)
    liens2 <- data.frame(source=l2$Index.x,target=l2$Index.y,value=l2$Budget)
    liens <- rbind(liens,liens2)
    # URL <- "https://cdn.rawgit.com/christophergandrud/networkD3/master/JSONdata/energy.json"
    # Energy <- jsonlite::fromJSON(URL)
    # sankeyNetwork(Links = Energy$links, Nodes = Energy$nodes, Source = "source",
    #               Target = "target", Value = "value", NodeID = "name",
    #               fontSize = 12, nodeWidth = 30, sinksRight = input$sinksRight)
    sankeyNetwork(Links = liens, Nodes = ListNode, Source = "source",
                  Target = "target", Value = "value", NodeID = "name",
                  fontSize = 12, nodeWidth = 30,sinksRight = TRUE)
    
  })
  
  
  output$sankey3 <- renderSankeyNetwork({

    sankeyData <- pilotage_data %>% filter(WEEK==max(WEEK),STEP %in% c("4 - Gagnée"), ASSOCIE %in% input$Associe, OFFRE_PRINCIPALE %in% input$Offre) %>%
      filter(WEEK==max(WEEK),STEP %in% c("4 - Gagnée"))
    levelsOFFRES <- levels(factor(sankeyData$OFFRE_PRINCIPALE))
    levelsGroupe <- levels(factor(sankeyData$GROUPE))
    levelsCompte <- paste(levels(factor(sankeyData$ENTITE__APPORTEUR)),".")
    ListNode <- data.frame(name=c(levelsCompte,levelsOFFRES,levelsGroupe))
    nb <- length (ListNode[[1]])-1
    indiceNode <-data.frame(Node=c(levelsCompte,levelsOFFRES,levelsGroupe),
                            Index=c(0:nb))
    src2 <- sankeyData %>% mutate(ENTITE__APPORTEUR=paste(ENTITE__APPORTEUR,".")) %>% 
      group_by(ENTITE__APPORTEUR,OFFRE_PRINCIPALE) %>% 
      summarize(Budget=round(sum(TOTAL_CA_VENDU_N__KE,na.rm =TRUE ),digits=0))
    src1 <- sankeyData %>% mutate(ENTITE__APPORTEUR=paste(ENTITE__APPORTEUR,".")) %>% 
      group_by(GROUPE,ENTITE__APPORTEUR) %>% 
      summarize(Budget=round(sum(TOTAL_CA_VENDU_N__KE,na.rm =TRUE ),digits=0))
    #jointure sur les champs 
    l1 <- merge(src1, indiceNode,  by.x = c("ENTITE__APPORTEUR"), by.y = c("Node"), all.x=TRUE)
    l2 <- merge(l1, indiceNode,  by.x = c("GROUPE"), by.y = c("Node"), all.x=TRUE)
    liens <- data.frame(source=l2$Index.x,target=l2$Index.y,value=l2$Budget)
    l1 <- merge(src2, indiceNode,  by.x = c("ENTITE__APPORTEUR"), by.y = c("Node"), all.x=TRUE)
    l2 <- merge(l1, indiceNode,  by.x = c("OFFRE_PRINCIPALE"), by.y = c("Node"), all.x=TRUE)
    liens2 <- data.frame(source=l2$Index.x,target=l2$Index.y,value=l2$Budget)
    liens <- rbind(liens,liens2)
    # URL <- "https://cdn.rawgit.com/christophergandrud/networkD3/master/JSONdata/energy.json"
    # Energy <- jsonlite::fromJSON(URL)
    # sankeyNetwork(Links = Energy$links, Nodes = Energy$nodes, Source = "source",
    #               Target = "target", Value = "value", NodeID = "name",
    #               fontSize = 12, nodeWidth = 30, sinksRight = input$sinksRight)
    sankeyNetwork(Links = liens, Nodes = ListNode, Source = "source",
                  Target = "target", Value = "value", NodeID = "name",
                  fontSize = 12, nodeWidth = 30,sinksRight = TRUE)
    
  })
  output$BT_Repartition <- renderPlot({
    
    data <- pilotage_data %>% 
      filter(WEEK==max(WEEK),STEP %in% c("4 - Gagnée"), ASSOCIE %in% input$Associe, OFFRE_PRINCIPALE %in% input$Offre) %>% 
      mutate(COMPTE=toupper(COMPTE)) %>% 
      select(D_GAIN,TOTAL_CA_VENDU_N__KE,OFFRE_PRINCIPALE,SUJET)
    
    ggplot(data,aes(x=D_GAIN,y=TOTAL_CA_VENDU_N__KE)) + 
      geom_point(
               aes(color = OFFRE_PRINCIPALE,size=TOTAL_CA_VENDU_N__KE))+
      
      scale_color_manual(values=couleurs)+
      theme(legend.position = 'top')
    
  })
 
  output$TotalBox <- renderValueBox({
    
    data <- pilotage_data %>% 
      filter(WEEK==max(WEEK),STEP %in% c("4 - Gagnée"), ASSOCIE %in% input$Associe,OFFRE_PRINCIPALE %in% input$Offre) 
    total <-   round(sum(data$TOTAL_CA_VENDU_N__KE,na.rm =TRUE ),digits=0)
    valueBox(
      paste0(total, " k€"), "TOTAL", icon = icon("money"),
      color = "purple"
    )
  })
  
  output$TotalETMBox <- renderValueBox({
    data <- pilotage_data %>% 
      filter(WEEK==max(WEEK),STEP %in% c("4 - Gagnée"), ASSOCIE %in% input$Associe, OFFRE_PRINCIPALE %in% c("ETM")) 
    total <-   round(sum(data$TOTAL_CA_VENDU_N__KE,na.rm =TRUE ),digits=0)
    if (sum("ETM"==input$Offre)==0) {total <- 0}
    valueBox(
      paste0(total, " k€"), "ETM", icon = icon("money"),
      color = "purple"
    )
  })
  
  output$TotalTRANSFOBox <- renderValueBox({
    
    data <- pilotage_data %>% 
      filter(WEEK==max(WEEK),STEP %in% c("4 - Gagnée"), ASSOCIE %in% input$Associe, OFFRE_PRINCIPALE %in% c("Transformation")) 
    total <-   round(sum(data$TOTAL_CA_VENDU_N__KE,na.rm =TRUE ),digits=0)
    if (sum("Transformation"==input$Offre)==0) {total <- 0}
    valueBox(
      paste0(total, " k€"), "Transformation", icon = icon("money"),
      color = "purple"
    )
  })
  
  output$TotalDIGITALBox <- renderValueBox({
    data <- pilotage_data %>% 
      filter(WEEK==max(WEEK),STEP %in% c("4 - Gagnée"), ASSOCIE %in% input$Associe, OFFRE_PRINCIPALE %in% c("Digital Innovation")) 
    total <-   round(sum(data$TOTAL_CA_VENDU_N__KE,na.rm =TRUE ),digits=0)
    if (sum("Digital Innovation"==input$Offre)==0) {total <- 0}
    valueBox(
      paste0(total, " k€"), "Digital Innovation", icon = icon("money"),
      color = "purple"
    )
  })
  
  output$nbBox <- renderValueBox({
    data <- pilotage_data %>% 
      filter(WEEK==max(WEEK),STEP %in% c("4 - Gagnée"), ASSOCIE %in% input$Associe, OFFRE_PRINCIPALE %in% input$Offre)
    valueBox(
      nrow(data), "Nb de missions",icon = icon("briefcase", lib = "glyphicon"),
      color = "yellow"
    )
  })
  
  output$TotalDATABox <- renderValueBox({
    data <- pilotage_data %>% 
      filter(WEEK==max(WEEK),STEP %in% c("4 - Gagnée"), ASSOCIE %in% input$Associe, OFFRE_PRINCIPALE %in% c("Data")) 
    total <-   round(sum(data$TOTAL_CA_VENDU_N__KE,na.rm =TRUE ),digits=0)
    valueBox(
      paste0(total, " k€"), "Data", icon = icon("money"),
      color = "purple"
    )
  })
  output$TotalSECURITEBox <- renderValueBox({
    data <- pilotage_data %>% 
      filter(WEEK==max(WEEK),STEP %in% c("4 - Gagnée"), ASSOCIE %in% input$Associe, OFFRE_PRINCIPALE %in% c("Sécurité")) 
    total <-   round(sum(data$TOTAL_CA_VENDU_N__KE,na.rm =TRUE ),digits=0)
    if (sum("Sécurité"==input$Offre)==0) {total <- 0}
    valueBox(
      paste0(total, " k€"), "Sécurité", icon = icon("money"),
      color = "purple"
    )
  })
  
  output$info <- renderPrint({
    data <- pilotage_data %>% 
      filter(STEP %in% c("4 - Gagnée"), ASSOCIE %in% input$Associe, OFFRE_PRINCIPALE %in% input$Offre) %>% 
      mutate(GROUPE=toupper(GROUPE)) %>%
      group_by(OFFRE_PRINCIPALE, WEEK) %>%
      summarise(Budget= round(sum(TOTAL_CA_VENDU_N__KE,na.rm =TRUE ),digits=0))
    brushedPoints(data, input$plot_brush)
  })
  
  output$plot_brushed_points <- DT::renderDataTable({
    dat <- pilotage_data %>% 
      filter(WEEK==max(WEEK),STEP %in% c("4 - Gagnée"), ASSOCIE %in% input$Associe, OFFRE_PRINCIPALE %in% input$Offre) %>% 
      mutate(COMPTE=toupper(COMPTE)) %>%
      select(D_GAIN,TOTAL_CA_VENDU_N__KE,OFFRE_PRINCIPALE,COMPTE,SUJET) 

      res <- brushedPoints(dat, input$plot_brush) %>% mutate(D_GAIN=format(as.Date(D_GAIN ), "%d/%m/%Y") )
    
    datatable(res,options = list(
      
      dom = 'Bfrtip', buttons = list('colvis','print',list(extend='collection',text='Download',buttons = list('copy','csv','excel','pdf'))),
      
      pageLength = 10
      
      
    ))  %>%
      formatRound('TOTAL_CA_VENDU_N__KE', digits = 0) %>%
      
      
      formatStyle('TOTAL_CA_VENDU_N__KE',
                  background = styleColorBar(res$TOTAL_CA_VENDU_N__KE,'lightblue'),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center') %>%
      formatStyle(
        'OFFRE_PRINCIPALE',
        #transform = 'rotateX(45deg) rotateY(20deg) rotateZ(30deg)',
        backgroundColor = styleEqual(names(couleurs),
                                     couleurs
        ))
  })
  
})

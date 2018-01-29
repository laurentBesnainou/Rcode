
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

server <- function(input, output) {
  
  #on met en place une reactiveValues pour permettre la réactivation de DT losrque l'on charge un fichier XLS ou que l'on deande une nouvelle
  #verion du fichier Google de suivi
  values <- reactiveValues()
  #On initialise les différences entre le staffing et google 

  Staff_Actuel <- prepareStaffing1(Staff_Actuel)
  Staff_Actuel2 <- prepareStaffing2(Staff_Actuel)
   Staffing <- Staffing %>% 
    filter(TYPE != 0)  %>%
    select(one_of(My_colonnes))

  resultatDiff <- diff_data(Staffing,Staff_Actuel2)

  comparaison <- as.data.frame(resultatDiff$get_matrix())
  comparaison <- compareStaff(comparaison)
  
  ######################## Initialisation pipe
  #il faut regarder les lignes différentes 
  Pilotage <- Pilotage %>% select(COMPTE,ASSOCIE, SUJET,STEP,CODE_TOTEM,PROB,CA_BT__N__KE)
  Pilotage_Actuel <- preparePipe1(Pilotage_Actuel)
  resultatDiff <- diff_data(Pilotage ,Pilotage_Actuel)
  comparaisonP <- as.data.frame(resultatDiff$get_matrix())
  comparaisonP <- comparePipe(comparaisonP)
  
  values$LogGoogle <- LogGoogle
  values$LogFichier <- LogFichier
  values$comparaisonP <- comparaisonP
  
  ##################################################

  #Vaues Box sur le Staffing & le Pipe
  output$Add <- renderValueBox({
    if (is.null(values$comparaison)){ comp <- comparaison}
    else {comp <- values$comparaison }
    Synthese <- comp %>% filter(!is.na(DELTA)) %>% group_by(Changement) %>% summarise(n())
    valueBox(
      ifelse(nrow(Synthese[Synthese$Changement=="+++",2])==0,0,Synthese[Synthese$Changement=="+++",2]),
      "Ligne ajoutée", icon = icon("plus", lib = "glyphicon"),
      color = "green"
    )
  })
  output$AddP <- renderValueBox({
    
    if (is.null(values$comparaisonP)){ compP <- comparaisonP}
    else {compP <- values$comparaisonP }
    SyntheseP <- compP %>% group_by(Changement) %>% summarise(n())
    valueBox(
      ifelse(nrow(SyntheseP[SyntheseP$Changement=="+++",2])==0,0,SyntheseP[SyntheseP$Changement=="+++",2]),
      "Ligne ajoutée", icon = icon("plus", lib = "glyphicon"),
      color = "green"
    )
  })
    output$Mod <- renderValueBox({
      if (is.null(values$comparaison)){ comp <- comparaison}
      else {comp <- values$comparaison }
      Synthese <- comp %>% filter(!is.na(DELTA)) %>% group_by(Changement) %>% summarise(n())  
    valueBox(
      ifelse(nrow(Synthese[Synthese$Changement=="mod",2])==0,0,Synthese[Synthese$Changement=="mod",2]),
      "Ligne modifiées", icon = icon("pencil", lib = "glyphicon"),
      color = "yellow"
    )
  })
    output$ModP <- renderValueBox({
      if (is.null(values$comparaisonP)){ compP <- comparaisonP}
      else {compP <- values$comparaisonP }
      SyntheseP <- compP %>% group_by(Changement) %>% summarise(n())
      valueBox(
        ifelse(nrow(SyntheseP[SyntheseP$Changement=="mod",2])==0,0,SyntheseP[SyntheseP$Changement=="mod",2]),
        "Ligne modifiées", icon = icon("pencil", lib = "glyphicon"),
        color = "yellow"
      )
    })
  output$Del <- renderValueBox({
    if (is.null(values$comparaison)){ comp <- comparaison}
    else {comp <- values$comparaison }
    Synthese <- comp %>% group_by(Changement) %>% summarise(n())
    valueBox(
      ifelse(nrow(Synthese[Synthese$Changement=="---",2])==0,0,Synthese[Synthese$Changement=="---",2]), 
      "Ligne supprimées", icon = icon("minus", lib = "glyphicon"),
      color = "red"
    )
  })
  output$DelP <- renderValueBox({
    if (is.null(values$comparaisonP)){ compP <- comparaisonP}
    else {compP <- values$comparaisonP }
    SyntheseP <- compP %>% group_by(Changement) %>% summarise(n())
    valueBox(
      ifelse(nrow(SyntheseP[SyntheseP$Changement=="---",2])==0,0,SyntheseP[SyntheseP$Changement=="---",2]), 
      "Ligne supprimées", icon = icon("minus", lib = "glyphicon"),
      color = "red"
    )
  })
  
  output$Add_TOTAL <- renderValueBox({
    if (is.null(values$comparaison)){ comp <- comparaison}
    else {comp <- values$comparaison }
    Synthese_TOTAL <- comp  %>% group_by(Changement)  %>%  filter(!is.na(DELTA)) %>% summarise(TOTAL = sum(DELTA,na.rm = TRUE))
    valueBox(
      ifelse(nrow(Synthese_TOTAL[Synthese_TOTAL$Changement=="+++",2])==0,0,Synthese_TOTAL[Synthese_TOTAL$Changement=="+++",2]),
      "Jours homme ajoutés", icon = icon("plus", lib = "glyphicon"),
      color = "green"
    )
  })
  output$Add_TOTALP <- renderValueBox({

    if (is.null(values$comparaisonP)){ compP <- comparaisonP}
    else {compP <- values$comparaisonP }
    Synthese_TOTALP <- compP  %>% group_by(Changement)  %>% summarise(TOTAL = sum(DELTA,na.rm = FALSE))
    valueBox(
      ifelse(nrow(Synthese_TOTALP[Synthese_TOTALP$Changement=="+++",2])==0,0,Synthese_TOTALP[Synthese_TOTALP$Changement=="+++",2]),
      "Jours homme ajoutés", icon = icon("plus", lib = "glyphicon"),
      color = "green"
    )
  })
  output$Mod_TOTAL <- renderValueBox({
    if (is.null(values$comparaison)){ comp <- comparaison}
    else {comp <- values$comparaison }
    Synthese_TOTAL <- comp  %>% group_by(Changement)  %>% summarise(TOTAL = sum(DELTA,na.rm = TRUE))
    valueBox(ifelse(nrow(Synthese_TOTAL[Synthese_TOTAL$Changement=="mod",2])==0,0,Synthese_TOTAL[Synthese_TOTAL$Changement=="mod",2]),
      "Impacts en jours homme", icon = icon("pencil", lib = "glyphicon"),
      color = "yellow"
    )
  })
  output$Mod_TOTALP <- renderValueBox({
    if (is.null(values$comparaisonP)){ compP <- comparaisonP}
    else {compP <- values$comparaisonP }
    dP <- compP  %>% filter (Changement=="mod", grepl("->",CA_BT__N__KE))
    valueBox(sum(dP$DELTA),
             "Impacts en jours homme", icon = icon("pencil", lib = "glyphicon"),
             color = "yellow"
    )
  })
  output$Del_TOTAL <- renderValueBox({
    if (is.null(values$comparaison)){ comp <- comparaison}
    else {comp <- values$comparaison }
    Synthese_TOTAL <- comp  %>% group_by(Changement)  %>% summarise(TOTAL = sum(DELTA,na.rm = TRUE))
    valueBox(
      ifelse(nrow(Synthese_TOTAL[Synthese_TOTAL$Changement=="---",2])==0,0,Synthese_TOTAL[Synthese_TOTAL$Changement=="---",2]), 
      "Jour/hommme supprimés", icon = icon("minus", lib = "glyphicon"),
      color = "red"
    )
  })
  output$Del_TOTALP <- renderValueBox({
    if (is.null(values$comparaisonP)){ compP <- comparaisonP}
    else {compP <- values$comparaisonP }
    Synthese_TOTALP <- compP  %>% group_by(Changement)  %>% summarise(TOTAL = sum(DELTA,na.rm = FALSE))
    valueBox(
      ifelse(nrow(Synthese_TOTALP[Synthese_TOTALP$Changement=="---",2])==0,0,Synthese_TOTALP[Synthese_TOTALP$Changement=="---",2]), 
      "Jour/hommme supprimés", icon = icon("minus", lib = "glyphicon"),
      color = "red"
    )
  })
  # Graphes sur les écarts entre la version de référence et le fichier Google
  output$Staffing_plot <- renderPlot({

    if (is.null(values$Staffing)) {Staff <- Staffing } 
    else {Staff <- values$Staffing }
    if (is.null(values$Staff_Actuel2)) {Staff2 <- Staff_Actuel2} 
    else {Staff2 <- values$Staff_Actuel2}

    
    plotOld <- Staff %>% filter(TYPE !=0)  %>% gather(mois, nb,5:6) %>%
      group_by(TYPE,mois) %>% 
      summarise(DELTA = sum(nb, na.rm = TRUE)) %>%
      mutate(Semaine = "Précédente") 
    
    plotNew <- Staff2 %>% filter(TYPE !=0) %>% gather(mois, nb,5:6) %>%
      group_by(TYPE,mois) %>%
      summarise(DELTA = sum(nb, na.rm = TRUE)) %>%
      mutate(Semaine = "Actuelle") 

    plot_final <- left_join(plotOld, plotNew, by = c("TYPE","mois")) %>% 
      mutate(Ecarts = DELTA.y - DELTA.x ) %>% ungroup () %>% mutate(TYPE=factor(TYPE)) 
    #on remplace les 1 à 7 par les bons libellés
    plot_final$TYPE <- remplace_Type(plot_final$TYPE)
    plot_final %>%
    ggplot(aes(x = TYPE,
               y = Ecarts, fill=TYPE))+
      geom_bar(stat = "identity")+
      ggtitle("Ecarts par type de ligne du staffing")+
      scale_x_discrete(name="TYPE") + scale_fill_discrete(name = "Type de ligne") + theme(legend.position="bottom") +
      facet_grid(mois ~ .)
    
  })
  
  output$Pipe_plot <- renderPlot({
    
    if (is.null(values$Pilotage)){ Pil <- Pilotage}
    else {Pil <- values$Pilotage }
    plotOld <- Pil %>% 
      group_by(STEP) %>%
      summarise(TOTAL = sum(CA_BT__N__KE, na.rm = TRUE)) %>%
      mutate(Semaine = "Précédente") 
    if (is.null(values$Pilotage_Actuel)){ PilA <- Pilotage_Actuel}
    else {PilA <- values$Pilotage_Actuel }
    plotNew <- PilA %>% mutate(CA_BT__N__KE = as.numeric(CA_BT__N__KE)) %>%
      group_by(STEP) %>%
      summarise(TOTAL = sum(CA_BT__N__KE, na.rm = TRUE)) %>%
      mutate(Semaine = "Actuelle") 
    
    plot_final <- left_join(plotOld, plotNew, by = c("STEP")) %>% 
      mutate(Ecarts = TOTAL.y - TOTAL.x )  
    
    plot_final %>%
      ggplot(aes(x = STEP,
                 y = Ecarts, fill=STEP))+
      geom_bar(stat = "identity")+
      ggtitle("Ecarts par type de ligne du Pipe")+
      scale_x_discrete(name="STEP")+
      theme_wsj() + scale_fill_discrete(name = "Type de ligne") + theme(legend.position="bottom")
  }) 
  
   output$contents <- renderTable({
     values$Staffing
   })
  
  output$contents2 <- renderTable({
    values$Pilotage
  })
  
  StaffTableau <- reactive ({
    
    #on va regarder par personne le nb de jours par mois 
    #si values$Staffing est null cela signifie que l'on pas importé de nouveau Dataframe
    if (is.null(values$Staffing)) {Staff <- Staffing } 
        else {Staff <- values$Staffing }
    if (is.null(values$Staff_Actuel2)) {Staff2 <- Staff_Actuel2} 
    else {Staff2 <- values$Staff_Actuel2}

    resultatDiff <- diff_data(Staff,Staff2)
    comparaison <- as.data.frame(resultatDiff$get_matrix())
    
    comparaison <- compareStaff(comparaison)
   
    Synthese <- comparaison %>% group_by(Changement) %>% summarise(n())
    Synthese_TOTAL <- comparaison  %>% group_by(Changement)  %>% summarise(TOTAL = sum(DELTA,na.rm = TRUE))
    
    comparaison <- comparaison %>% 
      mutate(TYPE= map_chr(clean_split(as.character(TYPE)), last_elem) ) %>%
      filter(TYPE %in% input$Staff, Changement %in% input$uiModifEcart, DELTA !=0)
    
    # Write the first data set in a new workbook
    write.xlsx(comparaison, file = "Staff.xlsx",
               sheetName = "USA-ARRESTS", append = FALSE)
   
    comparaison <-comparaison %>% 
      mutate(MISSIONS=ifelse(grepl("->",MISSIONS),paste("<font color=#BC0E1F>",MISSIONS,"</font>"),paste(MISSIONS,""))) %>%
      mutate(ID_TOTEM=ifelse(grepl("->",ID_TOTEM),paste("<font color=#BC0E1F>",ID_TOTEM,"</font>"),paste(ID_TOTEM,"")))
    datatable(comparaison, 
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
  
## Permet de dowloader le fichier XLSX
  
  output$downloadData <- downloadHandler(
    filename <- function() {
      paste("staff", "xlsx", sep=".")
    },
    
    content <- function(file) {
      file.copy("Staff.xlsx", file)
    },
    contentType = "application/vnd.ms-excel"
  )
  
  
#Ce code controle si on utilise le bouton permettant d'importer un fichier de Staffing, il met a jour la donnée reactivevalue Staffing et stock le nouveau
# Dataframe au format RData
observeEvent(input$Staffing
               
               , {
                 
                 inFile <- input$Staffing
                 if (is.null(inFile)) return(NULL)
                 file.copy(inFile$datapath,
                           "Staff.xlsx",overwrite = TRUE)
                 Staffing <-read_excel("Staff.xlsx", 1)
                 colnames(Staffing) <- format_col_names(colnames(Staffing))
                    Staffing <- Staffing %>% 
                      filter(TYPE != 0)  %>% 
                      select(one_of(My_colonnes))
                    save(Staffing,file="data/staffing2017.RData")
                    
                    resultatDiff <- diff_data(Staffing,Staff_Actuel2)
                    comparaison <- as.data.frame(resultatDiff$get_matrix())
                    
                    comparaison <- compareStaff(comparaison)
                    Synthese <- comparaison %>% filter(!is.na(DELTA)) %>% group_by(Changement) %>% summarise(n())
                    #Synthese_TOTAL <- comparaison  %>% group_by(Changement)  %>% summarise(TOTAL = sum(DELTA,na.rm = TRUE))
                    #d <-  comparaison  %>% filter (V1=="mod", grepl("->",V6)) 
                    #colnames(comparaison)  <-  c('Changement', 'Consultant', 'Type', 'Mission', 'TOTEM', 'Total',"Semaine prev", 'Semaine','DELTA')
                   #comparaison <- comparaison %>% filter(Type %in% input$Staff, Changement %in% input$uiModifEcart, DELTA !=0)
                    
                    #comparaison <-comparaison %>% 
                      # mutate(Mission=ifelse(grepl("->",Mission),paste("<font color=#BC0E1F>",Mission,"</font>"),paste(Mission,""))) %>%
                      # mutate(TOTEM=ifelse(grepl("->",TOTEM),paste("<font color=#BC0E1F>",TOTEM,"</font>"),paste(TOTEM,""))) %>%
                      # mutate(Total=ifelse(grepl("->",Total),paste("<font color=#BC0E1F>",Total,"</font>"),paste(Total,""))) 
                    log <- data.frame(DATE=as.character(Sys.time()),TYPE="Staffing",LIGNE=inFile$name)
                    LogFichier  <- rbind(LogFichier , log)
                   
                    save(LogFichier ,file="data/LogFichier.RData")
                    values$LogFichier <- LogFichier  
                 values$comparaison <- comparaison   
                 values$Staffing <- Staffing
  })


observeEvent(input$Pipe
             
             , {
               inFile <- input$Pipe
               if (is.null(inFile)) return(NULL)
               file.copy(inFile$datapath,
                         "Pipe.xlsx",overwrite = TRUE)
               Pilotage <-read_excel("Pipe.xlsx", 1)
               colnames(Pilotage) <- format_col_names(colnames(Pilotage))
               Pilotage <- Pilotage %>% select(COMPTE,ASSOCIE, SUJET,STEP,CODE_TOTEM,PROB,CA_BT__N__KE)
               save(Pilotage,file="data/Pilotage.RData")
               log <- data.frame(DATE=as.character(Sys.time()),TYPE="Pipe",LIGNE=inFile$name)
               LogFichier  <- rbind(LogFichier , log)
               #LogGoogle <- data.frame(DATE=character(),TYPE=character(),LIGNE=character(),stringsAsFactors=FALSE)
               save(LogFichier ,file="data/LogFichier.RData")
               values$LogFichier <- LogFichier 
              
               resultatDiff <- diff_data(Pilotage ,Pilotage_Actuel)
               comparaisonP <- as.data.frame(resultatDiff$get_matrix())
               comparaisonP <- comparePipe(comparaisonP)
               values$comparaisonP <- comparaisonP  
               values$Pilotage <- Pilotage   
               
             })

observeEvent(input$go
             
             , {
               if (input$go == 0) return(NULL)
               # Create a Progress object
               progress <- shiny::Progress$new(style = 'notification')
               progress$set(message = "Chargement des données", value = 0)
               # On charge la feuille google dans un dataframe (skip permet de passer les 15 premieres lignes)
               Staff_Actuel <- gs_read(ss=BT_Pilotage, ws = "Staffing", skip=15)
               #on va suprimer touts les colonnes X en fin de dataframe
               progress$set(message = "Formatage des données", value = 30)
               Sys.sleep(2)
               
               #on va comparer les differences entre le staffing et le Staff_Actuel
               Staffing <- Staffing %>% 
                 filter(TYPE != 0)  %>% 
                 select(CONSULTANTS,TYPE,MISSIONS,ID_TOTEM,TOTAL)
               Staff_Actuel <- prepareStaffing1(Staff_Actuel)
               Staff_Actuel2 <- prepareStaffing2(Staff_Actuel)
               
               #il faut regarder les lignes différentes 
               progress$set(message = "Comparaison avec semaine passée", value = 80)
               Sys.sleep(0.5)
               resultatDiff <- diff_data(Staffing,Staff_Actuel2)
               comparaison <- as.data.frame(resultatDiff$get_matrix())
               comparaison <- compareStaff(comparaison)
           
               #on va compter le nombre de modifs
               progress$set(message = "Agrégation des données", value = 100)
               Sys.sleep(0.5)
               
               progress$set(message = "Chargement des données Terminé", value = 100)
               on.exit(progress$close())
               HTML(paste("hello", "Traitement Terminé",  sep="\n"))
              
               #on ajoute une ligne à la log google
               log <- data.frame(DATE=as.character(Sys.time()),TYPE="Staffing",LIGNE=paste("Nb lignes: ",LIGNE=nrow(Staff_Actuel2)))
               LogGoogle  <- rbind(LogGoogle , log)
               #LogGoogle <- data.frame(DATE=character(),TYPE=character(),LIGNE=character(),stringsAsFactors=FALSE)
               save(LogGoogle ,file="data/LogGoogle.RData")
               values$LogGoogle <- LogGoogle  

               values$comparaison <- comparaison   
               values$Staff_Actuel2 <- Staff_Actuel2
             })

  #Tableau de comparaison
  output$DTSTAFF = DT::renderDataTable( StaffTableau())
  #Tableau qui affiche le staffing contenu dans le fichier Excel
  output$DTSTAFFING = DT::renderDataTable( values$Staffing )
  
  PipeTableau <- reactive ({

    #on va regarder par personne le nb de jours par mois 
    #si values$Staffing est null cela signifie que l'on pas importé de nouveau Dataframe
    if (is.null(values$Pilotage)){ Pil <- Pilotage}
      else {Pil <- values$Pilotage }
    if (is.null(values$Pilotage_Actuel)){Pil_Act <- Pilotage_Actuel}
      else {Pil_Act <- values$Pilotage_Actuel }
    
    #il faut regarder les lignes différentes 
    Pilotage <- Pil %>% select(COMPTE,ASSOCIE, SUJET,STEP,CODE_TOTEM,PROB,CA_BT__N__KE)
    #Pilotage_Actuel <- preparePipe1(values$Pilotage_Actuel)
    resultatDiff <- diff_data(Pil ,Pil_Act)
    comparaisonP <- as.data.frame(resultatDiff$get_matrix())
    
    #on conserve les lignes en écarts
    comparaisonP <- comparePipe(comparaisonP)
    SyntheseP <- comparaisonP %>% group_by(Changement) %>% summarise(n())
    
    comparaisonP <- comparaisonP %>% filter (Changement %in% input$EcartP)
    #| 
     #                                          (ifelse(abs(as.numeric(DELTA)) >=input$dynamicDELTA[1] & abs(as.numeric(DELTA)) <= input$dynamicDELTA[2]))
    
    comparaisonP <- comparaisonP %>% mutate(VariationCA=ifelse(as.numeric(CA),as.numeric(CA)/as.numeric(CA_Prev)-1,0)) %>%
      mutate(VariationCA = round(VariationCA*100,2))
    #o	Evolution des statuts perdus : ne pas prendre en compte les changements si le statut avant ET le statut après sont 5-, 6- ou 7- 
    # ou si création directement en statut 5-, 6-, 7
    comparaisonP <- comparaisonP %>% filter(!(STEP %in% c("5 - No Follow","6 - En Sommeil"," 7 - Perdue")&
                                              STEP_Prev %in% c("5 - No Follow","6 - En Sommeil"," 7 - Perdue"))) %>%
      
      mutate(CA = round(as.numeric(CA),1)) %>%
      mutate(CA_Prev = round(as.numeric(CA_Prev),1)) %>% 
      mutate(DELTA = round(as.numeric(DELTA),1)) %>%
      mutate(PROBA= map_chr(PROBA,clean_split_pourcentage)) %>%
      mutate(PROB= map_chr(PROB,clean_split_pourcentage))
      #on va regarder toutes les lignes "mod" qui ont un Detla en valeur absolue supérieur à 1 k€
    compPMod <- comparaisonP %>% filter(Changement == "mod", abs(DELTA) > input$numDELTA)
    compAutre <- comparaisonP %>% filter(Changement %in% c("+++","---"))
    comparaisonP <- rbind(compPMod,compAutre)
    comparaisonP <- comparaisonP %>% arrange(COMPTE,SUJET)
    
    
    datatable(comparaisonP, 
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
  
  output$DTPIPE = DT::renderDataTable( PipeTableau())
  
  output$LogPIPE = DT::renderDataTable( 
   
    if (is.null(values$LogGoogle)){LogGoogle %>% arrange(desc(DATE))} 
    else {values$LogGoogle %>% arrange(desc(DATE))}
    
    )
  
  output$LogSTAFF = DT::renderDataTable( 
    
    if (is.null(values$LogFichier)){LogFichier  %>% arrange(desc(DATE))} 
    else {values$LogFichier  %>% arrange(desc(DATE))}
    
  )
  
  observeEvent(input$goP
               
               , {
                 if (input$goP == 0) return(NULL)
                 # Create a Progress object
                 progress <- shiny::Progress$new(style = 'notification')
                 progress$set(message = "Chargement des données PIPE Google", value = 0)
                
                 Pilotage_Actuel <- gs_read(ss=BT_Pilotage, ws = "Pipe", skip=4)
                 
                 progress$set(message = "Formatage des données", value = 30)
                 Sys.sleep(2)
                 Pilotage_Actuel <- preparePipe1(Pilotage_Actuel)
                 Pilotage <- Pilotage %>% select(COMPTE,ASSOCIE, SUJET,STEP,CODE_TOTEM,PROB,CA_BT__N__KE) 
                 #il faut regarder les lignes différentes 
                 
                 #il faut regarder les lignes différentes 
                 progress$set(message = "Comparaison avec semaine passée", value = 80)
                 resultatDiff <- diff_data(Pilotage,Pilotage_Actuel)
                 comparaisonP <- as.data.frame(resultatDiff$get_matrix())
                 Sys.sleep(3)
                 #on conserve les lignes en écarts
                 progress$set(message = "Agrégation des données", value = 100)
                 Sys.sleep(0.5)
                 comparaisonP <- comparePipe(comparaisonP)
                 progress$set(message = "Chargement des données", value = 100)
                 on.exit(progress$close())
                 #on ajoute une ligne à la log google
               
                 log <- data.frame(DATE=as.character(Sys.time()),TYPE="Pipe",LIGNE=paste("Nb lignes: ",LIGNE=nrow(Pilotage_Actuel)))
                 LogGoogle  <- rbind(LogGoogle , log)
                 #LogGoogle <- data.frame(DATE=character(),TYPE=character(),LIGNE=character(),stringsAsFactors=FALSE)
                 print(log)
                 save(LogGoogle ,file="data/LogGoogle.RData")
                 values$LogGoogle <- LogGoogle  
                 values$comparaisonP <- comparaisonP   
                 values$Pilotage_Actuel <- Pilotage_Actuel
               })
  output$ui <- renderUI({
    
    minDelta <- round(abs(as.numeric(min(comparaisonP$DELTA,na.rm = TRUE))),0)
    maxDelta <- round(abs(as.numeric(max(comparaisonP$DELTA,na.rm = TRUE))),0)
    max <- max(minDelta,maxDelta)
    sliderInput("dynamicDELTA", "Filtre sur le Delta",
                min = 0, max = max, value=c(0,max))
    
  })
  
  output$PipeText <- renderUI({
    
    if (is.null(values$LogFichier)) {

      str1 <- paste("Données de référence importées le : ", tail(LogFichier[LogFichier$TYPE=="Pipe",],1)[1,1],
            " Fichier:", tail(LogFichier[LogFichier$TYPE=="Pipe",],1)[1,3] )
    } else {

      str1 <- paste("Données de référence importées le : ", tail(values$LogFichier[values$LogFichier$TYPE=="Pipe",],1)[1,1],
            " Fichier:", tail(values$LogFichier[values$LogFichier$TYPE=="Pipe",],1)[1,3])
    }
    if (is.null(values$LogGoogle)) {
      
      str2 <- paste("Données de référence importées le : ", tail(LogGoogle[LogGoogle$TYPE=="Pipe",],1)[1,1],
                   " Fichier:", tail(LogGoogle[LogGoogle$TYPE=="Pipe",],1)[1,3] )
    } else {
      
      str2 <- paste("Importation Onglet Pipe Google  : ", tail(values$LogGoogle[values$LogGoogle$TYPE=="Pipe",],1)[1,1],
                   " Fichier:", tail(values$LogGoogle[values$LogGoogle$TYPE=="Pipe",],1)[1,3])
    }
    HTML(paste("<DIV>",paste(str1, str2, sep = '<br/>'),"</DIV>"))
    
    })
  
  output$StaffText <- renderUI({
    if (is.null(values$LogFichier)) {
      
      str1 <- paste("Données de référence importées le : ", tail(LogFichier[LogFichier$TYPE=="Staffing",],1)[1,1],
                    " Fichier:", tail(LogFichier[LogFichier$TYPE=="Staffing",],1)[1,3] )
    } else {
      
      str1 <- paste("Données de référence importées le : ", tail(values$LogFichier[values$LogFichier$TYPE=="Staffing",],1)[1,1],
                    " Fichier:", tail(values$LogFichier[values$LogFichier$TYPE=="Staffing",],1)[1,3])
    }
    if (is.null(values$LogGoogle)) {
      
      str2 <- paste("Données de référence importées le : ", tail(LogGoogle[LogGoogle$TYPE=="Staffing",],1)[1,1],
                    " Fichier:", tail(LogGoogle[LogGoogle$TYPE=="Pipe",],1)[1,3] )
    } else {
      
      str2 <- paste("Importation Google de référence  : ", tail(values$LogGoogle[values$LogGoogle$TYPE=="Staffing",],1)[1,1],
                    " Fichier:", tail(values$LogGoogle[values$LogGoogle$TYPE=="Pipe",],1)[1,3])
    }
    HTML(paste("<DIV>",paste(str1, str2, sep = '<br/>'),"</DIV>"))
  })
}

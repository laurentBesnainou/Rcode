server <- function(input, output, session) {
  
  output$Pipe_Repartition <- renderPlot({

    
    data <- pipe %>% 
      filter(WEEK==max(pipe$WEEK),STEP %in% input$uiStep1, ASSOCIE %in% input$uiAssocie, OFFRE_PRINCIPALE %in% input$uiOffre) %>% 
      group_by(OFFRE_PRINCIPALE) %>%
      summarise(Budget= round(sum(TOTAL_CA_VENDU_N__KE,na.rm =TRUE ),digits=0))
    
    ggplot(data,aes(x=OFFRE_PRINCIPALE,y=Budget)) + 
      geom_bar(stat = "identity",
               aes(fill = OFFRE_PRINCIPALE ))+
      geom_text(aes(label = Budget),size = 5,  position=position_dodge(width=0.9), vjust=-0.2)+
      theme_economist() + scale_colour_economist() + 
      scale_fill_manual(values=couleurs) +
      guides(color=guide_legend("my title"))
   
    
  })
  
  output$Pipe_Clients <- renderPlot({

    data <- pipe %>%
      filter(WEEK==max(pipe$WEEK), STEP %in% input$uiStep1, ASSOCIE %in% input$uiAssocie, OFFRE_PRINCIPALE %in% input$uiOffre) %>% 
      group_by(OFFRE_PRINCIPALE,COMPTE) %>%
      summarise(Budget= round(sum(TOTAL_CA_VENDU_N__KE,na.rm =TRUE ),digits=0))
    
    
    
      
    data %>% ggplot(aes(x=COMPTE, y=Budget)) +geom_hline(yintercept = 100, size = 0.5, color="red") +
      
      geom_polygon(aes(group = OFFRE_PRINCIPALE, color = OFFRE_PRINCIPALE, fill=OFFRE_PRINCIPALE),  alpha=0.2,size = 1) +
      geom_point( aes(colour=OFFRE_PRINCIPALE,fill=OFFRE_PRINCIPALE),shape=18, size=2)+

      coord_polar()  +
      theme_bw() + 
      scale_color_manual(values=couleurs) +
      ggtitle("Propositions à émettre") +
      theme(legend.position="bottom",
            legend.box="horizontal",axis.text.x = 
              element_text(
                vjust=50,
                angle=-90 - 360 / length(unique(data$COMPTE)) * seq_along(data$COMPTE))) 

  })
  
  
  output$Pipe_Associe <- renderPlot({
    
    data <- pipe %>%
      filter(WEEK==max(pipe$WEEK), STEP %in% input$uiStep1, ASSOCIE %in% input$uiAssocie, OFFRE_PRINCIPALE %in% input$uiOffre) %>% 
      group_by(ASSOCIE, COMPTE) %>%
      summarise(Budget= round(sum(TOTAL_CA_VENDU_N__KE,na.rm =TRUE ),digits=0))
    
    
    
    
    data %>% ggplot(aes(x=COMPTE, y=Budget)) +geom_hline(yintercept = 100, size = 0.5, color="red") +
      
      geom_polygon(aes(group = ASSOCIE, color = ASSOCIE, fill=ASSOCIE),  alpha=0.2,size = 1) +
      geom_point( aes(colour=ASSOCIE,fill=ASSOCIE),shape=18, size=2)+
      
      coord_polar()  +
      theme_bw() + 
      scale_fill_manual(values=couleurs) +
      ggtitle("Propositions à émettre") +
      theme(legend.position="bottom",
            legend.box="horizontal",axis.text.x = 
              element_text(
                vjust=50,
                angle=-90 - 360 / length(unique(data$COMPTE)) * seq_along(data$COMPTE))) 
    
    
    
    
  })
  
  
  output$Offre_Repartition <- renderPlot({
    
    
    data <- pipe %>% 
      filter(WEEK==max(pipe$WEEK),STEP %in% input$uiStep1, ASSOCIE %in% input$uiAssocie, OFFRE_PRINCIPALE %in% input$uiOffre) %>% 
      group_by(OFFRE_PRINCIPALE) %>%
      summarise(Budget= round(sum(TOTAL_CA_VENDU_N__KE,na.rm =TRUE ),digits=0))
    
    ggplot(data,aes(x=OFFRE_PRINCIPALE,y=Budget)) + 
      geom_bar(stat = "identity",
               aes(fill = OFFRE_PRINCIPALE ))+
      geom_text(aes(label = Budget),size = 5,  position=position_dodge(width=0.9), vjust=-0.2)+
      theme_economist() + scale_colour_economist() +
      scale_fill_manual(values=couleurs) +
      guides(color=guide_legend("my title"))
    
    
  })
  
  output$Pipe_Step <- renderPlot({
    
    
    data <- pipe %>% 
      filter(WEEK==max(pipe$WEEK),STEP %in% input$uiStep1, ASSOCIE %in% input$uiAssocie, OFFRE_PRINCIPALE %in% input$uiOffre) %>% 
      group_by(STEP,OFFRE_PRINCIPALE) %>%
      summarise(Budget= round(sum(TOTAL_CA_VENDU_N__KE,na.rm =TRUE ),digits=0)) %>%
      mutate(countT= sum(Budget)) %>%
      group_by(STEP,OFFRE_PRINCIPALE, add=TRUE) %>% 
      mutate(per=round(100*Budget/countT,2))

    
    
    ggplot(data,aes(x=STEP,y=Budget,label = Budget,group = STEP)) + 
      geom_bar(stat = "identity",
               aes(fill = OFFRE_PRINCIPALE ))+
      geom_text(aes(label = Budget, y = Budget),position="stack",size = 5,   vjust=0.9)+
      theme_economist() + scale_colour_economist() +
      scale_fill_manual(values=couleurs) +
      
      
      guides(color=guide_legend("my title"))
    
    
  })
  
  output$TreeMAP <-  renderPlot({
    dt <- pipe %>% 
      filter(WEEK==max(pipe$WEEK),STEP %in% input$uiStep1, ASSOCIE %in% input$uiAssocie, OFFRE_PRINCIPALE %in% input$uiOffre) %>% 
      group_by(OFFRE_PRINCIPALE,COMPTE) %>%
      summarise(Budget= round(sum(TOTAL_CA_VENDU_N__KE,na.rm =TRUE ),digits=0))
        
    # Create data
    group=dt$OFFRE_PRINCIPALE
    subgroup = dt$COMPTE
    CA=dt$Budget
    data=data.frame(group,subgroup,CA)
    
    # treemap
    treemap(data,
            index=c("group","subgroup"),
            vSize="CA",
            vColor=  "CA", 
            align.labels=list(
              c("left", "top"), 
              c("right", "bottom"),
              inflate.labels=T
            ) ,   
            title.legend="Montant en k€ par compte",
            type="dens",
            palette="RdYlGn",
            range=c(0,100), # this is shown in the legend
            mapping=c(0, 30,  100) # Rd is mapped to -30k, Yl to 10k, and Gn to 40k
    )
  })
  
  output$TreeMAPAss <-  renderPlot({
    dt <- pipe %>% 
      filter(WEEK==max(pipe$WEEK),STEP %in% input$uiStep1, ASSOCIE %in% input$uiAssocie, OFFRE_PRINCIPALE %in% input$uiOffre) %>% 
      group_by(ASSOCIE,OFFRE_PRINCIPALE,COMPTE, SUJET) %>%
      summarise(Budget= round(sum(TOTAL_CA_VENDU_N__KE,na.rm =TRUE ),digits=0))
    
    # Create data
    Associe=dt$ASSOCIE
    Compte = paste(dt$COMPTE, dt$SUJET, sep=">")
    CA=dt$Budget
    Offre=dt$OFFRE_PRINCIPALE
    data=data.frame(Associe,Compte,CA,Offre)
    
    # treemap
    # treemap(data,
    #         index=c("Associe", "Compte"),
    #         vSize="CA",
    #         align.labels=list(
    #           c("left", "top"), 
    #           c("right", "bottom"),
    #           inflate.labels=T
    #         ) ,          
    #         type="index"   
    #         )
   
    treemap(data,
            index=c("Associe", "Compte"),
            vSize="CA",
            vColor=  "CA",     
            align.labels=list(
            c("left", "top"), 
            c("right", "bottom"),
            inflate.labels=T
                   ) ,      
            title.legend="Montant en k€ par proposition",
            type="dens",
            palette="RdYlGn",
            range=c(0,100), # this is shown in the legend
            mapping=c(0, 30,  100) # Rd is mapped to -30k, Yl to 10k, and Gn to 40k
    )
    
  })
  
  output$HeatMap <-  renderPlot({
    base_size <- 9
    dt <- pipe %>% 
      filter(WEEK==max(pipe$WEEK),STEP %in% input$uiStep1, ASSOCIE %in% input$uiAssocie, OFFRE_PRINCIPALE %in% input$uiOffre) %>% 
      group_by(ASSOCIE,OFFRE_PRINCIPALE,COMPTE, SUJET) %>%
      summarise(Budget= round(sum(TOTAL_CA_VENDU_N__KE,na.rm =TRUE ),digits=0))
  ggplot(data = dt, aes(x=ASSOCIE, y=COMPTE)) + 
    geom_tile( aes(fill=Budget),colour = "grey50") + 
    scale_fill_gradient(low = "white", high = "green")+
    theme_grey(base_size = base_size) +
    labs(x = "", y = "") + scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0))+
    theme(panel.background = element_rect(fill = "white", colour = "black"),
          panel.grid.major.y = element_line(colour = "black"),
          panel.grid.minor.y = element_line(colour = "black"))
    
  })
  ########### sunburst
  combo_output <- function(){
    seq_Asso <- pipe %>%
      filter(WEEK==max(pipe$WEEK)) %>% 
      group_by(GROUPE, OFFRE_PRINCIPALE,ASSOCIE,SECTEUR,SUJET, STEP) %>%
      summarize(CA = sum(TOTAL_CA_VENDU_N__KE,na.rm=TRUE)) %>% mutate (DEBUT = "Pipe BT")
    
    seq_Asso$GROUPE[is.na(seq_Asso$GROUPE)] <- "Non Renseigné"
    seq_Asso$OFFRE_PRINCIPALE[is.na(seq_Asso$OFFRE_PRINCIPALE)] <- "Non Renseigné"
    seq_Asso$ASSOCIE[is.na(seq_Asso$ASSOCIE)] <- "Non Renseigné"
    seq_Asso$SECTEUR[is.na(seq_Asso$SECTEUR)] <- "Non Renseigné"
    seq_Asso$SUJET[is.na(seq_Asso$SUJET)] <- "Non Renseigné"

    nom <- list()
    nb_1 <- length(seq_Asso$ASSOCIE)
    
    for (i in 1:nb_1) {
      nom[[i]] <- c(seq_Asso$DEBUT[i], seq_Asso$STEP[i],  seq_Asso$OFFRE_PRINCIPALE[i],seq_Asso$ASSOCIE[i],seq_Asso$GROUPE[i],
                    seq_Asso$SUJET[i],seq_Asso$CA[i])
      
    }
    CA_TOTAL <- c(seq_Asso$CA)

    combo_output <- list(path = nom, value = CA_TOTAL)
  }
  
  #output$D3PartTree <- renderD3partitionR(
   
  #   D3partitionR( data=list(path=combo_output()$path,value=combo_output()$value),type="treeMap")
  #   ,
  #                 tooltipOptions = list(showAbsolutePercent = T, showRelativePercent = T,
  #                 
  #                 # legend=list(type="sequential",no_show = FALSE,
  #                 #             color=list("ORE"="#E589B4", "JSO"="#C9C5E5", "OGR" = "#50E5D6",
  #                 #                        "JPP" = "#99F8FF", "MMO"= "#FF9770", 
  #                 #                        "UHE"="#E9FF70", "Transformation"="#0BA","ETM"="#AA1","Data"="#ECC")), 
  #                 trail = TRUE,
  #                 Input=list(enabled=T,Id="D3PartTreee",clickedStep=T,
  #                            currentPath=T,visiblePaths=T,visibleLeaf=T,visibleNode=T),
  #                 
  #                 width = 600,height = 600)   
  # )
  output$Pipe_Temps <- renderPlot({
 
    data <- pipe %>%
      filter(STEP %in% input$uiStep1, ASSOCIE %in% input$uiAssocie, OFFRE_PRINCIPALE %in% input$uiOffre) %>% 
      group_by(WEEK,OFFRE_PRINCIPALE) %>%
      summarise(Budget= round(sum(TOTAL_CA_VENDU_N__KE,na.rm =TRUE ),digits=0))
    data %>% ggplot(aes(x=WEEK, y=Budget)) +
      geom_bar(stat = "identity",
               aes(fill = OFFRE_PRINCIPALE ))+
      ggtitle("Evolution du pipe dans le temps") +
      theme_economist() + scale_colour_economist() + 
      scale_fill_manual(values=couleurs) + geom_smooth(method = "lm", span = 0.3, formula = y ~ splines::bs(x, 3), se = FALSE, color="red")
    
  })
  
}
server <- function(input, output) { 
  
  output$mon_vote_connaissance <- renderText({
    
    my_connaissance_answer <- questionnaire_connaissance %>% 
      filter(Associe == input$associe_id,
             ID_consultant == input$consultant_id)
    
    my_connaissance_answer[["response"]]
    
  })
  
  output$mon_radar_asso <- renderPlot({
    
    qualite_asso_indiv <- questionnaire_qualites %>% 
      select(ID_consultant,
             Associe, 
             qualite,
             score) %>% 
      filter(ID_consultant == input$consultant_id,
             Associe == input$associe_id)
    
    qualite_asso_indiv %>%
      mutate(qualite = factor(qualite)) %>% 
      ggplot(aes(x = qualite, y = score)) +
      geom_col(alpha = 0.5, 
               width = 1, show.legend = FALSE, color = "white", aes(fill = factor(score))) +
      geom_hline(yintercept = seq(0, 3, by = 1),
                 colour = "#949494", size = 0.5, lty = 3) +
      geom_vline(xintercept = seq(0.5, 3.5, 1),
                 colour = "#949494", size = 0.4, lty = 1) +
      coord_polar() +
      scale_fill_manual(values = c("1"= "#56B4E9", "2" = "#E69F00","3"  ="#009E73")) +
      scale_y_continuous(
        limits = c(0, 3), 
        breaks = c(1, 2, 3, 4)) + 
      labs(x = "", y = "") + 
      theme(
        panel.background = element_rect(fill = "#FFFFFF"),
        plot.background = element_rect(fill = "#FFFFFF"),
        strip.background = element_rect(fill = "#FFFFFF"),
        strip.text = element_text(size = 15),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 7),
        panel.spacing = grid::unit(2, "lines"))
  })
  
  output$global_radar_asso <- renderPlot({
    
    qualite_asso_global <- questionnaire_qualites %>% 
      select(ID_consultant,
             Associe, 
             qualite,
             score) %>% 
      filter(Associe == input$associe_id) %>% 
      group_by(qualite) %>% 
      summarise(score = mean(score))%>%
      mutate(couleur= ifelse(score<1.6,1,
                             ifelse(score<2.33,2,3))) %>%
      mutate(couleur= factor(couleur))
    
    qualite_asso_global %>%
      mutate(qualite = factor(qualite)) %>% 
      ggplot(aes(x = qualite, y = score)) +
      geom_col(alpha = 0.5, 
               width = 1, show.legend = FALSE, color = "white", aes(fill = couleur)) +
      geom_hline(yintercept = seq(0, 3, by = 1),
                 colour = "#949494", size = 0.5, lty = 3) +
      geom_vline(xintercept = seq(0.5, 3.5, 1),
                 colour = "#949494", size = 0.4, lty = 1) +
      coord_polar() +
      scale_fill_manual(values = c("1"= "#56B4E9", "2" = "#E69F00","3"  ="#009E73")) +
      scale_y_continuous(
        limits = c(0, 3), 
        breaks = c(1, 2, 3, 4)) + 
      labs(x = "", y = "") + 
      theme(
        panel.background = element_rect(fill = "#FFFFFF"),
        plot.background = element_rect(fill = "#FFFFFF"),
        strip.background = element_rect(fill = "#FFFFFF"),
        strip.text = element_text(size = 15),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 7),
        panel.spacing = grid::unit(2, "lines"))
  })
  
  output$know_radar_asso <- renderPlot({
    
    knows_asso <- questionnaire_connaissance %>% 
      filter(Associe == input$associe_id,
             response == "Je connais") %>% 
      select(ID_consultant)
    
    qualite_asso_know <- questionnaire_qualites %>% 
      select(ID_consultant,
             Associe, 
             qualite,
             score) %>% 
      filter(Associe == input$associe_id) %>% 
      inner_join(knows_asso, by = "ID_consultant") %>% 
      group_by(qualite) %>% 
      summarise(score = mean(score)) %>%
      mutate(couleur= ifelse(score<1.6,1,
                             ifelse(score<2.33,2,3))) %>%
      mutate(couleur= factor(couleur))
    
    qualite_asso_know %>%
      mutate(qualite = factor(qualite)) %>% 
      ggplot(aes(x = qualite, y = score)) +
      geom_col(alpha = 0.5, 
               width = 1, show.legend = FALSE, color = "white", aes(fill = couleur)) +
      geom_hline(yintercept = seq(0, 3, by = 1),
                 colour = "#949494", size = 0.5, lty = 3) +
      geom_vline(xintercept = seq(0.5, 3.5, 1),
                 colour = "#949494", size = 0.4, lty = 1) +
      coord_polar() +
      scale_fill_manual(values = c("1"= "#56B4E9", "2" = "#E69F00","3"  ="#009E73"))+
      scale_y_continuous(
        limits = c(0, 3), 
        breaks = c(1, 2, 3, 4)) + 
      labs(x = "", y = "") + 
      theme(
        panel.background = element_rect(fill = "#FFFFFF"),
        plot.background = element_rect(fill = "#FFFFFF"),
        strip.background = element_rect(fill = "#FFFFFF"),
        strip.text = element_text(size = 15),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 7),
        panel.spacing = grid::unit(2, "lines"))
  })
  
  output$dont_know_radar_asso <- renderPlot({
    
    dont_knows_asso <- questionnaire_connaissance %>% 
      filter(Associe == input$associe_id,
             response == "Je ne connais pas") %>% 
      select(ID_consultant)
    
    qualite_asso_dont_know <- questionnaire_qualites %>% 
      select(ID_consultant,
             Associe, 
             qualite,
             score) %>% 
      filter(Associe == input$associe_id) %>% 
      inner_join(dont_knows_asso, by = "ID_consultant") %>% 
      group_by(qualite) %>% 
      summarise(score = mean(score)) %>%
      mutate(couleur= ifelse(score<1.6,1,
                             ifelse(score<2.33,2,3))) %>%
      mutate(couleur= factor(couleur))
    
    qualite_asso_dont_know %>%
      mutate(qualite = factor(qualite)) %>% 
      ggplot(aes(x = qualite, y = score)) +
      geom_col(alpha = 0.5, 
               width = 1, show.legend = FALSE, color = "white", aes(fill = couleur)) +
      geom_hline(yintercept = seq(0, 3, by = 1),
                 colour = "#949494", size = 0.5, lty = 3) +
      geom_vline(xintercept = seq(0.5, 3.5, 1),
                 colour = "#949494", size = 0.4, lty = 1) +
      coord_polar() +
      
      scale_fill_manual(values = c("1"= "#56B4E9", "2" = "#E69F00","3"  ="#009E73"))+
      scale_y_continuous(
        limits = c(0, 3), 
        breaks = c(1, 2, 3, 4)) + 
      labs(x = "", y = "") + 
      theme(
        panel.background = element_rect(fill = "#FFFFFF"),
        plot.background = element_rect(fill = "#FFFFFF"),
        strip.background = element_rect(fill = "#FFFFFF"),
        strip.text = element_text(size = 15),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 7),
        panel.spacing = grid::unit(2, "lines"))
  })
  
  output$plotDiag <- renderPlot ({
    #On constitue la matrice des connaissancess
    to <- paste(c("Brice Escarguel", "Julien Soyer", "Marc Morel", "Olivier Grandjean", "Olivier Reisse"),sep = ",")
    from <- paste(c(10:61),sep = ",")
    mat <- matrix(0, nrow = length(unique(from)), ncol = length(unique(to)))
    col <- matrix(0, nrow = length(unique(from)), ncol = length(unique(to)))
    col[,1] <- "#1CB217FF"
    col[,2] <- "#DA8B1BFF"
    col[,3] <- "#813A72FF"
    col[,4] <- "#629177FF"
    col[,5] <- "#25DC66FF"
    rownames(mat) = unique(from)
    colnames(mat) = unique(to)
    noms <- c(from,to)
    gripCol <- c(rep("#016CF0",52),c("#1CB217FF","#DA8B1BFF","#813A72FF","#629177FF","#25DC66FF"))
    names(gripCol) <- noms
  
    for (i in 1:51) {
      for (j in 1:5) {
        mat[i,j] <- matriceConnais[i,j]
      }
    }
    for(i in from ) {

      if (i != input$consultant_id ) {

        col[which(from == i), 1] = "#FFFFFF00"
        col[which(from == i), 2] = "#FFFFFF00"
        col[which(from == i), 3] = "#FFFFFF00"
        col[which(from == i), 4] = "#FFFFFF00"
        col[which(from == i), 5] = "#FFFFFF00"
    }
    }

    
    par(mar = c(1, 1, 1, 1))

    c <-chordDiagramFromMatrix(mat,  directional = TRUE, 
                     col=col, grid.col=gripCol, column.col=1:5,
                     annotationTrack = c("name", "grid"), annotationTrackHeight = c(0.05, 0.05),
                     link.lwd = par("lwd"), link.lty = par("lty")
                    
                     
                     )
    highlight.sector(input$consultant_id, track.index = 1, col = "#E69F00", text = input$consultant_id, niceFacing = TRUE)
    print(c)
  })
}
shinyServer(function(input, output) {
    
    output$mon_radar_asso <- renderPlot({
      
      qualite_asso_indiv <- questionnaire_qualites %>% 
        filter(DM == input$consultant_id) %>% 
        mutate(qualite = factor(qualite))
      
      mon_radar_plot <- qualite_asso_indiv %>%
        filter(ID_consultant == ID_cible) %>% 
        ggplot(aes(x = qualite, y = score)) +
        geom_col(alpha = 0.5, 
                 width = 1, show.legend = FALSE, color = "white", aes(fill = factor(score))) +
        geom_hline(yintercept = seq(0, 3, by = 1),
                   colour = "#949494", size = 0.5, lty = 3) +
        geom_vline(xintercept = seq(0.5, 3.5, 1),
                   colour = "#949494", size = 0.4, lty = 1) +
        coord_polar() +
        scale_fill_manual(values = c("1"= "#DE002C", "2" = "#E69F00","3"  ="#009E73")) +
        scale_y_continuous(
          limits = c(0, 3), 
          breaks = c(1, 2, 3, 4)) + 
        labs(x = "", y = "",
             title = "Mon avis sur moi-même") + 
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
      
      global_radar_plot <- qualite_asso_indiv %>%
        group_by(qualite) %>% 
        summarise(score = mean(score)) %>%
        mutate(couleur= factor(round(score))) %>%
        ggplot(aes(x = qualite, y = score)) +
        geom_col(alpha = 0.5, 
                 width = 1, show.legend = FALSE, color = "white", aes(fill = couleur)) +
        geom_col(aes(x = qualite, y = score), 
                 data = qualite_asso_indiv %>% filter(ID_consultant == ID_cible), 
                 alpha = 0.01,
                 width = 1, show.legend = FALSE, color = "black") +
        geom_hline(yintercept = seq(0, 3, by = 1),
                   colour = "#949494", size = 0.5, lty = 3) +
        geom_vline(xintercept = seq(0.5, 3.5, 1),
                   colour = "#949494", size = 0.4, lty = 1) +
        coord_polar() +
        scale_fill_manual(values = c("1"= "#DE002C", "2" = "#E69F00","3"  ="#009E73")) +
        scale_y_continuous(
          limits = c(0, 3), 
          breaks = c(1, 2, 3, 4)) + 
        labs(x = "", y = "",
             title = "L'avis moyen du cabinet") + 
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
      
      know_radar_plot <- qualite_asso_indiv %>%
        semi_join(questionnaire_connaissance %>% filter(response == "Je connais"),
                  by = "ID_consultant") %>% 
        group_by(qualite) %>% 
        summarise(score = mean(score)) %>%
        mutate(couleur= factor(round(score))) %>%
        ggplot(aes(x = qualite, y = score)) +
        geom_col(alpha = 0.5, 
                 width = 1, show.legend = FALSE, color = "white", aes(fill = couleur)) +
        geom_col(aes(x = qualite, y = score), 
                 data = qualite_asso_indiv %>% filter(ID_consultant == ID_cible), 
                 alpha = 0.01,
                 width = 1, show.legend = FALSE, color = "black") +
        geom_hline(yintercept = seq(0, 3, by = 1),
                   colour = "#949494", size = 0.5, lty = 3) +
        geom_vline(xintercept = seq(0.5, 3.5, 1),
                   colour = "#949494", size = 0.4, lty = 1) +
        coord_polar() +
        scale_fill_manual(values = c("1"= "#DE002C", "2" = "#E69F00","3"  ="#009E73"))+
        scale_y_continuous(
          limits = c(0, 3), 
          breaks = c(1, 2, 3, 4)) + 
        labs(x = "", y = "",
             title = "Ceux qui me connaissent") + 
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
      
      dont_know_radar_plot <- qualite_asso_indiv %>%
        semi_join(questionnaire_connaissance %>% filter(response == "Je ne connais pas"),
                  by = "ID_consultant") %>% 
        group_by(qualite) %>% 
        summarise(score = mean(score)) %>%
        mutate(couleur= factor(round(score))) %>%
        ggplot(aes(x = qualite, y = score)) +
        geom_col(alpha = 0.5, 
                 width = 1, show.legend = FALSE, color = "white", aes(fill = couleur)) +
        geom_col(aes(x = qualite, y = score), 
                 data = qualite_asso_indiv %>% filter(ID_consultant == ID_cible), 
                 alpha = 0.01,
                 width = 1, show.legend = FALSE, color = "black") +
        geom_hline(yintercept = seq(0, 3, by = 1),
                   colour = "#949494", size = 0.5, lty = 3) +
        geom_vline(xintercept = seq(0.5, 3.5, 1),
                   colour = "#949494", size = 0.4, lty = 1) +
        coord_polar() +
        scale_fill_manual(values = c("1"= "#DE002C", "2" = "#E69F00","3"  ="#009E73")) +
        scale_y_continuous(
          limits = c(0, 3), 
          breaks = c(1, 2, 3, 4)) + 
        labs(x = "", 
             y = "",
             title = "Ceux qui ne me connaissent pas") + 
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
      
      grid.arrange(mon_radar_plot, global_radar_plot, 
                   know_radar_plot, dont_know_radar_plot, ncol=2, nrow=2)
      
  })
  
  output$mon_ranking <- renderPlot({
    dm_ranking %>% 
      filter(DM == input$consultant_id) %>% 
      ggplot(aes(x = fct_reorder(qualite, son_rang), y = son_rang)) +
      geom_bar(stat = "identity", fill = "steelblue", alpha = 0.8) +
      labs(x = "",
           y = "Rang",
           title = "Mon classement selon le cabinet") +
      #scale_y_continuous(breaks = c(1, 2, 3, 4, 5), 
      #                   labels = c("1er", "2ème", "3ème", "4ème", "5ème")) + 
      coord_flip() + #ylim = c(0, 5)
      theme(
        panel.background = element_rect(fill = "#FFFFFF"),
        plot.background = element_rect(fill = "#FFFFFF"),
        strip.background = element_rect(fill = "#FFFFFF"),
        strip.text = element_text(size = 15),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(size = 7),
        panel.spacing = grid::unit(2, "lines"))
  })
  
  output$Connaissance <- renderTable({
    datasetInput <- questionnaire_connaissance %>% 
      filter(DM == input$consultant_id) %>% 
      group_by(response) %>% 
      summarize(Avis= n())
    
    colnames(datasetInput) <- c("Pertinence","Nombre") 
    
    datasetInput
  })
})
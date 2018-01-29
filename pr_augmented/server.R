shinyServer(function(input, output) {
  output$mon_radar_asso <- renderPlot({
    
    qualite_asso_indiv <- questionnaire_qualites %>% 
      select(ID_consultant,
             Consultant, 
             qualite,
             score) %>% 
      filter(ID_consultant == input$consultant_id,
             Consultant == names(accessible_ID)[accessible_ID == input$consultant_id])
    
    qualite_asso_indiv <- qualite_asso_indiv %>%
      mutate(qualite = factor(qualite))
    
    mon_radar_plot <- qualite_asso_indiv %>%
      mutate(qualite = factor(qualite)) %>% 
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
    
    qualite_asso_global <- questionnaire_qualites %>% 
      select(ID_consultant,
             Consultant, 
             qualite,
             score) %>% 
      filter(Consultant == names(accessible_ID)[accessible_ID == input$consultant_id]) %>% 
      group_by(qualite) %>% 
      summarise(score = mean(score))%>%
      mutate(couleur= round(score)) %>%
      mutate(couleur= factor(couleur))
    
    global_radar_plot <- qualite_asso_global %>%
      mutate(qualite = factor(qualite)) %>% 
      ggplot(aes(x = qualite, y = score)) +
      geom_col(alpha = 0.5, 
               width = 1, show.legend = FALSE, color = "white", aes(fill = couleur)) +
      geom_col(aes(x = qualite, y = score), data = qualite_asso_indiv, alpha = 0.1,
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
    
    knows_asso <- questionnaire_connaissance %>% 
      filter(Consultant == names(accessible_ID)[accessible_ID == input$consultant_id],
             response == "Je connais") %>% 
      select(ID_consultant)
    
    qualite_asso_know <- questionnaire_qualites %>% 
      select(ID_consultant,
             Consultant, 
             qualite,
             score) %>% 
      filter(Consultant == names(accessible_ID)[accessible_ID == input$consultant_id]) %>% 
      inner_join(knows_asso, by = "ID_consultant") %>% 
      group_by(qualite) %>% 
      summarise(score = mean(score)) %>%
      mutate(couleur= round(score)) %>%
      mutate(couleur= factor(couleur))
    
    know_radar_plot <- qualite_asso_know %>%
      mutate(qualite = factor(qualite)) %>% 
      ggplot(aes(x = qualite, y = score)) +
      geom_col(alpha = 0.5, 
               width = 1, show.legend = FALSE, color = "white", aes(fill = couleur)) +
      geom_col(aes(x = qualite, y = score), data = qualite_asso_indiv, alpha = 0.1,
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
    
    dont_knows_asso <- questionnaire_connaissance %>% 
      filter(Consultant == names(accessible_ID)[accessible_ID == input$consultant_id],
             response == "Je ne connais pas") %>% 
      select(ID_consultant)
    
    qualite_asso_dont_know <- questionnaire_qualites %>% 
      select(ID_consultant,
             Consultant, 
             qualite,
             score) %>% 
      filter(Consultant == names(accessible_ID)[accessible_ID == input$consultant_id]) %>% 
      inner_join(dont_knows_asso, by = "ID_consultant") %>% 
      group_by(qualite) %>% 
      summarise(score = mean(score)) %>%
      mutate(couleur= round(score)) %>%
      mutate(couleur= factor(couleur))
    
    dont_know_radar_plot <- qualite_asso_dont_know %>%
      mutate(qualite = factor(qualite)) %>% 
      ggplot(aes(x = qualite, y = score)) +
      geom_col(alpha = 0.5, 
               width = 1, show.legend = FALSE, color = "white", aes(fill = couleur)) +
      geom_col(aes(x = qualite, y = score), data = qualite_asso_indiv, alpha = 0.1,
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
    asso_ranking %>% 
      filter(Asso_ID == input$consultant_id) %>% 
      ggplot(aes(x = qualite, y = son_rang)) +
      geom_bar(stat = "identity", fill = "steelblue", alpha = 0.8) +
      coord_cartesian(ylim = c(0, 4)) +
      labs(x = "",
           y = "Rang",
           title = "Mon classement selon le cabinet") +
      scale_y_continuous(breaks = c(1, 2, 3, 4), labels = c("1er", "2ème", "3ème", "4ème")) + 
      coord_flip(ylim = c(0, 4)) +
      theme(
        panel.background = element_rect(fill = "#FFFFFF"),
        plot.background = element_rect(fill = "#FFFFFF"),
        strip.background = element_rect(fill = "#FFFFFF"),
        strip.text = element_text(size = 15),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        #axis.text.y = element_blank(),
        axis.text.x = element_text(size = 7),
        panel.spacing = grid::unit(2, "lines"))
  })
})
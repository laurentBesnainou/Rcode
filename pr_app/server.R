server <- function(input, output) { 
  
  output$connaissance_associes <- renderPlot({
    
      my_answer <- questionnaire_associe %>% 
        filter(Question == q1_str, ID_consultant == input$consultant_id) %>% 
        select(- Question) %>% 
        mutate(my_answer = 1) %>% 
        mutate(response = factor(response),
               Associe = factor(Associe))
      
      q1_count <- questionnaire_associe %>% 
        filter(Question == q1_str) %>% 
        select(- Question) %>% 
        group_by(Associe, response) %>% 
        summarise(COUNT = n()) %>% 
        ungroup() %>% 
        mutate(response = factor(response),
               Associe = factor(Associe)) %>% 
        left_join(my_answer, by = c("Associe", "response")) %>% 
        replace_na(list(my_answer = 0)) %>% 
        mutate(my_label = ifelse(my_answer == 1, "ici", "")) %>% 
        mutate(my_answer = factor(my_answer))
      
      cbbPalette <- c("#56B4E9", 
                      "#E69F00", 
                      "#009E73", 
                      "#F0E442", 
                      "#0072B2", 
                      "#D55E00", 
                      "#CC79A7")
      
      q1_count %>% 
        ggplot(aes(x = Associe, y = COUNT, 
                   fill = response, colour = my_answer)) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_text(aes(label = my_label), 
                  position = position_dodge(width = 0.9), 
                  vjust = -0.5, hjust = 0.5) +
        labs(x = "",
             y = "Nombre de consultants",
             fill = "") +
        scale_color_manual(values = c('white', 'red'))  +
        scale_fill_manual(values = cbbPalette) +
        guides(colour=FALSE) +
        theme_bw()
      
    })
    
    
  output$Q1_associes <- renderPlot({
    
    my_answer <- questionnaire_associe %>% 
      filter(Question == q2_str, 
             ID_consultant == input$consultant_id) %>% 
      select(- Question) %>% 
      mutate(my_answer = 1) %>% 
      mutate(response = factor(response),
             Associe = factor(Associe))
    
    q1_count <- questionnaire_associe %>% 
      filter(Question == q2_str) %>% 
      select(- Question) %>% 
      group_by(Associe, response) %>% 
      summarise(COUNT = n()) %>% 
      ungroup() %>% 
      mutate(response = factor(response),
             Associe = factor(Associe)) %>% 
      left_join(my_answer, by = c("Associe", "response")) %>% 
      replace_na(list(my_answer = 0)) %>% 
      mutate(my_label = ifelse(my_answer == 1, "ici", "")) %>% 
      mutate(my_answer = factor(my_answer))
    
    cbbPalette <- c("Content" = "#56B4E9", 
                    "Mécontent" = "#E69F00", 
                    "Neutre"  = "#009E73", 
                    "Pas Répondu" = "#F0E442")
    
    q1_count$response[is.na(q1_count$response)] <- "Pas Répondu"
    
    q1_count %>% 
      ggplot(aes(x = Associe, y = COUNT, fill = response, 
                 colour = my_answer)) +
      geom_bar(stat = "identity", position = "dodge",na.rm = FALSE) +
      geom_text(aes(label = my_label), position = position_dodge(width = 0.9), 
                vjust = -0.5, hjust = 0.5) +
      labs(x = "",
           y = "Nombre de consultants",
           fill = "") +
      scale_color_manual(values = c('white','red') )  +
      scale_fill_manual("response",values=cbbPalette)+
      guides(colour=FALSE) +
      theme_bw()
  })
 
  output$Q2_associes <- renderPlot({
    my_answer <- questionnaire_associe %>% 
      filter(Question == q3_str, 
             ID_consultant == input$consultant_id) %>% 
      select(- Question) %>% 
      mutate(my_answer = 1) %>% 
      mutate(response = factor(response),
             Associe = factor(Associe))
    
    q1_count <- questionnaire_associe %>% 
      filter(Question == q3_str) %>% 
      select(- Question) %>% 
      group_by(Associe, response) %>% 
      summarise(COUNT = n()) %>% 
      ungroup() %>% 
      mutate(response = factor(response),
             Associe = factor(Associe)) %>% 
      left_join(my_answer, by = c("Associe", "response")) %>% 
      replace_na(list(my_answer = 0)) %>% 
      mutate(my_label = ifelse(my_answer == 1, "ici", "")) %>% 
      mutate(my_answer = factor(my_answer))
    
    cbbPalette <- c("Content"= "#56B4E9", 
                    "Mécontent" = "#E69F00", 
                    "Neutre"  ="#009E73", 
                    "Pas Répondu"= "#F0E442")
    
    q1_count$response[is.na(q1_count$response)] <- "Pas Répondu"
    
    q1_count %>% 
      ggplot(aes(x = Associe, y = COUNT, fill = response, 
                 colour = my_answer)) +
      geom_bar(stat = "identity", position = "dodge",na.rm = FALSE) +
      geom_text(aes(label = my_label), position = position_dodge(width = 0.9), 
                vjust = -0.5, hjust = 0.5) +
      labs(x = "",
           y = "Nombre de consultants",
           fill = "") +
      scale_color_manual(values = c('white','red') )  +
      scale_fill_manual("response",values=cbbPalette)+
      guides(colour=FALSE) +
      theme_bw()
  })
  
  output$Q3_associes <- renderPlot({
    
    my_answer <- questionnaire_associe %>% 
      filter(Question == q4_str, 
             ID_consultant == input$consultant_id) %>% 
      select(- Question) %>% 
      mutate(my_answer = 1) %>% 
      mutate(response = factor(response),
             Associe = factor(Associe))
    
    q1_count <- questionnaire_associe %>% 
      filter(Question == q4_str) %>% 
      select(- Question) %>% 
      group_by(Associe, response) %>% 
      summarise(COUNT = n()) %>% 
      ungroup() %>% 
      mutate(response = factor(response),
             Associe = factor(Associe)) %>% 
      left_join(my_answer, by = c("Associe", "response")) %>% 
      replace_na(list(my_answer = 0)) %>% 
      mutate(my_label = ifelse(my_answer == 1, "ici", "")) %>% 
      mutate(my_answer = factor(my_answer))
    
    cbbPalette <- c("Content"= "#56B4E9", 
                    "Mécontent" = "#E69F00", 
                    "Neutre"  ="#009E73", 
                    "Pas Répondu"= "#F0E442")
    
    q1_count$response[is.na(q1_count$response)] <- "Pas Répondu"
    
    q1_count %>% 
      ggplot(aes(x = Associe, y = COUNT, fill = response, 
                 colour = my_answer)) +
      geom_bar(stat = "identity", position = "dodge",na.rm = FALSE) +
      geom_text(aes(label = my_label), position = position_dodge(width = 0.9), 
                vjust = -0.5, hjust = 0.5) +
      labs(x = "",
           y = "Nombre de consultants",
           fill = "") +
      scale_color_manual(values = c('white','red') )  +
      scale_fill_manual("response", values=cbbPalette)+
      guides(colour=FALSE) +
      theme_bw()
  })
  
  output$Q4_associes <- renderPlot({
    
    my_answer <- questionnaire_associe %>% 
      filter(Question == q5_str, 
             ID_consultant == input$consultant_id) %>% 
      select(- Question) %>% 
      mutate(my_answer = 1) %>% 
      mutate(response = factor(response),
             Associe = factor(Associe))
    
    q1_count <- questionnaire_associe %>% 
      filter(Question == q5_str) %>% 
      select(- Question) %>% 
      group_by(Associe, response) %>% 
      summarise(COUNT = n()) %>% 
      ungroup() %>% 
      mutate(response = factor(response),
             Associe = factor(Associe)) %>% 
      left_join(my_answer, by = c("Associe", "response")) %>% 
      replace_na(list(my_answer = 0)) %>% 
      mutate(my_label = ifelse(my_answer == 1, "ici", "")) %>% 
      mutate(my_answer = factor(my_answer))
    
    cbbPalette <- c("Content"= "#56B4E9", 
                    "Mécontent" = "#E69F00", 
                    "Neutre"  ="#009E73", 
                    "Pas Répondu"= "#F0E442")
    
    q1_count$response[is.na(q1_count$response)] <- "Pas Répondu"
    
    q1_count %>% 
      ggplot(aes(x = Associe, y = COUNT, fill = response, 
                 colour = my_answer)) +
      geom_bar(stat = "identity", position = "dodge",na.rm = FALSE) +
      geom_text(aes(label = my_label), position = position_dodge(width = 0.9), 
                vjust = -0.5, hjust = 0.5) +
      labs(x = "",
           y = "Nombre de consultants",
           fill = "") +
      scale_color_manual(values = c('white','red') )  +
      scale_fill_manual("response",values=cbbPalette)+
      guides(colour=FALSE) +
      theme_bw()
  })
  
  output$Global_associes <- renderPlot({
    
    my_answer <- questionnaire_associe %>% 
      filter(Question %in% c(q2_str,q3_str,q4_str,q5_str) , 
             ID_consultant == input$consultant_id) %>% 
      select(- Question) %>% 
      mutate(my_answer = 1) %>% 
      mutate(response = factor(response),
             Associe = factor(Associe))
    
    q1_count <- questionnaire_associe %>% 
      filter(Question %in% c(q2_str,q3_str,q4_str,q5_str)) %>% 
      select(- Question) %>% 
      group_by(Associe, response) %>% 
      summarise(COUNT = n()) %>% 
      ungroup() %>% 
      mutate(response = factor(response),
             Associe = factor(Associe)) %>% 
      left_join(my_answer, by = c("Associe", "response")) %>% 
      replace_na(list(my_answer = 0)) %>% 
      mutate(my_label = ifelse(my_answer == 1, "ici", "")) %>% 
      mutate(my_answer = factor(my_answer))
    
    cbbPalette <- c("Content"= "#56B4E9", 
                    "Mécontent" = "#E69F00", 
                    "Neutre"  ="#009E73", 
                    "Pas Répondu"= "#F0E442")
    
    q1_count$response[is.na(q1_count$response)] <- "Pas Répondu"
    
    q1_count %>% 
      ggplot(aes(x = Associe, y = COUNT, fill = response, 
                 colour = my_answer)) +
      geom_bar(stat = "identity", position = "dodge",na.rm = FALSE) +
      geom_text(aes(label = my_label), position = position_dodge(width = 0.9), 
                vjust = -0.5, hjust = 0.5) +
      labs(x = "",
           y = "Nombre de consultants",
           fill = "") +
      scale_color_manual(values = c('white','red') )  +
      scale_fill_manual("response",values=cbbPalette)+
      guides(colour=FALSE) +
      theme_bw()
  })
  
  output$Global2_associes <- renderPlot({
    my_answer <- questionnaire_associe %>% 
      filter(Question %in% c(q2_str,q3_str,q4_str,q5_str) , ID_consultant == input$consultant_id) %>% 
      select(- Question) %>% 
      mutate(my_answer = 1) %>% 
      mutate(response = factor(response),
             Associe = factor(Associe)) %>%
      group_by(response) %>% 
      summarise(COUNT = n())
    
    cbbPalette <- c("Content"= "#56B4E9", "Mécontent" = "#E69F00","Neutre"  ="#009E73","Pas Répondu"= "#F0E442")
    q1_count$response[is.na(q1_count$response)] <- "Pas Répondu"
    my_answer %>% 
      ggplot(aes(x = "", y = COUNT, fill = response)) +
      geom_bar(width = 1, stat = "identity") +
      scale_fill_manual(values = cbbPalette) +
      coord_polar(theta = "y") +
      guides(colour=FALSE) +
      theme_bw()
    
    
  })
  
  output$Global3_associes <- renderPlot({
    my_answer <- questionnaire_associe %>% 
      filter(Question %in% c(q2_str,q3_str,q4_str,q5_str) , ID_consultant == input$consultant_id) %>% 
      select(- Question) %>% mutate(Associe = str_trim(Associe)) %>%
      group_by(Associe, response) %>% 
      summarise(COUNT = n())
   
    my_answer_Asso <- questionnaire_associe %>% 
      filter(Question == q1_str , ID_consultant == input$consultant_id) %>% 
      select(Associe, response) 
    Avis <- left_join(my_answer,my_answer_Asso, by=c("Associe" ="Associe") )
    cbbPalette <- c("Content"= "#56B4E9", "Mécontent" = "#E69F00","Neutre"  ="#009E73","Pas Répondu"= "#F0E442")
    q1_count$response[is.na(q1_count$response)] <- "Pas Répondu"
    Avis %>% 
      ggplot(aes(x = "", y = COUNT, fill = response.x)) +
      geom_bar(width = 1, stat = "identity") +
      scale_fill_manual(values = cbbPalette) +
      coord_polar(theta = "y") +
      guides(colour=FALSE) +
      theme_bw()+
      facet_wrap(~response.y)
    
  })
  
  output$mon_radar_asso <- renderPlot({
    
    qualite_asso_indiv <- questionnaires_qualites %>% 
      mutate(stat = recode(Question,
                           "Dans quelle mesure considères-tu que les associés suivants incarnent la valeur de leadership ?" = "LEADERSHIP",
                           "Dans quelle mesure considères-tu que les associés suivants sont porteurs de vision ?" = "VISION",
                           "Dans quelle mesure considères-tu que les associés suivants incarnent un savoir-faire et savoir-être permettant l'origination commerciale ?" = "ORIGINATION\nCOMMERCIALE",
                           "Dans quelle mesure considères-tu que les associés suivants incarnent un savoir-faire et savoir-être permettant le développement des collaborateurs ?" = "DEVELOPPEMENT\nCOLLABORATEURS")) %>% 
      mutate(score = recode(response,
                            "Content" = 3,
                            "Neutre" = 2,
                            "Mécontent" = 1)) %>% 
      filter(!is.na(score)) %>% 
      select(ID_consultant,
             Associe, 
             stat,
             score) %>% 
      mutate(Associe = str_trim(Associe)) %>% 
      filter(ID_consultant == input$consultant_id,
             Associe == input$associe_id)
    
    qualite_asso_indiv %>%
      mutate(col = 1) %>%
      mutate(col = factor(col)) %>% 
      mutate(stat = factor(stat)) %>% 
      ggplot(aes(x = stat, y = score)) +
      geom_col(alpha = 0.5, aes(fill = factor(score)), 
               width = 1, show.legend = FALSE, color = "white") +
      geom_hline(yintercept = seq(0, 3, by = 1),
                 colour = "#949494", size = 0.5, lty = 3) +
      geom_vline(xintercept = seq(0.5, 5.5, 1),
                 colour = "#949494", size = 0.4, lty = 1) +
      coord_polar() +
      scale_fill_manual(values = c("1"= "#56B4E9", "2" = "#E69F00","3"  ="#009E73")) + 
      scale_y_continuous(
        limits = c(0, 4), 
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
    
    qualite_asso_global <- questionnaires_qualites %>% 
      mutate(stat = recode(Question,
                           "Dans quelle mesure considères-tu que les associés suivants incarnent la valeur de leadership ?" = "LEADERSHIP",
                           "Dans quelle mesure considères-tu que les associés suivants sont porteurs de vision ?" = "VISION",
                           "Dans quelle mesure considères-tu que les associés suivants incarnent un savoir-faire et savoir-être permettant l'origination commerciale ?" = "ORIGINATION\nCOMMERCIALE",
                           "Dans quelle mesure considères-tu que les associés suivants incarnent un savoir-faire et savoir-être permettant le développement des collaborateurs ?" = "DEVELOPPEMENT\nCOLLABORATEURS")) %>% 
      mutate(score = recode(response,
                            "Content" = 3,
                            "Neutre" = 2,
                            "Mécontent" = 1)) %>% 
      filter(!is.na(score)) %>% 
      select(ID_consultant,
             Associe, 
             stat,
             score) %>% 
      mutate(Associe = str_trim(Associe)) %>% 
      filter(Associe == input$associe_id) %>% 
      group_by(Associe, stat) %>% 
      summarise(score = mean(score)) %>% 
      mutate(couleur= ifelse(score<1.6,1,
                             ifelse(score<2.33,2,3))) %>%
      mutate(couleur= factor(couleur)) %>%
      ungroup()
    
    qualite_asso_global %>%
      mutate(col = 1) %>%
      mutate(col = factor(col)) %>% 
      mutate(stat = factor(stat)) %>% 
      ggplot(aes(x = stat, y = score)) +
      geom_col(alpha = 0.5, aes(fill = couleur), 
               width = 1, show.legend = FALSE, color = "white") +
      geom_hline(yintercept = seq(0, 3, by = 1),
                 colour = "#949494", size = 0.5, lty = 3) +
      geom_vline(xintercept = seq(0.5, 5.5, 1),
                 colour = "#949494", size = 0.4, lty = 1) +
      coord_polar() +
      scale_fill_manual(values = c("1"= "#56B4E9", "2" = "#E69F00","3"  ="#009E73"))+
      scale_y_continuous(
        limits = c(0, 3), 
        breaks = c(1, 2, 3)) + 
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
  
  output$global_radar_assoConnait <- renderPlot({
    connait <- questionnaire_associe %>% 
      filter(response=="Je connais", Associe == input$associe_id) %>% select(ID_consultant) %>% unique()
    questionnaires_qual <- questionnaire_associe %>% 
      filter(Question %in% c(q2_str, q3_str, q4_str, q5_str), ID_consultant %in% connait[[1]])
    qualite_asso_global <- questionnaires_qual %>% 
      mutate(stat = recode(Question,
                           "Dans quelle mesure considères-tu que les associés suivants incarnent la valeur de leadership ?" = "LEADERSHIP",
                           "Dans quelle mesure considères-tu que les associés suivants sont porteurs de vision ?" = "VISION",
                           "Dans quelle mesure considères-tu que les associés suivants incarnent un savoir-faire et savoir-être permettant l'origination commerciale ?" = "ORIGINATION\nCOMMERCIALE",
                           "Dans quelle mesure considères-tu que les associés suivants incarnent un savoir-faire et savoir-être permettant le développement des collaborateurs ?" = "DEVELOPPEMENT\nCOLLABORATEURS")) %>% 
      mutate(score = recode(response,
                            "Content" = 3,
                            "Neutre" = 2,
                            "Mécontent" = 1)) %>% 
      filter(!is.na(score)) %>% 
      select(ID_consultant,
             Associe, 
             stat,
             score) %>% 
      mutate(Associe = str_trim(Associe)) %>% 
      filter(Associe == input$associe_id) %>%  
      group_by(Associe, stat) %>% 
      summarise(score = mean(score)) %>% 
      mutate(couleur= ifelse(score<1.6,1,
                             ifelse(score<2.33,2,3))) %>%
      mutate(couleur= factor(couleur)) %>%
      ungroup()
    
    qualite_asso_global %>%
      mutate(col = 1) %>%
      mutate(col = factor(col)) %>% 
      mutate(stat = factor(stat)) %>% 
      ggplot(aes(x = stat, y = score)) +
      geom_col(alpha = 0.5, aes(fill = couleur), 
               width = 1, show.legend = FALSE, color = "white") +
      geom_hline(yintercept = seq(0, 3, by = 1),
                 colour = "#949494", size = 0.5, lty = 3) +
      geom_vline(xintercept = seq(0.5, 5.5, 1),
                 colour = "#949494", size = 0.4, lty = 1) +
      coord_polar() +
      scale_fill_manual(values = c("1"= "#56B4E9", "2" = "#E69F00","3"  ="#009E73"))+
      scale_y_continuous(
        limits = c(0, 3), 
        breaks = c(1, 2, 3)) + 
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
  
  
  output$global_radar_assoConnaitPas <- renderPlot({
    connait <- questionnaire_associe %>% 
      filter(response=="Je ne connais pas", Associe == input$associe_id) %>% select(ID_consultant) %>% unique()
    questionnaires_qual <- questionnaire_associe %>% 
      filter(Question %in% c(q2_str, q3_str, q4_str, q5_str), ID_consultant %in% connait[[1]])
    qualite_asso_global <- questionnaires_qual %>% 
      mutate(stat = recode(Question,
                           "Dans quelle mesure considères-tu que les associés suivants incarnent la valeur de leadership ?" = "LEADERSHIP",
                           "Dans quelle mesure considères-tu que les associés suivants sont porteurs de vision ?" = "VISION",
                           "Dans quelle mesure considères-tu que les associés suivants incarnent un savoir-faire et savoir-être permettant l'origination commerciale ?" = "ORIGINATION\nCOMMERCIALE",
                           "Dans quelle mesure considères-tu que les associés suivants incarnent un savoir-faire et savoir-être permettant le développement des collaborateurs ?" = "DEVELOPPEMENT\nCOLLABORATEURS")) %>% 
      mutate(score = recode(response,
                            "Content" = 3,
                            "Neutre" = 2,
                            "Mécontent" = 1)) %>% 
      filter(!is.na(score)) %>% 
      select(ID_consultant,
             Associe, 
             stat,
             score) %>% 
      mutate(Associe = str_trim(Associe)) %>% 
      filter(Associe == input$associe_id) %>%  
      group_by(Associe, stat) %>% 
      summarise(score = mean(score)) %>% 
      mutate(couleur= ifelse(score<1.6,1,
                             ifelse(score<2.33,2,3))) %>%
      mutate(couleur= factor(couleur)) %>%
      ungroup()
    
    qualite_asso_global %>%
      mutate(col = 1) %>%
      mutate(col = factor(col)) %>% 
      mutate(stat = factor(stat)) %>% 
      ggplot(aes(x = stat, y = score)) +
      geom_col(alpha = 0.5, aes(fill = couleur), 
               width = 1, show.legend = FALSE, color = "white") +
      geom_hline(yintercept = seq(0, 3, by = 1),
                 colour = "#949494", size = 0.5, lty = 3) +
      geom_vline(xintercept = seq(0.5, 5.5, 1),
                 colour = "#949494", size = 0.4, lty = 1) +
      coord_polar() +
      scale_fill_manual(values = c("1"= "#56B4E9", "2" = "#E69F00","3"  ="#009E73"))+
      scale_y_continuous(
        limits = c(0, 3), 
        breaks = c(1, 2, 3)) + 
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
}
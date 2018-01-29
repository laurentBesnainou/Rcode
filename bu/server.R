library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(RColorBrewer) #coulors pour ggplot
library(stringr)
library(lubridate) # gestion des dates
library(xts) #serie temporelle
library(dygraphs) #affichage de graphe avec affichage dynamique
library(forcats)
library(DT) # Affichage des tableaux dynamiques
library(purrr) # 
library(tidyr)
library(scales) 
library(googleVis) # Permet d'utiliser les graphiques >  Google Chart 
library(timevis)# Gantt

Q1 <- c("sais pas","sais pas","sais pas","Oui","Non","Oui","Non","Non","Oui",
        "Non","Non","Non","sais pas","sais pas","sais pas","Oui","Oui","Non",
        "Non","Non","Non","sais pas","Non","sais pas","sais pas","sais pas","Oui",
        "Non","Non","Non","sais pas","Non","sais pas","sais pas","sais pas","Oui")
Q2 <- c("sais pas", "Oui","sais pas", "sais pas", "Oui","sais pas", "sais pas", "Oui",
        "sais pas", "sais pas", "sais pas", "sais pas", "Oui","Oui","Oui","Non", "Oui",
        "Oui", "Non", "Oui","Oui","Oui","Non","sais pas","sais pas","sais pas","Oui",
        "Oui", "Non", "Oui","Oui","Oui","Non","sais pas","sais pas","sais pas")
Q3 <- c("Oui","Non","Non","Oui","Oui","sais pas", "Oui","sais pas", "Non",
        "Oui","Non","Non","Oui","Oui","sais pas", "Oui","sais pas", "Non",
        "Oui","sais pas", "sais pas", "sais pas", "Oui","Oui","Non","Oui","Oui",
        "Non","Non","Oui","Non","Oui","Non","Oui","sais pas", "sais pas")

Qui <- c("AUDINET", "BERGER","BESNAINOU","BEYLLE","BOULAYE (de la)",
               "BRUNAT", "CHOUAI", "COICAULT",
               "COLLUMEAU", "DIAS",
               "DUFOREST", "GALL",
               "GASCON", "GASCUEL",
               "GAUTHIER",  "HUREAU",
               "LANFRANCHI",  "LE CALLONNEC",
               "LISSOIR", "LORANT",
               "MAILLARD", "MALOSSE",
               "MARCHIVE", "MUR",
               "PERNET", "PETIT",
               "POPP",   "ROCHE",
               "SALOUM", "STURTZER",
               "SUCHAUD", "TARDU",
               "THION", "URVOY DE CLOSMADEUC",
               "VANHEECKHOET", "VELTEN-JAMESON")


#On charge les données du questionnaire sur les associés
load("data_Associe.RData")
data_src <- data_Associe
Q1_ASS <- data_Associe %>% select(AvisBrice,AvisOlivierG,AvisMarc,AvisOlivierR,AvisJulien,
                                  Q1Brice,Q1OlivierG,Q1Marc,Q1OlivierR,Q1Julien) %>%
  gather(key = "QUI", value = "VALEUR", Q1Brice,Q1OlivierG,Q1Marc,Q1OlivierR,Q1Julien) %>%
  gather(key = "AVIS", value = "SUR", AvisBrice,AvisOlivierG,AvisMarc,AvisOlivierR,AvisJulien)
#Nombre de reponses dans le questionnaire
nbReponse <- nrow(data_Associe) - 1 
#nbReponsesAsso <- data_Associe %>% filter(!is.na(`e-mail`)) %>% nrow()
#ou regadre qui connait les associés
data_Associe_Avis <- data_Associe %>% gather(key = "TYPE", value = "VALEUR", AvisBrice, AvisOlivierG,AvisMarc,AvisOlivierR,AvisJulien)

#Q1
data_Associe_Q1 <- data_src %>% gather(key = "Q1", value = "VQ1", Q1Brice,       Q1OlivierG,   Q1Marc,        Q1OlivierR,    Q1Julien)

#Q2
data_Associe_Q2 <- data_src %>% gather(key = "Q2", value = "VQ2", Q2Brice,       Q2OlivierG,   Q2Marc,        Q2OlivierR,    Q2Julien)

#Q3
data_Associe_Q3 <- data_src %>% gather(key = "Q3", value = "VQ3", Q3Brice,       Q3OlivierG,   Q3Marc,        Q3OlivierR,    Q3Julien)

#Q4
data_Associe_Q4 <- data_src %>% gather(key = "Q4", value = "VQ4", Q4Brice,       Q4OlivierG,   Q4Marc,        Q4OlivierR,    Q4Julien)


RepALL <- data_frame("QUI" = Qui, "Q1"=Q1,"Q2"=Q2,"Q3"=Q3)
QAll <-  data_frame("QUESTION" = c("Q1","Q2","Q3"), 
                    "OUI"=c(20,10,7),
                    "OUICONNAIT"=c(18,10,2),
                    "OUISAISPAS"=c(2,0,5),
                    "NON"=c(15,7,4),
                    "NONCONNAIT"=c(10,5,2),
                    "NONSAISPAS"=c(5,2,2),
                    "SP"=c(5,3,3),
                    "SPCONNAIT"=c(3,3,2),
                    "SPSAISPAS"=c(2,0,1))
QAll <- QAll %>% gather(key = "TYPE", value = "VALEUR", -QUESTION)
server <- function(input, output, session) {
 

  ################# Liste des Infobox  
  
  caBT <- reactive({
    ChoixAnnee(input$Annee)
    pilotage_data %>% filter(STEP == "4 - Gagnée", WEEK == input$semaine,
                             OFFRE_PRINCIPALE %in% input$uiChk1) %>%
      summarise(CA_BT__N__KE = sum(CA_BT__N__KE, na.rm = TRUE))
  })  
  
iconSmile <- function(qui,Question) {
  reponse <- RepALL[RepALL$QUI==qui,Question]
  if (reponse == "Oui") {icone <- "smile-o" } 
  if (reponse == "Non") {icone <- "frown-o " }
  if (reponse == "sais pas") {icone <- "meh-o" }
  icone
}
  
  
  output$Q1 <- renderInfoBox({
    infoBox("<< Réponse", value = "Je pense Business Technology ",
            
            icon = icon(iconSmile(input$presonne,"Q1")),
            color= "aqua")
  })
  
  output$Q2 <- renderInfoBox({
    infoBox("la personne a voté", value = "Je pense  différemment ",
          
            icon = icon(iconSmile(input$presonne,"Q2")),
            color= "aqua")
  })

  output$Q3 <- renderInfoBox({
    infoBox("la personne a voté", value = tags$p("Je contribue à l'intelligence connective ", 
                                         style = "font-size: 70%;"),
            
            icon = icon(iconSmile(input$presonne,"Q3")),
            color= "aqua")
  }) 
  
  
  output$A1 <- renderInfoBox({
    infoBox("Nombre de réponses", value = nbReponse,
            
            icon = icon("user-circle-o"),
            color= "aqua")
  }) 
  

  output$Q2_plot <- renderPlot({
    QAll %>% filter (QUESTION == "Q1", TYPE %in% c("OUICONNAIT","NONCONNAIT","SPCONNAIT") )%>%
      ggplot(aes(x = TYPE,
                 y = VALEUR, fill= TYPE )) +
      geom_bar(stat = "identity") +
      scale_x_discrete(name="",limit = c("OUICONNAIT", "NONCONNAIT", "SPCONNAIT"))+
      scale_y_continuous(name="Reponses")+
      ggtitle("Question 1")+
      theme_hc() +
      scale_colour_hc()
  })
  output$Q3_plot <- renderPlot({
    QAll %>% filter (QUESTION == "Q1", TYPE %in% c("OUISAISPAS","NONSAISPAS","SPSAISPAS") )%>%
      ggplot(aes(x = TYPE,
                 y = VALEUR, fill= TYPE )) +
      geom_bar(stat = "identity") +
      scale_x_discrete(name="",limit = c("OUISAISPAS","NONSAISPAS","SPSAISPAS"))+
      scale_y_continuous(name="Reponses")+
      ggtitle("Question 1")
  })
  
  
  
  output$Q1_plot <- renderPlot({
    cbbPalette <- c("#00A8E8", "#33658A", "#F6AE2D")
    levels <- c("OUI", "NON", "SP","OUICONNAIT", "NONCONNAIT", "SPCONNAIT","OUISAISPAS","NONSAISPAS","SPSAISPAS")
    labels <- c("OUI", "NON", "SP","OUI", "NON", "SP","OUI", "SP", "NON")
    avis <- c("Total", "Total", "Total","Connait", "Connait", "Connait","SaisPAs", "SaisPAs", "SaisPAs")
    QAll %>% mutate(TYPE2 = labels[match(TYPE,levels)],
                    TYPE3 = avis[match(TYPE,levels)]) %>%
    filter (QUESTION == "Q1")%>% 
      mutate (TYPE3 = factor(TYPE3, levels=c("Total", "Connait","SaisPAs"))) %>%
      ggplot(aes(x = TYPE2,
                 y = VALEUR, fill= TYPE3 )) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_x_discrete(name="",limit = c("OUI","NON","SP"))+
      scale_y_continuous(name="Reponses")+
      ggtitle("Ce que BT pense de moi sur ce point - Répartition des reponses")+
      theme_economist() + scale_colour_economist() +scale_fill_manual(values=cbbPalette)
  })
  
  
  output$A1_plot <- renderPlot({
    cbbPalette <- c("#00A8E8", "#33658A", "#F6AE2D")
    
    data_Associe_Avis %>%filter(!is.na(ID)) %>% 
      mutate(TYPE=str_replace_all(TYPE,"Avis","")) %>%
      select (TYPE,VALEUR) %>% group_by(TYPE) %>%
      summarise(Total = sum(VALEUR, na.rm = TRUE))%>%
      ggplot(aes(x = reorder(TYPE, -Total),
                 y = Total, fill= "#F6AE2D" )) +
      geom_bar(stat = "identity") +
      scale_y_continuous(name="Reponses")+
      scale_x_discrete(name="Associés") +
      ggtitle("Mon Avis est pertinent pour - Associés")+
      theme_economist() + scale_colour_economist() +scale_fill_manual(values=cbbPalette) +
      theme(legend.position="none")
  })
  
  
  output$A2_plot <- renderPlot({
    cbbPalette <- c("#00A8E8", "#33658A", "#9BC995","#F6AE2D")
    
    data_Associe_Q1 %>%filter(!is.na(ID)) %>% 
      mutate(Q1=str_replace_all(Q1,"Q1","")) %>%
      select (Q1,VQ1) %>% mutate (VQ1=factor(VQ1)) %>% group_by(Q1,VQ1) %>% summarise(nb=n()) %>%
      ggplot(aes(x = Q1,y = nb,fill=VQ1) )+
      geom_bar(stat = "identity", position = "Dodge") +
      scale_y_continuous(name="Reponses")+
      scale_x_discrete(name="Associés") +
      ggtitle("Question 1 - Associés")+
      theme_economist() + scale_colour_economist() +
      #scale_fill_manual(values=cbbPalette) +
      theme(legend.position="bottom")
  })
  
  output$A3_plot <- renderPlot({
    cbbPalette <- c("#00A8E8", "#33658A", "#9BC995","#F6AE2D")
    
    data_Associe_Q2 %>%filter(!is.na(ID)) %>% 
      mutate(Q2=str_replace_all(Q2,"Q2","")) %>%
      select (Q2,VQ2) %>% mutate (VQ2=factor(VQ2)) %>% group_by(Q2,VQ2) %>% summarise(nb=n()) %>%
      ggplot(aes(x = Q2,y = nb,fill=VQ2) )+
      geom_bar(stat = "identity", position = "Dodge") +
      scale_y_continuous(name="Reponses")+
      scale_x_discrete(name="Associés") +
      ggtitle("Question 2 - Associés")+
      theme_economist() + scale_colour_economist() +
      scale_fill_manual(values=cbbPalette) +
      theme(legend.position="bottom")
  })
  
  output$A4_plot <- renderPlot({
    cbbPalette <- c("#00A8E8", "#33658A", "#9BC995","#F6AE2D")
    
    data_Associe_Q3 %>%filter(!is.na(ID)) %>% 
      mutate(Q3=str_replace_all(Q3,"Q3","")) %>%
      select (Q3,VQ3) %>% mutate (VQ3=factor(VQ3)) %>% group_by(Q3,VQ3) %>% summarise(nb=n()) %>%
      ggplot(aes(x = Q3,y = nb,fill=VQ3) )+
      geom_bar(stat = "identity", position = "Dodge") +
      scale_y_continuous(name="Reponses")+
      scale_x_discrete(name="Associés") +
      ggtitle("Question 3 - Associés")+
      theme_economist() + scale_colour_economist() +
      #scale_fill_manual(values=cbbPalette) +
      theme(legend.position="bottom")
  })
  
  output$A5_plot <- renderPlot({
    cbbPalette <- c("#00A8E8", "#33658A", "#9BC995","#F6AE2D")
    
    data_Associe_Q4 %>%filter(!is.na(ID)) %>% 
      mutate(Q4=str_replace_all(Q4,"Q4","")) %>%
      select (Q4,VQ4) %>% mutate (VQ4=factor(VQ4)) %>% group_by(Q4,VQ4) %>% summarise(nb=n()) %>%
      ggplot(aes(x = Q4,y = nb,fill=VQ4) )+
      geom_bar(stat = "identity", position = "Dodge") +
      scale_y_continuous(name="Reponses")+
      scale_x_discrete(name="Associés") +
      ggtitle("Question 4 - Associés")+
      theme_economist() + scale_colour_economist() +
      #scale_fill_manual(values=cbbPalette) +
      theme(legend.position="bottom")
  })
  
  output$A1bis_plot <- renderPlot({
    cbbPalette <- c("#00A8E8", "#33658A", "#9BC995","#F6AE2D")
    
    data_Associe %>%filter(!is.na(ID)) %>%
      mutate (AvisBrice = ifelse(AvisBrice == TRUE,"Connait","Connait pas")) %>%
      select (Q1Brice,AvisBrice) %>% mutate (AvisBrice=factor(AvisBrice)) %>% group_by(AvisBrice,Q1Brice) %>% 
      summarise(nb=n()) %>%
      ggplot(aes(x = Q1Brice,y = nb, fill=AvisBrice) )+
      geom_bar(stat = "identity", position = "Dodge") +
      scale_y_continuous(name="Reponses")+
      scale_x_discrete(name="Associés") +
      ggtitle("Question 1  En fonction de la connaissance - pour Brice")+
      theme_economist() + scale_colour_economist() +
      scale_fill_manual(values=cbbPalette) +
      facet_wrap( ~ AvisBrice)
  })
  
  output$A2bis_plot <- renderPlot({
    cbbPalette <- c("#00A8E8", "#33658A", "#9BC995","#F6AE2D")
    
    data_Associe %>%filter(!is.na(ID)) %>%
      mutate (AvisOlivierG = ifelse(AvisBrice == TRUE,"Connait","Connait pas")) %>%
      select (Q1OlivierG,AvisOlivierG) %>% mutate (AvisOlivierG=factor(AvisOlivierG)) %>% group_by(AvisOlivierG,Q1OlivierG) %>% 
      summarise(nb=n()) %>%
      ggplot(aes(x = Q1OlivierG,y = nb, fill=AvisOlivierG) )+
      geom_bar(stat = "identity", position = "Dodge") +
      scale_y_continuous(name="Reponses")+
      scale_x_discrete(name="Associés") +
      ggtitle("Question 1  En fonction de la connaissance - pour OlivierG")+
      theme_economist() + scale_colour_economist() +
      scale_fill_manual(values=cbbPalette) +
      facet_wrap( ~ AvisOlivierG)
  })
  
  
  output$A3bis_plot <- renderPlot({
    cbbPalette <- c("#00A8E8", "#33658A", "#9BC995","#F6AE2D")
    
    data_Associe %>%filter(!is.na(ID)) %>%
      mutate (AvisMarc = ifelse(AvisMarc == TRUE,"Connait","Connait pas")) %>%
      select (Q1Marc,AvisMarc) %>% mutate (AvisMarc=factor(AvisMarc)) %>% group_by(AvisMarc,Q1Marc) %>% 
      summarise(nb=n()) %>%
      ggplot(aes(x = Q1Marc,y = nb, fill=AvisMarc) )+
      geom_bar(stat = "identity", position = "Dodge") +
      scale_y_continuous(name="Reponses")+
      scale_x_discrete(name="Associés") +
      ggtitle("Question 1  En fonction de la connaissance - pour Marc")+
      theme_economist() + scale_colour_economist() +
      scale_fill_manual(values=cbbPalette) +
      facet_wrap( ~ AvisMarc)
  })
 
  output$A4bis_plot <- renderPlot({
    cbbPalette <- c("#00A8E8", "#33658A", "#9BC995","#F6AE2D")
    
    data_Associe %>%filter(!is.na(ID)) %>%
      mutate (AvisOlivierR = ifelse(AvisOlivierR == TRUE,"Connait","Connait pas")) %>%
      select (Q1OlivierR,AvisOlivierR) %>% mutate (AvisOlivierR=factor(AvisOlivierR)) %>% group_by(AvisOlivierR,Q1OlivierR) %>% 
      summarise(nb=n()) %>%
      ggplot(aes(x = Q1OlivierR,y = nb, fill=AvisOlivierR) )+
      geom_bar(stat = "identity", position = "Dodge") +
      scale_y_continuous(name="Reponses")+
      scale_x_discrete(name="Associés") +
      ggtitle("Question 1  En fonction de la connaissance - pour Marc")+
      theme_economist() + scale_colour_economist() +
      scale_fill_manual(values=cbbPalette) +
      facet_wrap( ~ AvisOlivierR)
  })
  
  output$A3bis_plot <- renderPlot({
    cbbPalette <- c("#00A8E8", "#33658A", "#9BC995","#F6AE2D")
    
    data_Associe %>%filter(!is.na(ID)) %>%
      mutate (AvisMarc = ifelse(AvisMarc == TRUE,"Connait","Connait pas")) %>%
      select (Q1Marc,AvisMarc) %>% mutate (AvisMarc=factor(AvisMarc)) %>% group_by(AvisMarc,Q1Marc) %>% 
      summarise(nb=n()) %>%
      ggplot(aes(x = Q1Marc,y = nb, fill=AvisMarc) )+
      geom_bar(stat = "identity", position = "Dodge") +
      scale_y_continuous(name="Reponses")+
      scale_x_discrete(name="Associés") +
      ggtitle("Question 1  En fonction de la connaissance - pour Marc")+
      theme_economist() + scale_colour_economist() +
      scale_fill_manual(values=cbbPalette) +
      facet_wrap( ~ AvisMarc)
  })
  }
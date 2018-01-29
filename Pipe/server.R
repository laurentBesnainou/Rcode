
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(input, output) {

  output$hcontainer <- renderHighchart({

    p2015 <- pilotage_2015  %>% mutate(YEAR=2015) %>%
      filter(STEP %in% c("1 - Qualifiée", "2 - A émettre", "3 - Emise")) %>%
      rename(CA_BT = CA_BT__N__KE) %>%select(STEP,
                                             CA_BT,
                                             OFFRE_PRINCIPALE,
                                             WEEK,
                                             YEAR)
    p2016 <- pilotage_2016  %>% mutate(YEAR=2016) %>%
      filter(STEP %in% c("1 - Qualifiée", "2 - A émettre", "3 - Emise")) %>%
      rename(CA_BT = CA_BT__N__KE) %>%select(STEP,
                                             CA_BT,
                                             OFFRE_PRINCIPALE,
                                             WEEK,
                                             YEAR)
    
    p2017 <- pilotage_data  %>% mutate(YEAR=2017) %>%
      filter(STEP %in% c("1 - Qualifiée", "2 - A émettre", "3 - Emise")) %>%
      rename(CA_BT = CA_BT__N__KE) %>%select(STEP,
                                             CA_BT,
                                             OFFRE_PRINCIPALE,
                                             WEEK,YEAR)
    
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
    
    
    plot_chart <- pilotage_tot %>% 
      mutate(YEAR = factor(YEAR)) %>% 
      group_by(YEAR, WEEK) %>% 
      summarise(CA_TOT = sum(CA_BT, na.rm = TRUE), Median=median(CA_BT, na.rm = TRUE), Moyenne=mean(CA_BT, na.rm = TRUE))
    
    hc <-  highchart() %>% 
      hc_add_series (plot_chart, name="moyenne",type = "line",dashStyle= 'dot',marker = list(enabled = FALSE), #C ,marker = list(symbol = fa_icon_mark("eur")),
                     color= couleur, hcaes(x = WEEK, y = Moyenne, group = YEAR) ) %>% 
      hc_add_series (plot_chart, type = "line",marker = list(enabled = FALSE), #C ,marker = list(symbol = fa_icon_mark("eur")),
                     color= couleur, hcaes(x = WEEK, y = Median, group = YEAR) ) %>% 
      hc_subtitle(text = "Pipe :Qualifiée, A émettre, Emise") %>%
      hc_title(text = "Evolution du pipe Mediane (valeur moyenne en pointillé)",
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
    hc <- hc %>%
      hc_add_theme(theme)
    
    

    
    
    hc
  })

  output$hcTree <- renderHighchart({
    

    
    p2017 <- pilotage_data  %>% mutate(YEAR=2017) %>%
      filter(STEP %in% c( "2 - A émettre","1 - Qualifiée","3 - Emise"),WEEK==27) %>% 
      mutate(CA_BT = as.integer(CA_BT__N__KE))      %>%select(STEP,
                                             OFFRE_PRINCIPALE,
                                             SECTEUR,SUJET,
                                             CA_BT
                                             ) %>% filter(!is.na(CA_BT)) 
   # p2017$Couleur <- 0
   #  p2017[p2017$STEP=="1 - Qualifiée",]$Couleur <- 1
   #  p2017[p2017$STEP=="2 - A émettre",]$Couleur <- 2
   #  p2017[p2017$STEP=="3 - Emise",]$Couleur <- 3
    

    tm <- treemap(p2017, #Your data frame object
            index=c("STEP", "OFFRE_PRINCIPALE","SUJET"),  #A list of your categorical variables
            vSize = "CA_BT",  #This is your quantitative variable
            type="index", #Type sets the organization and color scheme of your treemap
            palette = "Accent",  #Select your color palette from the RColorBrewer presets or make your own.
            title="Spending", #Customize your title
            fontsize.title = 14 #Change the font size of the title
    )
    
   
    hc <- hctreemap(tm, allowDrillToNode = TRUE, layoutAlgorithm = "squarified") %>% 
      hc_title(text = "Répartition Pipe") %>% 
      hc_tooltip(pointFormat = "SECTEUR: {point.value} <br> cluster: {point.name}") 
     
   
    
    
    hc
  })
  
  
  
  
  # Create Sunburst plot
  output$sunburstPlot <- renderSunburst({ 

    p2017 <- pilotage_data  %>% mutate(YEAR=2017) %>%
      filter(STEP %in% c( "2 - A émettre","1 - Qualifiée","3 - Emise"),WEEK==27) %>% 
      mutate(CA_BT = as.integer(CA_BT__N__KE))      %>%select(STEP,
                                                              OFFRE_PRINCIPALE,
                                                              SECTEUR,SUJET,
                                                              CA_BT
      ) %>% filter(!is.na(CA_BT))
    p2017$STEP <- gsub("-", "#", p2017$STEP)
    p2017$SUJET <- gsub("-", "#", p2017$SUJET)
      tempDat <-  data.table(A=p2017$STEP, B = p2017$OFFRE_PRINCIPALE, C = p2017$SUJET,D=p2017$CA_BT)
      tempDat[,c("V1","V2"):= list(paste0(A,"-",B, "-", C),D)]
      sunburst(tempDat[,.(V1,V2)], percent = FALSE,count = TRUE)
    mutate(V1=paste(OFFRE_PRINCIPALE,SUJET,sep="-")) %>%
      mutate(V2=CA_BT)
    sunburst(p2017[,.(V1,V2)])
    
  })
})

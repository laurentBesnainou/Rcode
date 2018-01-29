shinyUI(fluidPage(
  
  theme = shinythemes::shinytheme("cosmo"),
  titlePanel("PR - Augmentée Consultants"),
  
  sidebarLayout(
    sidebarPanel(selectInput("consultant_id", "Sélectionner un individu:", 
                             choices = questionnaire_qualites %>% distinct(Consultant)), 
                 tableOutput("Connaissance"),
                 width = 3),
    
    mainPanel(
      plotOutput("mon_ranking", height= 300),
      tags$br(),
      tags$br(),
      plotOutput("mon_radar_asso", height = 500),  tableOutput("view"),
      width = 9
    )
    
  )
)
)
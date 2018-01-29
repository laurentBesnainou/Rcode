shinyUI(
  
  fluidPage(
    HTML('<style type="text/css">
     h2, .h2 {font-size: 20px; font-weight: bold;}
    .row-fluid { width: 50%; }  
         .well { background-color: #D7EBFC; }
         .shiny-html-output { font-size: 14px; line-height: 10px; }
         </style>'),
  theme = shinythemes::shinytheme("cosmo"),
  titlePanel("PR - Augmentée, Associés"),
  
  sidebarLayout(
    sidebarPanel(selectInput("consultant_id", "Sélectionner un individu:", 
                             choices = questionnaire_qualites %>% distinct(Associe)), 
                 width = 3,
                 tableOutput("Connaissance"),
                 tableOutput("view")),
    
    mainPanel(
      plotOutput("mon_ranking", height= 300),
      tags$br(),
      tags$br(),
      plotOutput("mon_radar_asso", height = 500), width = 9
    )
    
  )
)
)
shinyUI(fluidPage(
  
  theme = shinythemes::shinytheme("cosmo"),
  titlePanel("PR - Augmentée"),
  
  sidebarLayout(
    sidebarPanel(selectInput("consultant_id", "Sélectionner un individu:", 
                              choices = accessible_ID %>% as.vector()), width = 2),
    
#     mainPanel(
#       plotOutput("mon_ranking", height= 300),
#       tags$br(),
#       tags$br(),
#       plotOutput("mon_radar_asso", height = 500), width = 10    )
#   
# )

mainPanel(
  tabsetPanel(
    # tabPanel("Associé", 
    #          fluidRow(column(10, offset=1, plotOutput("mon_ranking", height= 300) ),
    #                   column(5, class= "R2C2", helpText("This is an exemple") ) ),
    #          fluidRow(column(5, offset=1, plotOutput("mon_radar_asso", height = 500) )), 
    tabPanel("Manager", verbatimTextOutput("summary")), 
    tabPanel("Consultant", verbatimTextOutput("summary"))
  )


)
)
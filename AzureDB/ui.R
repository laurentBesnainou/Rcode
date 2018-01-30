dashboardBody


header <- dashboardHeader(
  title = "Analyse Semaine BT",
  tags$li(a(href = 'https://demodashboard.shinyapps.io/DB_V1_LP/',
            icon("power-off"),
            title = "Back to Apps Home"),
          class = "dropdown")
  
)

body <- dashboardBody(
  fluidRow(
    column(width = 9,
           tabsetPanel(
             tabPanel("Indicateurs",
                      # Dynamic valueBoxes
                      box( width = 12,
                           tags$p("Données actuels"),
                           valueBoxOutput("TotalBox"),
                           valueBoxOutput("TotalPondereBox"),
                           valueBoxOutput("Total0"),
                           valueBoxOutput("Total1"),
                           valueBoxOutput("Total2"),
                           valueBoxOutput("Total3")
                      ),
                      box( width = 12,
                           tags$p("Semaine passée"),
                           valueBoxOutput("TotalBoxS"),
                           valueBoxOutput("TotalPondereBoxS"),
                           valueBoxOutput("Total0S"),
                           valueBoxOutput("Total1S"),
                           valueBoxOutput("Total2S"),
                           valueBoxOutput("Total3S")
                      )
                      
                      ),
             # tabPanel("Répartition par Groupe", 
             #          box( width = 12,
             #               div(dataTableOutput("DT_win"), style = "font-size:80%"))
             #          ),
             tabPanel("Evolution dans le temps", box( width = 12,actionButton("button", "Actualiser  données Pipe"),
                                                      plotOutput("Pipe_Repartition"),
                                                      plotOutput("Win_Repartition"))
                      ),
             tabPanel("Evolution Staffing", box( width = 12,actionButton("buttonStaff", "Actualiser données Staffing"),
                                                 selectInput("Semaine", label = h3("choix semaine"), 
                                                             choices = c(1:(week(now())-1)), 
                                                             selected = week(now())-1),
                                                      plotOutput("Staffing"),
                                                 plotOutput("StaffingGrade"),
                                                 plotOutput("StaffingHeat"),
                                                 plotOutput("StaffingHeatEcart"))
             )
             
           
            )
          )
    )
  )

dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)

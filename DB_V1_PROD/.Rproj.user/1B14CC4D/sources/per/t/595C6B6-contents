
header <- dashboardHeader(
  title = "Production BT",
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
                      valueBoxOutput("TotalBox")
                      ),
                      valueBoxOutput("TotalETMBox"),
                      valueBoxOutput("TotalTRANSFOBox"),
                      valueBoxOutput("TotalDIGITALBox"),
                      valueBoxOutput("TotalDATABox"),
                      valueBoxOutput("TotalSECURITEBox"),
                      box( width = 12,
                      valueBoxOutput("nbBox"))),
             tabPanel("Répartition par Groupe",plotOutput("Pipe_Repartition")),
             tabPanel("Evolution dans le temps", plotOutput("BT_Repartition",brush = brushOpts(
               id = "plot_brush")),submitButton("infos", icon("refresh")),
                    DT::dataTableOutput("plot_brushed_points")),
             tabPanel("Gain par Mois",plotOutput("PROD_Repartition")),
             tabPanel("Groupe/Offres/associés", sankeyNetworkOutput("sankey")),
             tabPanel("Groupe/Comptes/Offres", sankeyNetworkOutput("sankey2")),
             tabPanel("Source/Groupe/Offres", sankeyNetworkOutput("sankey3")),
             tabPanel("Data",div(dataTableOutput("DT_win"), style = "font-size:80%"))
           )
           
           
           
    ),
    column(width = 3,
           box(width = NULL, status = "warning",
               submitButton("Appliquer la séléction", icon("refresh")),
               checkboxGroupInput("Offre", "OFFRE",
                                  choices = c("Data", "Digital Innovation", "ETM", "PocLab", "Sécurité", "Transformation"
                                  ),
                                  selected = c("Data", "Digital Innovation", "ETM", "PocLab", "Sécurité", "Transformation"
                                  )
               ),
          
              
               selectInput("annee", "Année:",
                           list("selectionne",2017,2016,2015),choices=year(today("GMT"))),
               checkboxGroupInput("Associe", "ASSOCIE",
                           choices = c(
                             "BES", "GCH", "GLE", "JDO", "JPP", "JPR", "JSO", "MBO", "MMO", "OGR", "ORE", "PDOM", "UHE", "UHL")
                           
                        
               ),
               
               
              
               p(class = "text-muted",
                 br(),
                 "Source data updates every 30 seconds."
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

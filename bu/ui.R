library(shinydashboard)
library(dplyr)
library(flexdashboard)
library(dygraphs)
library(DT)
library(timevis)

  effectifs <- c("AUDINET", "BERGER","BESNAINOU","BEYLLE","BOULAYE (de la)",
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

ui <- dashboardPage( 
  dashboardHeader(title = "PR - Augmentée"),
  dashboardSidebar(
    sidebarMenu(
      selectInput("presonne", "Choisir une personne:", 
                  choices = effectifs),
      tags$hr(),
      menuItem("Mon avis est perinant Pour", tabName = "Avis", icon = icon("users")),
      menuItem("Questionnaire Associé", tabName = "Asso", icon = icon("user-circle ")),
      menuItem("Mes réponses", tabName = "Reponses", icon = icon("camera"))
  
      )
    
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Avis",
              fluidRow(
                box(title = "Graphe des connaissances",
                    width = 12, status = "primary"
                
                
                
                )
              )
      ),
      tabItem(tabName = "Asso",
              fluidRow(
                box(width = 12, status = "primary",
                    "Ce que la personne a répondu pour elle",tags$br(),
                    infoBoxOutput("A1")
                ),
                box(
                  width = 12, status = "primary",
                  plotOutput("A1_plot",height=300)
                ),
                box(title ="Q1 - Dans quelle mesure considères-tu que les associés suivants incarnent la valeur de leadership ?",
                  width = 12, status = "primary",
                  plotOutput("A2_plot",height=300)
                ),
                box(title ="Dans quelle mesure considères-tu que les associés suivants sont porteurs de vision?",
                    width = 12, status = "primary",
                    plotOutput("A3_plot",height=300)
                ),
                box(title ="Dans quelle mesure considères-tu que les associés suivants incarnent un savoir-faire et savoir-être permettant l'origination commerciale?",
                    width = 12, status = "primary",
                    plotOutput("A4_plot",height=300)
                ),
                box(title ="Dans quelle mesure considères-tu que les associés suivants incarnent un savoir-faire et savoir-être permettant le développement des collaborateurs?",
                    width = 12, status = "primary",
                    plotOutput("A5_plot",height=300)
                ),
                box(title ="Q1 Dans quelle mesure considères-tu que les associés suivants incarnent la valeur de leadership ?",
                    width = 12, status = "primary",
                    plotOutput("A1bis_plot",height=300)
                ),
                box(title ="Q1 Dans quelle mesure considères-tu que les associés suivants incarnent la valeur de leadership ?",
                    width = 12, status = "primary",
                    plotOutput("A2bis_plot",height=300)
                ),
                box(title ="Q1 Dans quelle mesure considères-tu que les associés suivants incarnent la valeur de leadership ?",
                    width = 12, status = "primary",
                    plotOutput("A3bis_plot",height=300)
                ),
                box(title ="Q1 Dans quelle mesure considères-tu que les associés suivants incarnent la valeur de leadership ?",
                    width = 12, status = "primary",
                    plotOutput("A4bis_plot",height=300)
                ),
                box(title ="Q1 Dans quelle mesure considères-tu que les associés suivants incarnent la valeur de leadership ?",
                    width = 12, status = "primary",
                    plotOutput("A5bis_plot",height=300)
                )


                
                
              )
      ),
      tabItem(tabName = "Reponses",
                      
              navbarPage(
                title = "Les questions",
                tabPanel('Questions 1', 
                         fluidRow(
                           box(title = "Dans quelle mesure considères-tu que les personnes suivantes pensent Business Technology ?",
                               width = 12, status = "primary",
                               "Ce que la personne a répondu pour elle",tags$br(),
                               infoBoxOutput("Q1")
                           ),
                           box(
                               width = 12, status = "primary",
                               plotOutput("Q1_plot",height=300)
                           )
                           
                           )
                         ),
               
                tabPanel('Question 2', 
                         fluidRow(
                           box(title = "Dans quelle mesure considères-tu que les personnes suivantes pensent différemment ?",
                               width = 12, status = "primary",
                               infoBoxOutput("Q2")
                               
                           ),
                           box(title = "",
                               width = 12, status = "primary",
                               plotOutput("Q2_plot",height=450)
                           )
                         )
                ),
                tabPanel('Question 3', 
                         fluidRow(
                           box(title = "Dans quelle mesure considères-tu que les personnes suivantes contribuent à l'intelligence connective ?",
                               width = 12, status = "primary",
                               infoBoxOutput("Q3")
                           ),
                           box(title = "",
                               width = 12, status = "primary",
                               plotOutput("Q3_plot",height=450)
                           )
                         )
                )
              )
              )
      


    )
 )
)


dashboardPage(
  
  dashboardHeader(title = "PR - Augmentée"),
  
  dashboardSidebar(
    selectInput("consultant_id", "Sélectionner un consultant:", 
                choices = unique(questionnaire_associe[["ID_consultant"]])),
    
    menuItem("Questionnaire Associés",
             tabName = "assoradar", icon = icon("users"))
  ),
  
  dashboardBody(
    tabItems(
      
      tabItem(tabName = "assoradar",
              fluidRow(
                box(title = "Mes réponses sur cet associé:",
                    width = 6, status = "primary",
                    selectInput("associe_id", "Sélectionner un associé:", 
                                choices = unique(questionnaire_connaissance[["Associe"]]))),
                box(title = "Les réponses moyennes sur cet associé:",
                    width = 6, status = "primary",
                    textOutput("mon_vote_connaissance")),
                tags$div(
                  HTML("<strong>Les 3 couleurs utilisées <font color='#009E73'>Content</font>&nbsp;<font color='#E69F00'>Neutre</font>&nbsp;<font color='#56B4E9'>Mécontent</font></strong> ")
                )
              ),
              fluidRow(
                box(title = "Avis sur cet associé:",
                    width = 3, status = "primary",
                    plotOutput("mon_radar_asso", height = 200)),
                box(title = "Avis global BT",
                    width = 3, status = "primary",
                    plotOutput("global_radar_asso", height = 200)),
                box(title = "Avis BT le connaissant",
                    width = 3, status = "primary",
                    plotOutput("know_radar_asso", height = 200)),
                box(title = "Avis BT ne le connaissant pas",
                    width = 3, status = "primary",
                    plotOutput("dont_know_radar_asso", height = 200))
              ),
              fluidRow(title = "Il connait",
                       width = 12, status = "primary",
                       plotOutput("plotDiag", height = 800))
      ) # close tabitem
      
    ) # closes tabitems
  ) # closes dashboard body
)
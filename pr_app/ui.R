dashboardPage(
  
  dashboardHeader(title = "PR - Augmentée"),
  
  dashboardSidebar(
    selectInput("consultant_id", "Sélectionner un consultant:", 
                choices = unique(questionnaire_associe[["ID_consultant"]])),
    
    menuItem("Questionnaire Associés", 
             tabName = "associes", icon = icon("users")),
    tags$b("This text is bold."),
    menuItem("Questionnaire Associés - Alternative",
             tabName = "assoradar", icon = icon("dashboard"))
  ),
  
  dashboardBody(
    tabItems(
      
      tabItem(tabName = "associes",
              
              fluidRow(
                box(title = "Connaissance des associés",
                    width = 12, status = "primary",
                    plotOutput("connaissance_associes", height = 300))
              ),
              
              fluidRow(
                box(title = q2_str,
                    width = 12, status = "primary",
                    plotOutput("Q1_associes", height = 300))
              ),
              
              fluidRow(
                box(title = q3_str,
                    width = 12, status = "primary",
                    plotOutput("Q2_associes", height = 300))
              ),
              
              fluidRow(
                box(title = q4_str,
                    width = 12, status = "primary",
                    plotOutput("Q3_associes", height = 300))
              ),
              
              fluidRow(
                box(title = q5_str,
                    width = 12, status = "primary",
                    plotOutput("Q4_associes", height = 300))
              ),
              
              fluidRow(
                box(title = "ce que je pense globalement",
                    width = 12, status = "primary",
                    plotOutput("Global_associes", height = 300))
              ),
              
              fluidRow(
                box(title = "ce que je pense globalement",
                    width = 12, status = "primary",
                    plotOutput("Global2_associes", height = 300))
              ),
              
              fluidRow(
                box(title = "ce que je pense globalement",
                    width = 12, status = "primary",
                    plotOutput("Global3_associes", height = 300))
              )
              
      ), # closes first tabitem
      
      tabItem(tabName = "assoradar",
              fluidRow(
                box(title = "Ce que l'individu à voté",width=12,
                selectInput("associe_id", "Sélectionner un associé:", 
                            choices = unique(questionnaire_connaissance[["Associe"]])),
                
                tags$div(
                  HTML("<strong>Les 3 couleurs utilisées <font color='#009E73'>Content</font>&nbsp;<font color='#E69F00'>Neutre</font>&nbsp;<font color='#56B4E9'>Mécontent</font></strong> ")
                )
                )
              ),
              fluidRow(
                box(title = "Ce que la pesronne a voté",
                    width = 3, status = "primary",
                    plotOutput("mon_radar_asso", height = 300)),
                box(title = "Tous le monde",
                    width = 3, status = "primary",
                    plotOutput("global_radar_asso", height = 300)),
                box(title = "Ceux qui le connaisse",
                    width = 3, status = "primary",
                    plotOutput("global_radar_assoConnait", height = 300)
                    ),
                box(title = "Ceux qui le connaisse pas",
                    width = 3, status = "primary",
                    
                    
                    plotOutput("global_radar_assoConnaitPas", height = 150))
              )
              ) 
    ) # closes tabitems
  ) # closes dashboard body
)
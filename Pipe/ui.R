
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#


dashboardPage(skin = "blue",
              dashboardHeader(title = "PipeViz - BT", titleWidth = 150),
              dashboardSidebar(
                sidebarMenu(
                  
                  menuItem("Niveau du pipe", tabName = "Mediane", icon = icon("tachometer")),
                  menuItem("Montant du Pipe", tabName = "map", icon = icon("bar-chart-o")),
                  menuItem("planning People", tabName = "planning", icon = icon("calendar")),
                  menuItem("planning Mission", tabName = "planningM", icon = icon("tasks", lib = "glyphicon")),
                  tags$hr())
              ),
              dashboardBody(
                tabItems(
                  tabItem(tabName = "Mediane",
                          fluidRow(
                            checkboxGroupInput("Courbes", "courbes à afficher:",inline = TRUE,
                                               c(2015,2016),selected = "Objectif 2017"),
                            highchartOutput("hcontainer", height = "500px")
                                  )
                  ),
                  tabItem(tabName = "map",
                          fluidRow(
                            highchartOutput("hcTree")
                                   
                          )
                  ),
                  tabItem(tabName = "planning",
                          fluidRow(column(width = 11, offset = 1, style='padding:3px;'
                                         
                          ))
                  ),
                  tabItem(tabName = "planningM",
                          fluidRow(column(width = 11, offset = 1, style='padding:3px;'
                          ))
                  )
                  
                  
                )
                
                
                
                
                
                
                
              )
)



dashboardPage(skin = "black",
              dashboardHeader(title = "PipeViz - BT", titleWidth = 150,
                              tags$li(a(href = 'https://demodashboard.shinyapps.io/DB_V1_LP/',
                                        icon("power-off"),
                                        title = "Back to Apps Home"),
                                      class = "dropdown")),
              dashboardSidebar(
                sidebarMenu(
                  menuItem("Pipe en cours", tabName = "Pipe", icon = icon("tachometer")),
                  menuItem("Pipe par clients", tabName = "Clients", icon = icon("users")),
                  tags$hr(),
                  submitButton("actualiser", icon("refresh")),
                  checkboxGroupInput("uiStep1",label ="Step",
                                     choices = list("0 - A qualifier"  , "1 - Qualifiée" , "2 - A émettre" ,"3 - Emise"),
                                     
                                     selected = c("3 - Emise")),
                  checkboxGroupInput("uiAssocie",label ="Associé",
                                     choices = c("JPR","MMO","ORE","JSO","UHE","JPP","OGR"),
                                     inline = TRUE,
                                     selected = c("JPR","MMO","ORE","JSO","UHE","JPP","OGR")),
                  checkboxGroupInput("uiOffre",label ="Offre",
                                     choices = c("Transformation"  , "ETM" , "Data" ,"Digital Innovation", "PocLab","Sécurité"),
                                     inline = TRUE,
                                     selected =c("Transformation"  , "ETM" , "Data" ,"Digital Innovation", "PocLab","Sécurité"))
                )
                  
              ),
              dashboardBody(
                
                tabItems(
                  tabItem(tabName = "Pipe",
                          box(
                            title = "Répartition du Pipe>>", status = "primary", solidHeader = TRUE,
                            collapsible = TRUE,
                            plotOutput("Pipe_Repartition")
                          ),
                          
                          box(
                            title = "Répartition par step", status = "warning", solidHeader = TRUE,
                            
                            plotOutput("Pipe_Step")
                          ),
                          fluidRow( box(
                            plotOutput("Pipe_Temps")
                          ))
                         
                          
                          
                          
                  ),
                  tabItem(tabName = "Clients",
                           

                         fluidRow(
                          box(collapsible = TRUE,width =6,
                              title = "Par Offre", status = "primary", solidHeader = TRUE,
                              plotOutput("TreeMAP")),
                      
                          box(collapsible = TRUE,width =6,
                              title = "Par Associé", status = "primary", solidHeader = TRUE,
                              plotOutput("TreeMAPAss"))
                          ),
                         fluidRow(
                           box(collapsible = TRUE,width =6,
                               title = "Répartition par offre", status = "primary", solidHeader = TRUE,
                               plotOutput("Pipe_Clients")),
                          box(collapsible = TRUE,width =6,
                              title = "Répartition par associé", status = "primary", solidHeader = TRUE,
                              plotOutput("HeatMap")))
                  ),
                  tabItem(tabName = "Analyse",
                          h1("Répartition du Pipe"),
                          checkboxGroupInput("uiStep",label ="Step",
                                             choices = list("0 - A qualifier"  , "1 - Qualifiée" , "2 - A émettre" ,"3 - Emise"),
                                             inline = TRUE,
                                             selected = c("3 - Emise")),
                          D3partitionROutput("D3PartTree")
                          
                        
                          
                          
                  )
                  
                  
                  )
                  
                  
                )
                
                
              
)


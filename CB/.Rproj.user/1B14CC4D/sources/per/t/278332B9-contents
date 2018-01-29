ui <- dashboardPage(skin="blue",
        dashboardHeader(title = "Analyse CB"),
          dashboardSidebar(
                  sidebarMenu(
                    menuItem("Vision type", tabName = "flower", icon = icon("dashboard")),
                    menuItem("Par type", tabName = "dashboard", icon = icon("dashboard")),
                    menuItem("Par commerçant", tabName = "Commerces", icon = icon("line-chart")),
                    menuItem("Sur une plage de date", tabName = "Details", icon = icon("calendar")),
                    menuItem("Par nature", tabName = "nature", icon = icon("th"))
                    
                  )
                  
                  
        ),
          dashboardBody(
            tabItems(
              tabItem(tabName = "flower",
                      fluidRow(box(width=3,
                        numericInput("an", label = "Année", value =  year(today()))
                        ),
                        box(width=3,
                        numericInput("mois", label = "Mois", value = month(today())-1)
                        ),
                        box(
                            infoBoxOutput("TotalSylvie"),
                            infoBoxOutput("TotalLaurent")
                        ),
                       
                      box(width=6,
                            plotOutput("CB_plot_Nature",width = "400px", height = "400px")),
                      box(width=6,
                          plotOutput("CB_plot_Nature2",width = "400px", height = "400px")),
                      box(width=12,
                          highchartOutput("hc", height = "500px"))
                      )
              ),
                   
                     tabItem(tabName = "dashboard",
                             fluidRow(
                               
                               box(width=12,
                                   checkboxGroupInput("Type", label = "Nature", 
                                                      choices = liste_type,inline = TRUE,
                                                      selected = liste_type)
                               ),
                               box(width=12,
                                   #plotOutput("CB_plot_Nature"),
                                   highchartOutput("hcontainer", height = "500px")
                               )

                             )
                     ),
                     tabItem(tabName = "nature",
                             fluidRow(
                               box(width=12,
                                   selectInput("Type1", label = "Nature", 
                                               choices = liste_type,
                                               selected = liste_type[5]),
                                   uiOutput("Categorie")
                               ),
                               box(width=12,
                                   plotOutput("CB_plot_Categorie")
                               ),
                               box(width=12,
                                   tableOutput('tbl')
                               )
                             )
                     ),
                     tabItem(tabName = "Commerces",
                             fluidRow(
                               box(width=12,
                               chooserInput("mychooser", "Available frobs", "Selected frobs",
                                            liste_Clients$Client, c(), size = 10, multiple = TRUE
                               )),
                               valueBoxOutput("Commerce2015"),
                               valueBoxOutput("Commerce2016"),
                               valueBoxOutput("Commerce2017"),
                               box(width=12,
                                   
                                   #plotOutput("CB_plot_Commercant"),
                                   highchartOutput("hcontainer2", height = "500px")
                               ),
                               
                               box(width=12,
                                   ""
                               )
                             )
                     ),
                     tabItem(tabName = "Details",
                             fluidRow(
                               box(width=12,
                                   dateRangeInput('dateRange',
                                                  label = paste('Date pour analyse'),
                                                  start = as.Date(paste(year(Sys.Date()),"/",month(Sys.Date()),"/01",sep=""))-10,
                                                  end = as.Date(paste(year(Sys.Date()),"/",month(Sys.Date()),"/01",sep=""))+30,
                                                  separator = " - ", format = "dd/mm/yy",
                                                  startview = 'month', language = 'fr', weekstart = 1
                                   ),
                                   box(width=12,
                                       checkboxGroupInput("Type2", label = "Nature", 
                                                          choices = liste_type,inline = TRUE,
                                                          selected = liste_type)
                                   )),
                               valueBoxOutput("Comapraison2015"),
                               valueBoxOutput("Comapraison2016"),
                               valueBoxOutput("Comapraison2017"),
                               box(width=12,
                                   
                             
                                   highchartOutput("hcontainer3", height = "500px")
                               ),
                               
                               box(width=12,
                                   ""
                               )
                             )
                     )
                  )
                )
  )
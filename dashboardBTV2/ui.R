



ui <- dashboardPage( 
  dashboardHeader(title = "Dashboard - BT"),
  dashboardSidebar(
    sidebarMenu(
      tags$a(href="javascript:location.reload();", "Refresh"),
      menuItem("Chiffre d'affaire", tabName = "ca", icon = icon("eur")),
      menuItem("Pipe", tabName = "pipe", icon = icon("bar-chart-o")),
      menuItem("Miss. perdues", tabName = "lost", icon = icon("thumbs-o-down")),
      tags$hr(),
      
      menuItem("Staffing", tabName = "staff", icon = icon("tachometer")),
      
      tags$hr(),
      sliderInput("Annee", label = "Année:", min = 2015, max = 2017, value = 2017, sep = "", step = 1, ticks = FALSE),
      sliderInput("semaine", label = "Semaine:", min = 1, max = maxWeek, value = maxWeek,step = 1),
      checkboxGroupInput("uiChk1", label = "Offres", 
                         choices = list("Transformation"  , "ETM" , "Data" ,"Digital Innovation", "PocLab","Sécurité"),
                         selected = c("Transformation"  , "ETM" , "Data" ,"Digital Innovation", "PocLab","Sécurité")
      ),
      tags$hr(),
      menuItem("Chargement data", tabName = "data", icon = icon("table"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "ca",
              fluidRow(
                infoBoxOutput("ca_box"),
                infoBoxOutput("ca_production"),
                infoBoxOutput("nbMission")
              ),
              navbarPage(
                title = "Chiffre d'affaires",
                tabPanel('Evolution', 
                    fluidRow(
                  
                      
                      tabPanel('par semaine',   
                               box(textOutput("legendDivID"), title = "Legend", status = "primary", width=6),
                               box(title = "Evolution de Chiffre d'affaire en keuros en fonction des choix des offres",
                                   status = "primary",
                                   width = 12,
                                   dygraphOutput("dygraph1")
                               )
                      )
                    )
                ),
                tabPanel('Répartition', 
                         fluidRow(
                           box(title = "Liste des comptes",
                               width = 12, status = "primary",
                               sliderInput(inputId = "uiS5", label = "CA VENDU", 
                                           min = 0,max = 5000, step=100, value = c(0,500)),
                               plotOutput("ca_plot")
                           ),
                           box(title = "Données",
                               width = 12, status = "info",
                               DT::dataTableOutput('DTCABT1')
                           )
                         )
                ),
                tabPanel('Evolutions Clients',     
                         box(title = "",
                             width = 12, status = "primary",
                             htmlOutput("chartCABT")
                         )
                ),
                tabPanel('TreeMap',  
                        
                         box(title = "",
                             width = 12, status = "primary",
                         
                                                   # D3partitionROutput("D3Part1"),
                                          D3partitionROutput("D3Part2")
                             )
                             # radioButtons("radio", label = h3("Regroupement"),
                             #              choices = list("Par associé", "Par Offre", "Par groupe"), 
                             #              selected = "Par groupe",inline = TRUE),
                             # radioButtons("radio2",label = "type",
                             #              choices = list("circleTreeMap", "partitionChart", "treeMap", "sunburst",
                             #                             "collapsibleIndentedTree", "collapsibleTree"), 
                             #              selected = "sunburst",inline = TRUE)
                            
                         
                ),
                tabPanel('Comparatifs 2015-2017',     
                         
                         box(title = "Comparatif pour la semaine séléctionnée",
                             width = 12, status = "primary",
                             plotOutput("radar_CA")
                         ),
                         box(title = "Comparaison des CA BT 2015 à 2017",
                             width = 12, status = "primary",
                             plotOutput("compare_plot"),
                             selectInput("hc_theme", label = "format", 
                                         choices = list("darkunica", "fivethirtyeight","Dotabuff",
                                                        "gridlight","handdrawn","economist","financialTimes","sandsignika"), selected = "economist"),
                             
                             
                             highchartOutput("hcontainer", height = "500px")
                         ),
                         box(title = "Comparaison des CA BT 2015 à 2017 pondérée par l'effectif",
                             width = 12, status = "primary",
                             plotOutput("compare2_plot")
                         )
                )
                
              )
      ),
      tabItem(tabName = "pipe",
              fluidRow(
                fluidRow(
                  infoBoxOutput("pipe_ca"),
                  infoBoxOutput("pipe_caBT"),
                  infoBoxOutput("pipe_nb")
                  
                  
                )
              ),
              fluidRow(
                # Copy the line below to make a slider range 
                box(width = 12, sliderInput("uiS1", 
                                label = "% chance de gagner", 
                                min = 0,max = 100, value = c(0, 80)),
                    checkboxGroupInput("uiChk2",label ="Step",
                                       choices = list("0 - A qualifier"  , "1 - Qualifiée" , "2 - A émettre" ,"3 - Emise"),
                                       inline = TRUE,
                                       selected = c("3 - Emise")
                    )
                )
              ),
              fluidRow(
                navbarPage(
                  title = "Pipe",
                  tabPanel('Graphe par client',
                           box(
                               width = 12, status = "info",
                               plotOutput("pipe1_plot")
                           )
                  ),
                  tabPanel('Données',
                           box(width = 12, 
                               status = "info",
                               DT::dataTableOutput('DTtable')
                           )
                  ),
                  tabPanel('Visualisation',
                           box(
                               width = 12, status = "info",
                               plotOutput("plotDiag",height = 800)
                           )
                  ),
                  
                  tabPanel('Ecarts semaines',
                           box(
                             width = 12, status = "info",
                             checkboxGroupInput("uiModifEcart",label ="MODIF",
                                                choices = list("AJOUT"="+++"  , "SUPRESSION"="---" , "MODIF"="mod"),
                                                inline = TRUE,
                                                selected = c("+++"  , "---" , "mod")   ),
                             checkboxGroupInput("uiStepEcart",label ="STEP",
                                                choices = list("0 - A qualifier"  , "1 - Qualifiée" , "2 - A émettre" ,"3 - Emise", "4 - Gagnée", "5 - No follow","6 - En sommeil",   "7 - Perdue"   ),
                                                inline = TRUE,
                                                selected = c("0 - A qualifier"  , "1 - Qualifiée" , "2 - A émettre" ,"3 - Emise", "4 - Gagnée", "5 - No follow","6 - En sommeil",   "7 - Perdue"   )
                           )),
                           DT::dataTableOutput('DTPIPEEcarts')
                  ),
                  
                  tabPanel('Transformation du pipe',
                           box(title = "Evolution du Pipe entre les semaines 1 à 52",
                               width = 12, status = "info",
                               sliderInput("uiS6", label = "N° Semaine", min = 1, 
                                           max = 52, value = c(2, 52)),
                               p("Processus utilisé:"),
                               p("On regarde les lignes dans le pipe entre les semaines en fonciton du pourcentage de Prob"),
                               p("A gauche le % en entrée. A droite évolution du %. La popup affiche le CA
                                 sur la semaine de fin.", 
                                 style = "font-family: 'times'; font-si8pt"),
                               "Données brutes",
                               htmlOutput("chartPipe3"),
                               DT::dataTableOutput('DTPipe2')
                           )
                           )))
              
      ),
      tabItem(tabName = "lost",
              fluidRow(
                infoBoxOutput("lost1",2 * 2),
                infoBoxOutput("lost2",2 * 2),
                
                    checkboxGroupInput("uiChk3", label = "", 
                                       choices = list("6 - En sommeil", "7 - Perdue", "5 - No follow"),
                                       selected = c("7 - Perdue"),
                                       inline = TRUE
                    ),
                    sliderInput("uiS2", 
                                label = "", 
                                min = 0,max = 900, value = c(0, 900))
                
              ),
              fluidRow(plotOutput("lost_plot"),
                       DT::dataTableOutput('DTlost'))
      ),
      tabItem(tabName = "data", 
              fluidRow(
                fileInput('file1', 'Ajouter les données de la semaine',
                          accept=c('application/vnd.ms-excel',
                                   'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet',
                                   '.xls',
                                   '.xlsx')),
                tags$hr(),
                tableOutput("contents")
                
              )),
      tabItem(tabName = "staff",
              fluidRow(
                infoBoxOutput("saff1_box"),
                infoBoxOutput("saff2_box"),
                infoBoxOutput("saff3_box"),
                box(title = "Mois & grades",
                    width =12, status = "info",
                    solidHeader = TRUE,
                    sliderInput(inputId = "uiS3", label = "Mois", 
                                min = 1,max = 12, step=1, value = c(1, 12) ),
                    checkboxGroupInput("uiChk4", label = "", 
                                       choices = list("2-C", "3-CC", "4-CS", "5-MNG", "6-SM", "7-DIR", "8-ASS", "A1-AE", "A2-AES", "D1-DS1", "D3-DS3"),
                                       selected = c("2-C", "3-CC", "4-CS", "5-MNG", "6-SM", "7-DIR", "8-ASS", "A1-AE", "A2-AES", "D1-DS1", "D3-DS3"),
                                       inline = TRUE
                    )
                )
              ),
              
              navbarPage(
                title = "Staffing",
                tabPanel('Planning People',
                         box(title = "planning",
                             width =12,
                             selectInput("selUI1", 'ctl+A pour tous', people, multiple=TRUE, size=10, selected=people[1,1],	selectize=FALSE),
                             timevisOutput("timelineGroups")
                         )
                ),
                tabPanel('Planning Mission',
                         box(title = "planning",
                            
                             width =12,
                             selectInput("selUI2", 'ctl+A pour tous', totem, multiple=TRUE, size=10, selected=totem[1,1],	selectize=FALSE),
                             
                             timevisOutput("timelineGroupsTOTEM")
                         )
                ),
                tabPanel('Disponibilités',
                         box(title = "Taux de staffing (ferme + congès) ",
                             width =12, status = "info",
                             sliderInput(inputId = "uiS4", label = "Filte de % staffing sur le mois courant", 
                                         min = 0,max = 1, step=0.1, value = 0.5 ),
                             actionButton("btn1", label="Consultants"),
                             actionButton("btn2", label="ADM"),
                             actionButton("btn3", label="Architectes"),
                             actionButton("btn4", label="Data scientists"),
                             plotOutput("staff4_plot", height=450)
                         )
                ),
                tabPanel('Modifictations',
                         box(title = "planning",
                             width =12,
                             "  1 = 'Ferme',2	=	'Prévi',3	=	'CONGES',4	=	'Formation',5	=	'Activités Internes ou à affecter', 6	=	'Arrêt maladie', 7	=	'Inactivité'",
                             selectInput("Staff", 'ctl+A pour tous', Type_Staffing, multiple=TRUE, size=7, selected=Type_Staffing
                                         ,	selectize=FALSE)
                             
                         ),
                         DT::dataTableOutput('DTSTAFF')
                )
              )
      )
    )
  )
)

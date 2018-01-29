

ui <- dashboardPage(
  dashboardHeader(title = "Ecarts Pipe-Staffing"),
  dashboardSidebar(
    sidebarMenu(
      "Staffing",
      menuItem("Indicateurs", tabName = "Indicateurs", icon = icon("tachometer")),
      menuItem("Détails", tabName = "Detail", icon = icon("table")),
      menuItem("Chargement Staffing", tabName = "upload", icon = icon("file-excel-o ")),
      tags$hr(),
      "Pipe",
      menuItem("Indicateurs", tabName = "IndicateursP", icon = icon("tachometer")),
      menuItem("Détails", tabName = "DetailP", icon = icon("table")),
      menuItem("Chargement Pipe", tabName = "uploadP", icon = icon("file-excel-o ")),
      tags$hr(),
      menuItem("Logs", tabName = "Histo", icon = icon("history"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Detail",
              
              
        box(title = "Ecarts Staffing",
                       width =12,
                       checkboxGroupInput("uiModifEcart",label ="MODIF",
                                          choices = list("AJOUT"="+++"  , "SUPRESSION"="---" , "MODIF"="mod"),
                                          inline = TRUE,
                                          selected = c("+++"  , "---" , "mod")   ),
                       "  1 = 'Ferme',2	=	'Prévi',3	=	'CONGES',4	=	'Formation',5	=	'Activités Internes ou à affecter', 6	=	'Arrêt maladie', 7	=	'Inactivité'",
                       selectInput("Staff", 'ctl+A pour tous', Type_Staffing, multiple=TRUE, size=7, selected=Type_Staffing
                                   ,	selectize=FALSE)

                   ),
        downloadButton("downloadData", label = "Télécharger le fichier"),
        
                   DT::dataTableOutput('DTSTAFF'),
        htmlOutput('StaffText'),
        DT::dataTableOutput('DTSTAFFING')
      ),
      tabItem(tabName = "DetailP",
      box(title = "Ecarts Pipe",
          width =12,
          checkboxGroupInput("EcartP",label ="MODIF",
                             choices = list("AJOUT"="+++"  , "SUPRESSION"="---" , "MODIF"="mod"),
                             inline = TRUE,
                             selected = c("+++"  , "---" , "mod")   ),
          
          selectInput("STEP", 'ctl+A pour tous', STEPS , multiple=TRUE, size=7, selected=STEPS
                      ,	selectize=FALSE),
          
          # Copy the line below to make a number input box into the UI.
          numericInput("numDELTA", label = "Filter sur le DELTA en valeur absolue (inférieur)", value = 1)
          # This outputs the dynamic UI component
          #uiOutput("ui")
          
      ),
      htmlOutput('PipeText'),
      DT::dataTableOutput('DTPIPE')
    ),
      tabItem(tabName = "IndicateursP",
              fluidRow(
                infoBoxOutput("ModP"),
                infoBoxOutput("AddP"),
                infoBoxOutput("DelP"),
                infoBoxOutput("Mod_TOTALP"),
                infoBoxOutput("Add_TOTALP"),
                infoBoxOutput("Del_TOTALP"),
                box(
                  width = 12, status = "info",
                  plotOutput("Pipe_plot")
                )
              )),
      tabItem(tabName = "Indicateurs",
              fluidRow(
              infoBoxOutput("Mod"),
              infoBoxOutput("Add"),
              infoBoxOutput("Del"),
              infoBoxOutput("Mod_TOTAL"),
              infoBoxOutput("Add_TOTAL"),
              infoBoxOutput("Del_TOTAL"),
              box(
                             width = 12, status = "info",
                             plotOutput("Staffing_plot")
                           )
              )),
    tabItem(tabName = "upload",
            fluidRow(
              box(title = "Charger un nouveau fichier XLS de staffing  de référence",
                  width =12,
                  fileInput('Staffing', 'Choisir le XLS staffing', accept = '.xlsx')
                  
              ),
              box(title = "Rechargement du fichier Google",
                  width =12,
                  actionButton("go", "Upload données Google"),
                  verbatimTextOutput("value")
              ),
              box(title = "Chargement XLSX",
                  width =12,
                  tableOutput('contents')
              )
            )
    ),
            tabItem(tabName = "uploadP",
                    fluidRow(
                    box(title = "Charger un nouveau fichier XLS de Pipe  de référence",
                        width =12,
                        fileInput('Pipe', 'Choisir le XLS Pipe')
                    ),
                    box(title = "Rechargement du fichier Google",
                        width =12,
                        actionButton("goP", "Upload données Google"),
                        verbatimTextOutput("valueP")
                    ),
                    box(title = "Chargement XLSX",
                        width =12,
                        tableOutput('contents2')
                    )
                    )
            ),
            tabItem(tabName = "Histo",
            fluidRow(
              box(title = "Récupérations Pipe et Staffing sur GDrive effectuées",
                  width =6,
                  DT::dataTableOutput('LogPIPE')
                  
              ),
              box(title = "Chargements de fichiers Pipe & Staffing effectués",
                  width =6,
                  DT::dataTableOutput('LogSTAFF')
              )
            )
    )
      )
  )
)
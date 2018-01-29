# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Define UI for application
shinyUI(
  
  # Include a fliudPage above the navbar to incorporate a icon in the header
  # Source: http://stackoverflow.com/a/24764483
  fluidPage(theme = shinytheme("united"),
    
    list(tags$head(HTML('<link rel="icon", href="logo.png",                        type="image/png" />'))),
    div(style="padding: 1px 0px; width: '100%'",
        titlePanel(
          title="", windowTitle="Window Tab title"
        )
    ),
    
    navbarPage(title=div(img(src="Rlogo.png"), "BT VIZ"),
               inverse = F, # for diff color view
 
               tabPanel("Production BT", icon = icon("home"),
                        
                         #jumbotron("Dataviz BT","ee"),
                        wells(content =textOutput("Jumbo"),
                              size = "small"),

                        fluidRow(
                          
                          column(4,  panel_div(class_type = "primary", panel_title = "Vente BT en k€",
                                              content = highchartOutput("hcPie1"))),
                          column(4, panel_div(class_type = "primary", "Production BT en k€",
                                              content = highchartOutput("hcPie2"))),
                          column(4, panel_div("success", "Pipe BT en k€",
                                              content = highchartOutput("hcPie3")))
                        ),  # end of fluidRow
                        fluidRow(
                          column(12, 
                          dataTableOutput('tbl')
                         # plotOutput('distPlot')
                          ))
                        ),
               tabPanel('CA evolutions', icon = icon("line-chart"),
                        fluidRow(
                          
                          checkboxGroupInput("Courbes", "courbes à afficher:",inline = TRUE,
                                             c(2015,2016),selected = "Objectif 2017"),
                        
                          highchartOutput("hcontainer", height = "500px"),
                          h1("Ecart des predictions par rapport aux CA par semaine"),
                          plotOutput("Residuals_plot"),
                          plotOutput("Previ_plot")
                          
                        )
               ),
               tabPanel("Staffing", icon = icon("users"),
                        wells(content = "Basés sur les données extraites du dashboard BT Excel",
                              size = "default"),
                        h1("Staffing BT", align = "center"),
                        fluidRow(
                          column(6, panel_div("info", "Staffing", 
                                              content = highchartOutput("myGauge"))),
                          box(
                            title = "Détail Staffing",width=6,
                            background = "green",
                            
                            plotOutput("Staff_plot"),
                            checkboxGroupInput("GRADE", label = "Grade", 
                                               choices = list("0-Global","1-STA", "2-C", "3-CC", "4-CS", "5-MNG", "6-SM", "7-DIR", "8-ASS",
                                                              "A1-AE", "A2-AES", "D1-DS1", "D3-DS3"),
                                               selected = "0-Global",
                                               inline = TRUE
                                               
                            )),
                          
                          #### FAVICON TAGS SECTION ####
                          tags$head(tags$link(rel="shortcut icon", href="favicon.ico")),
                          
                          #### JAVASCRIPT TAGS SECTION #### - ENABLE WHEN READY
                          #tags$head(tags$script(src='pl.js')),
                          
                          bsModal("modalExample", "Evolution du CA BT", "tabBut", size = "medium" ,
                                  p("Comparaison avec 2015 et 2016")
                                  
                          )
                          
                        ),
                        
                        list_group(c(list_item("Application V0", badge_value = 27),
                                     list_item("@Aout 2017", badge_value = 24)))
                        
               ),
               
               tabPanel("Applications", icon = icon("desktop"),
                        hr(),
                        fluidRow(
                          column(4, thumbnail_label(image = 'Staff.jpg', label = 'Dataviz Staffing',
                                                    content = 'Détail sur le staffing par nom et planning associés',
                                                    button_link = 'https://demodashboard.shinyapps.io/DB_V1/', button_label = 'Click me')
                          ),
                          column(4, thumbnail_label(image = 'Prod.jpg', label = 'Dataviz Production',
                                                    content = 'Détails sur les missions vendues. ',
                                                    button_link = 'https://demodashboard.shinyapps.io/DB_V1_PROD/', button_label = 'Click me')),
                          column(4, thumbnail_label(image = 'prospects.jpg', label = 'Dataviz Pipe',
                                                    content = 'Analyse sur le pipe en cours. ',
                                                    button_link = 'https://demodashboard.shinyapps.io/DB_V1_Pipe/', button_label = 'Click me'))
                          
                          )))
    
                          )) # end of shiny
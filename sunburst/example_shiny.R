library(shiny)
library(sunburstR)
library(dplyr)

sequences <- read.csv(
  system.file("examples/visit-sequences.csv",package="sunburstR")
  ,header=F
  ,stringsAsFactors = FALSE
)
seq2 <- pilotage_data %>% filter (STEP == "4 - Gagnée",WEEK==6) %>%
  group_by(OFFRE_PRINCIPALE,ASSOCIE,SECTEUR,GROUPE, COMPTE) %>%
  summarize(CA = sum(CA_BT__N__KE,na.rm=TRUE)) %>% 
  mutate (V1= paste(ASSOCIE,OFFRE_PRINCIPALE,SECTEUR,GROUPE,COMPTE,sep="-")) %>%
  mutate (V2= round(CA,0)) %>% ungroup() %>%
  select(V1,V2)
sum()
# constitution des donnes sunburstR 
server <- function(input,output,session){
  
  output$sunburst <- renderSunburst({
    #invalidateLater(1000, session)
    add_shiny(
#       sunburst(seq2, percent=FALSE, count = TRUE, height = "750", 
#                        width = "100%",
#                        explanation = "function(d){ return (d.name + ' '  + d.size)}"
# )
sunburst(
  seq2,
  explanation = 
    '
function(d){return d.value}'
  
)

)
  })
  
  
  selection <- reactive({
    input$sunburst_mouseover
  })
  
  output$selection <- renderText(selection())
}


ui  <-  fluidPage(
  sidebarLayout( "demo,",
    sunburstOutput("sunburst"),
    textOutput("selection")
  )
)



shinyApp(ui = ui, server = server)



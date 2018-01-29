library(shiny)
library(readxl)
library(dplyr)
library(stringr)
library(lubridate) # gestion des dates

runApp(
  list(
    ui = fluidPage(
      titlePanel("Use readxl"),
      sidebarLayout(
        sidebarPanel(
          fileInput('file1', 'Choose xlsx file',
                    accept = c(".xlsx")
          )
        ),
        mainPanel(
          tableOutput('contents'))
      )
    ),
    server = function(input, output){
      output$contents <- renderTable({
        inFile <- input$file1
        
        if(is.null(inFile))
          return(NULL)
        file.rename(inFile$datapath,
                    paste(inFile$datapath, ".xlsx", sep=""))
        fichier <- paste(inFile$datapath, ".xlsx", sep="") 
        data1 <- read_excel(fichier, col_types = c(
          rep("text", 6),
          "numeric",
          rep("text", 2),
          rep("numeric", 6),
          "text",
          "date",
          rep("text", 3),
          rep("numeric", 2),
          rep("date", 5)))
        
        nb <- str_sub(fichier,22)
        fin <- str_locate(nb,".xlsx")[1]
        nb <-  as.numeric(str_sub(nb,1,fin-1))
        #on ajoute la semaine et aussi la date de début de la semaine
        date_ref <- data_frame(
          date_ref = seq(from = dmy("01/01/2017"), to = dmy("31/12/2017"), by = "weeks"),
          week = week(date_ref)
        )
        data1 %>% mutate(week=nb) -> pilotage_data_tmp
        pilotage_data_tmp %>% 
          inner_join(date_ref, by = "week")
      })
    }
  )
)
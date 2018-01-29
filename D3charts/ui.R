
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
library(shiny)
library(D3partitionR)
library(data.table)
library(highcharter)
require(shinydashboard)
body<-dashboardBody(fluidPage(
  h2("Exploring Japan trade from 1988 to 2015",align="center",style="font-variant: small-caps;"),
  tabBox(width = 12,
         tabPanel( 
           
           "Export and import by country",fluidRow(
             box(width=12,title="Options",solidHeader = T,status = "primary",
                 column(3, sliderInput("DateRange1", "Time selection:",min = 1988, max = 2015, value = c(1988, 2015))),
                 column(width=3,radioButtons("exchangeToShow",label="Exchanges to show",choices=c("import","export"))))
             ,box(D3partitionROutput("D3Part1"),width = 6,height = 800),
             box(highchartOutput("Graph",height = "600px"),width=6)
           )
         ),
         tabPanel(
           "Export and import by type of product",fluidRow(
             box(width=12,title="Options",solidHeader = T,status = "primary",
                 column(3, sliderInput("DateRange2", "Time selection:",min = 1988, max = 2015, value = c(1988, 2015))),
                 column(width=3,radioButtons("exchangeToShow2",label="Exchanges to show",choices=c("import","export"))))
             ,box(solidHeader=T,D3partitionROutput("D3Part2"),width = 6,height = 700)
            
           )
         )
  )
)
)
dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(disable = TRUE),
  body
)
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
library(shiny)
library(D3partitionR)
library(data.table)
library(highcharter)
require(shinydashboard)
#year_latest <- data.table(read.csv("~/PackageDev/D3partitionJapanTrade/japan-trade-statistics/year_latest.csv"))
#hs2_eng <- data.table(read.csv("~/PackageDev/D3partitionJapanTrade/japan-trade-statistics/hs2_eng.csv"))
#hs4_eng <- data.table(read.csv("~/PackageDev/D3partitionJapanTrade/japan-trade-statistics/hs4_eng.csv"))
#hs6_eng <- data.table(read.csv("~/PackageDev/D3partitionJapanTrade/japan-trade-statistics/hs6_eng.csv"))
#country_eng <- data.table(read.csv("~/PackageDev/D3partitionJapanTrade/japan-trade-statistics/country_eng.csv"))
#year_latest <- merge(year_latest,hs2_eng,by="hs2")
#year_latest <- merge(year_latest,hs4_eng,by="hs4")
#year_latest <- merge(year_latest,hs6_eng,by="hs6")
#year_latest <- merge(year_latest,country_eng,by="Country")
year_latest<-readRDS("data_proc2.RDS")
year_latest_proc<-year_latest[,.(hs2_name,hs4_name,hs6_name,Country_name,Area,Year,VY,exp_imp)]
year_latest_proc_year<-year_latest[,.(Value=sum(VY)),by=c("Country_name","Area","Year","hs2_name","exp_imp")]
year_latest_proc_year[,tot_value:=sum(Value),by=c("Country_name","Area","hs2_name","exp_imp")]
year_latest_proc_year[,prev_value:=sum(Value),by=c("Country_name","Area","exp_imp")]
year_latest_proc_year[tot_value/prev_value<0.02,hs2_name:="Other"]
year_latest_proc_year<-unique(year_latest_proc_year[,.(Value=sum(Value)),by=c("Country_name","Area","hs2_name","Year","exp_imp")])
year_latest_proc_year[,path_str:=paste(paste("World",Area,Country_name,sep="/"),hs2_name)]
year_latest_proc_year[,path:=strsplit(path_str,"/")]
year_latest_proc_year2<-year_latest[,.(Value=sum(VY)),by=c("Country_name","Area","Year","hs2_name","exp_imp")]
year_latest_proc_year2[,tot_value:=sum(Value),by=c("Country_name","Area","hs2_name","exp_imp")]
year_latest_proc_year2[,tot_value_hs2:=sum(Value)]
year_latest_proc_year2[,value_hs2:=sum(Value),by=c("hs2_name")]
year_latest_proc_year2[value_hs2/tot_value_hs2<0.02,hs2_name:="Other"]
year_latest_proc_year2<-unique(year_latest_proc_year2[,.(Value=sum(Value)),by=c("Country_name","Area","hs2_name","Year","exp_imp")])
year_latest_proc_year2[,path_str:=paste(paste("Trade",hs2_name,Area,sep="/"),Country_name)]
year_latest_proc_year2[,path:=strsplit(path_str,"/")]
shinyServer(function(input, output) {
  
  year_latest_proc_noyear_reac<-reactive({
    year_latest_proc_noyear<-unique(year_latest[Year>=input$DateRange1[1] & Year <=input$DateRange1[2],.(Value=sum(VY)),by=c("Country_name","Area","hs2_name","exp_imp")])
    year_latest_proc_noyear[,prev_value:=sum(Value),by=c("Country_name","Area","exp_imp")]
    year_latest_proc_noyear[Value/prev_value<0.02,hs2_name:="Other"]
    year_latest_proc_noyear<-unique(year_latest_proc_noyear[,.(Value=sum(Value)),by=c("Country_name","Area","hs2_name","exp_imp")])
    year_latest_proc_noyear[,path_str:=paste("World",Area,Country_name,hs2_name,sep="/")]
    year_latest_proc_noyear[,path:=strsplit(path_str,"/")]
    year_latest_proc_noyear[eval(parse(text=exchangesType())),.(Value=sum(Value),path),by=c("Country_name","Area","hs2_name","path_str")]
  })
  
  
  year_latest_proc_noyear2_reac<-reactive({
    year_latest_proc_noyear2<-year_latest[Year>=input$DateRange2[1] & Year <=input$DateRange2[2],.(Value=sum(VY)),by=c("Country_name","Area","hs2_name","exp_imp")]
    year_latest_proc_noyear2[,tot_value:=sum(Value),by=c("Country_name","Area","hs2_name","exp_imp")]
    year_latest_proc_noyear2[,tot_value_hs2:=sum(Value)]
    year_latest_proc_noyear2[,value_hs2:=sum(Value),by=c("hs2_name")]
    year_latest_proc_noyear2[value_hs2/tot_value_hs2<0.01,hs2_name:="Other"]
    year_latest_proc_noyear2<-unique(year_latest_proc_noyear2[,.(Value=sum(Value)),by=c("Country_name","Area","hs2_name","exp_imp")])
    year_latest_proc_noyear2[,path_str:=paste("Trade",hs2_name,Area,Country_name,sep="/")]
    year_latest_proc_noyear2[,path:=strsplit(path_str,"/")]
    year_latest_proc_noyear2[eval(parse(text=exchangesType2())),.(Value=sum(Value),path),by=c("Country_name","Area","hs2_name","path_str")]
  })
  
  output$D3Part1 <- renderD3partitionR(
    D3partitionR(data =list(path=year_latest_proc_noyear_reac()$path,
                            value=year_latest_proc_noyear_reac()$Value),
                 Input=list(enabled=T,Id="D3Part1",
                            clickedStep=T,currentPath=T,
                            visiblePaths=T,visibleLeaf=T,visibleNode=T),width = 600,height = 600))
  
  output$Graph<-renderHighchart(
    hchart(unique(year_latest_proc_year[path_str%like%input$D3Part1$clickedStep & eval(parse(text=exchangesType())) & Year>=input$DateRange1[1] & Year <=input$DateRange1[2],.(Value=sum(Value)),by=c("Year","hs2_name")][order(Year)]), "line", x = Year, y = Value, group = hs2_name)
  )
  
  output$Graph2<-renderHighchart(
    if (length(input$D3Part2$currentPath)==1)
      
      hchart(unique(year_latest_proc_year2[Year>=input$DateRange2[1] & Year <=input$DateRange2[2] & 
                                             path_str%like%input$D3Part2$currentPath & 
                                             eval(parse(text=exchangesType2())),.(Value=sum(Value)),by=c("Year","hs2_name")][order(Year)]), 
             "line", x = Year, y = Value, group = hs2_name)
    else if (length(input$D3Part2$currentPath)==2)
      hchart(unique(year_latest_proc_year2[Year>=input$DateRange2[1] & Year <=input$DateRange2[2] &
                                             path_str%like%input$D3Part2$currentPath & 
                                             eval(parse(text=exchangesType2())),.(Value=sum(Value)),by=c("Year","Area")][order(Year)]), 
             "line", x = Year, y = Value, group = Area)
    else if (length(input$D3Part2$currentPath)==3)
      hchart(unique(year_latest_proc_year2[Year>=input$DateRange2[1] & Year <=input$DateRange2[2] & path_str%like%input$D3Part2$currentPath & eval(parse(text=exchangesType2())),.(Value=sum(Value)),by=c("Year","Country_name")][order(Year)]), "line", x = Year, y = Value, group = Country_name)
  )
  
  output$D3Part2 <- renderD3partitionR(
    D3partitionR(data =list(path=year_latest_proc_noyear2_reac()$path,
                            value=year_latest_proc_noyear2_reac()$Value),
                 type="treeMap",Input=list(enabled=T,Id="D3Part2",clickedStep=T,
                                           currentPath=T,visiblePaths=T,visibleLeaf=T,visibleNode=T),width = 700,height = 600))
  
  
  exchangesType<-reactive({
    switch(input$exchangeToShow,
           "import"="exp_imp==2",
           "export"="exp_imp==1"
    )
  })
  
  exchangesType2<-reactive({
    switch(input$exchangeToShow2,
           "import"="exp_imp==2",
           "export"="exp_imp==1"
    )
  })
})
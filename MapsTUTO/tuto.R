# Load packages
library("rgdal")
library("rgeos")

setwd("D:/tmp/MapsTUTO/data/")
# Load the output area shapefiles
Output.Areas<- readOGR(".", "Camden_oa11")


Census.Data <-read.csv("practicaldata.csv")

# plots the shapefile
plot(Output.Areas)


# joins data to the shapefile
OA.Census <- merge(Output.Areas, Census.Data, by.x="OA11CD", by.y="OA")


# loads packages
library(tmap)
library(leaflet)
library(RColorBrewer)

# this will prodyce a quick map of our qualification variable
qtm(OA.Census, fill = "Qualification")

# Creates a simple choropleth map of our qualification variable
tm_shape(OA.Census) + tm_fill("Qualification") 


# setting a colour palette
tm_shape(OA.Census) + tm_fill("Qualification", palette = "-Greens")  + 
  tm_borders(alpha=.4) +
  tm_compass() + 
  tm_layout(title = "Camden, London", legend.text.size = 1.1, legend.title.size = 1.4, legend.position = c("right", "top"), frame = FALSE) 



writeOGR(OA.Census, dsn = "D:/tmp/MapsTUTO", layer =  "Census_OA_Shapefile", driver="ESRI Shapefile")

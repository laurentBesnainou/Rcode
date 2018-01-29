#integration de Leaflet pour positionner les consultants
library(shiny)
library (ggmap)
library(leaflet) 
shinyApp(
  ui = fluidPage(leafletOutput('myMap')),
  server = function(input, output) {
    
    # download and load data
    url <-  "https://github.com/Robinlovelace/sdvwR/raw/master/data/gps-trace.gpx"
    download.file(url, destfile = "shef2leeds.gpx" )
      library(rgdal)
    shef2leeds <- readOGR("shef2leeds.gpx", layer = "tracks")
    
    map = leaflet() %>% addTiles() %>% setView(-1.5, 53.4, 9) %>% 
      addPolylines(data = shef2leeds, color = "red", weight = 4)
    output$myMap = renderLeaflet(map)
  }
)

m <- leaflet() %>% addTiles()

img <- readPNG("~/repos/Creating-maps-in-R/figure//shiny_world.png")
grid.raster(img)

m %>% setView(lng = -1.5, lat = 53.4, zoom = 10) # set centre and extent of map


m2 <- m %>%
  setView(-1.5, 53.4, 10) %>% # map location
  addMarkers(-1.4, 53.5) %>% # add a marker
  addPopups(-1.6, 53.3, popup = "Hello Sheffield!") %>% # popup
  # add som circles:
  addCircles(color = "black", runif(90, -2, -1), runif(90, 53, 54), runif(90, 10, 500))
(m2)

#Fonction ggmap
poi <- data.frame(nom = c('Bscc',
                          'SKILOC JAY SPORT'),
                  adresses = c("La poste, saint quentin en yvelines",
                               'Immeuble Samoens 1600, Grande Rue, 74340 Samoens'), 
                  stringsAsFactors = F)

poigeo <- geocode(poi$adresses, output = "latlona")
geoPOI <- data.frame(poi,poigeo)

# affichage des points d'intérêt sur la carte
m <- leaflet(data = geoPOI ) %>% addTiles() %>% addMarkers(~lon, ~lat,  popup = ~as.character(nom))
(m)



library(leaflet)

icon.glyphicon <- makeAwesomeIcon(icon= 'flag', markerColor = 'blue',
                                  iconColor = 'black', library = 'glyphicon')
icon.fa <- makeAwesomeIcon(icon = 'flag', markerColor = 'red', library='fa',
                           iconColor = 'black')
icon.ion <- makeAwesomeIcon(icon = 'home', markerColor = 'green',
                            library='ion')


# Marker + Label
leaflet() %>% addTiles() %>%
  addAwesomeMarkers(
    lng=-118.456554, lat=34.078039,
    label='This is a label'
    )

leaflet() %>% addTiles() %>%
  addAwesomeMarkers(
    lng=-118.456554, lat=34.078039,
    label='This is a label',
    icon = icon.fa)

leaflet() %>% addTiles() %>%
  addAwesomeMarkers(
    lng=-118.456554, lat=34.078039,
    label='This is a label',
    icon = icon.ion)

# Marker + Static Label using custom label options
leaflet() %>% addTiles() %>%
  addAwesomeMarkers(
    lng=-118.456554, lat=34.078039,
    label='This is a static label',
    labelOptions = labelOptions(noHide = T),
    icon = icon.fa)


cities <- read.csv(textConnection("
                                  City,Lat,Long,Pop
                                  Boston,42.3601,-71.0589,645966
                                  Hartford,41.7627,-72.6743,125017
                                  New York City,40.7127,-74.0059,8406000
                                  Philadelphia,39.9500,-75.1667,1553000
                                  Pittsburgh,40.4397,-79.9764,305841
                                  Providence,41.8236,-71.4222,177994
                                  "))

library(dplyr)
cities <- cities %>% mutate(PopCat=ifelse(Pop <500000,'blue','red'))


leaflet(cities) %>% addTiles() %>%
  addAwesomeMarkers(lng = ~Long, lat = ~Lat,
                    label = ~City,
                    icon = icon.ion)

icon.pop <- awesomeIcons(icon = 'users',
                         markerColor = ifelse(cities$Pop <500000,'blue','red'),
                         library = 'fa',
                         iconColor = 'black')

leaflet(cities) %>% addTiles() %>%
  addAwesomeMarkers(lng = ~Long, lat = ~Lat,
                    label = ~City,
                    icon = icon.pop)

# Make a list of icons (from two different icon libraries).
# We'll index into it based on name.
popIcons <- awesomeIconList(
  blue = makeAwesomeIcon(icon='user', library='glyphicon', markerColor = 'blue'),
  red = makeAwesomeIcon(icon='users', library='fa', markerColor = 'red'))

m = leaflet() %>% addTiles()
m  # a map with the default OSM tile layer

m = m %>% setView(-93.65, 42.0285, zoom = 17)
m

m %>% addPopups(-93.65, 42.0285, 'Here is the <b>Department of Statistics</b>, ISU')
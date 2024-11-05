#options(timeout = 600)  # Zwiększenie limitu czasu do 600 sekund
#install.packages("sf")
#install.packages("terra")

library(leaflet)
library(sf)
library(terra)
library(dplyr)
library(ggmap)
library(tidygeocoder)
library(data.table)
library(htmltools)
library(mapview)
library(webshot)
library(htmlwidgets)

m <- leaflet()

m <- leaflet() %>% 
  addTiles() %>% 
  setView(lng = 20, lat = 52, zoom = 6)
m


m <- leaflet() %>% 
  addProviderTiles(providers$Stadia.Outdoors) %>% 
  setView(lng = -150, lat = 64, zoom = 4)
m
m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldTerrain) %>% 
  setView(lng = -150, lat = 64, zoom = 4)
m

ak_counties <- st_read("R Leaflet/data/tl_2013_02_cousub/tl_2013_02_cousub.shp")

m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldTerrain) %>% 
  setView(lng = -150, lat = 64, zoom = 4) %>% 
  addPolygons(data = ak_counties, 
              color = "#660000",
              weight = 1,
              smoothFactor = 0.5) #wartość generalizacji
m

fbi_data <- readRDS("R Leaflet/data/database.rds")

ak <- filter(fbi_data, State == "Alaska")
ak <- mutate(ak, adress = paste(City, State, "United States"))
adress <- unique(ak$adress)

adresses_data <- data.table(adress)
geocodes <- adresses_data %>% geocode(address = adress, method = "osm")

geocodes <- adresses_data %>% 
  geocode_combine(queries = list(
    list(method = "census"),
    list(method = "osm")
  ),
  global_params = list(address = "adresses"),
  cascade = FALSE
)

geocodes <- filter(geocodes, query == "osm")
geocodes <- geocodes[-4]

rm(adresses_data)

ak <- left_join(ak, geocodes, by = "adress")

ak$lat <- jitter(ak$lat, factor = 1) #wprowadzenie szumu, żeby punkty nie nakładały się na siebie
ak$long <- jitter(ak$long, factor = 1)

ak_unsolved <- ak %>% 
  filter(Crime.Type == "Murder or Manslaughter", 
         Crime.Solved == "No")

ak_unsolved$label <- paste("<p>", "City: ", ak_unsolved$City, "</p>",
                           "<p>", "Month: ", ak_unsolved$Month, "</p>",
                           "<p>", "Victim Sex: ", ak_unsolved$Victim.Sex, "</p>",
                           "<p>", "Victim Race: ", ak_unsolved$Victim.Race, "</p>",
                           "<p>", "Weapon: ", ak_unsolved$Weapon, "</p>",
                           "<p>", "Year: ", ak_unsolved$Year, "</p>")

m <- leaflet() %>% 
  addProviderTiles(providers$OpenStreetMap) %>% 
  setView(lng = -150, lat = 64, zoom = 4) %>% 
  addPolygons(data = ak_counties, 
              color = "#660000",
              weight = 1,
              smoothFactor = 0.5) %>%  #wartość generalizacji
  addCircleMarkers(lng = ak_unsolved$long, 
                   lat = ak_unsolved$lat,
                   color = "#ffffffff",
                   weight = 1,
                   radius = 5,
                   label = lapply(ak_unsolved$label, HTML),
                   clusterOptions = markerClusterOptions(
                     showCoverageOnHover = FALSE)) #usuwa niebieskie poligony
m

ak_solved <- ak %>% 
  filter(ak$Crime.Solved == "Yes", ak$Crime.Type == "Murder or Manslaughter")

ak_solved$label <- paste("<p>", "City: ", ak_solved$City, "</p>",
                         "<p>", "Month: ", ak_solved$Month, "</p>",
                         "<p>", "Victim Sex: ", ak_solved$Victim.Sex, "</p>",
                         "<p>", "Victim Race: ", ak_solved$Victim.Race, "</p>",
                         "<p>", "Weapon: ", ak_solved$Weapon, "</p>",
                         "<p>", "Year: ", ak_solved$Year, "</p>")


m <- leaflet() %>% 
  addProviderTiles(providers$OpenStreetMap) %>% 
  setView(lng = -150, lat = 64, zoom = 4) %>% 
  addPolygons(data = ak_counties, 
              color = "#660000",
              weight = 1,
              smoothFactor = 0.5) %>%  #wartość generalizacji
  addCircleMarkers(lng = ak_unsolved$long, 
                   lat = ak_unsolved$lat,
                   color = "red",
                   weight = 1,
                   radius = 5,
                   group = "Unsolved",
                   label = lapply(ak_unsolved$label, HTML)) %>% 
  addCircleMarkers(lng = ak_solved$long, 
                   lat = ak_solved$lat,
                   color = "blue",
                   weight = 1,
                   radius = 5,
                   group = "Solved",
                   label = lapply(ak_solved$label, HTML)) %>% 
  addLayersControl(overlayGroups = c("Unsolved", "Solved"),
                   options = layersControlOptions(collapsed = FALSE))
m

##############

us <- fbi_data %>% 
  mutate(Solved = ifelse(Crime.Solved == "Yes", 1, 0)) %>% 
  filter(Crime.Type == "Murder or Manslaughter") %>% 
  group_by(State) %>% 
  summarise(Num.Murders = n(),
            Num.Solved = sum(Solved)) %>% 
  mutate(Num.Unsolved = Num.Murders - Num.Solved,
         Solve.Rate = Num.Solved/Num.Murders)

states <- vect("R Leaflet/data/cb_2016_us_state_500k/cb_2016_us_state_500k.shp") #terra
states1 <- st_read("R Leaflet/data/cb_2016_us_state_500k/cb_2016_us_state_500k.shp") #sf

is.element(us$State, states$NAME)

levels(us$State)[40] <- "Rhode Island"

states <- subset(states, is.element(states$NAME, us$State))

us <- us[order(match(us$State, states$NAME))]

bins <- seq(0.3, 1.0, 0.1)
pal <- colorBin("RdYlBu", domain = us$Solve.Rate, bins = bins)


labels <- paste("<p>", "State: ", us$State, "</p>",
                "<p>", "Solve Rate: ", round(us$Solve.Rate, 3), "</p>", sep = "")

m <- leaflet() %>% 
  setView(-96, 37.8, 4) %>% 
  addProviderTiles(providers$Stadia.StamenToner) %>% 
  addPolygons(data = states,
              weight = 1,
              smoothFactor = 0.5,
              color = "white",
              fillOpacity = 0.8,
              fillColor = pal(us$Solve.Rate),
              highlight = highlightOptions(
                weight = 5,
                color = "#666666",
                #dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE
              ),
              label = lapply(labels, HTML)) %>% 
  addLegend(pal = pal,
            values = us$Solve.Rate,
            opacity = 0.7,
            position = "topright",
            title = "Solve Rate")
m

mapshot(x = m, file = "static.png")

saveWidget(m, "dynamic_map.html")

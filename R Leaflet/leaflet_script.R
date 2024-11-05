#Leaflet Map of Alaska Homocides

library(leaflet)
library(dplyr)
library(ggmap)
library(rgdal)
library(htmltools)
library(mapview)
library(htmlwidgets)

# First Map ------------------------------------------------------

#empty viewer window
m <- leaflet()
m

#world map
m <- leaflet() %>%
  addTiles()
m

#map centered on Alaska
m <- leaflet() %>%
  addTiles() %>%
  setView(lng = -149.4937, lat = 64.2008, zoom = 4)
m

#building a map without pipe operator by nesting functions together
m <- setView(addTiles(leaflet()),lng = -149.4937, lat = 64.2008, zoom = 4)
m

#building a map without pipe operator by creating intermediate objects
m_1 <- leaflet()
m_2 <- addTiles(m_1)
m <- setView(m_2, lng = -149.4937, lat = 64.2008, zoom = 4)
m
rm(m_1,m_2)



# Customizing Look and Feel --------------------------------------

#Load counties shape file.
counties <- readOGR("data/tl_2013_02_cousub/tl_2013_02_cousub.shp")
#Filter for Alaska
ak_counties <- subset(counties, counties$STATEFP == "02")


#add shapefile with the addPolygons function
m <- leaflet() %>%
  addTiles() %>%
  setView(lng = -149.4937, lat = 64.2008, zoom = 4) %>%
  addPolygons(data = ak_counties)
m


#further customize our polygons appearance
m <- leaflet() %>%
  addTiles() %>%
  setView(lng = -149.4937, lat = 64.2008, zoom = 4) %>%
  addPolygons(data = ak_counties,
              color = "#660000", 
              weight = 1, 
              smoothFactor = 0.5)
m


#use a different basemap
m <- leaflet(ak_counties) %>%
  addProviderTiles(providers$Thunderforest.Pioneer) %>%
  setView(lng = -149.4937, lat = 64.2008, zoom = 3)
m

m <- leaflet(ak_counties) %>%
  addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
  setView(lng = -149.4937, lat = 64.2008, zoom = 3)
m

m <- leaflet(ak_counties) %>%
  addProviderTiles(providers$Stamen.Toner) %>%
  setView(lng = -149.4937, lat = 64.2008, zoom = 3)
m

m <- leaflet() %>%
  addProviderTiles(providers$Stamen.Toner) %>%
  setView(lng = -149.4937, lat = 64.2008, zoom = 4) %>%
  addPolygons(data = ak_counties,
              color = "#660000", 
              weight = 1, 
              smoothFactor = 0.5)
m


# Data import and Geocode -------------------------------------------------

#import fbi data
fbi_data <- read.csv("data/database.csv")

#filter for Alaska
ak <- filter(fbi_data, State == "Alaska")

#missing lat and lon.  We can use geocode to get them, but first need a more
#complete address.
ak <- mutate(ak, address = paste(City, State, "United States"))
addresses <- unique(ak$address)
geocodes <- geocode(addresses, source = "google")

#combine geocodes with address and join to ak data
addresses_and_coords <- data.frame(address = addresses, 
                                   lon = geocodes$lon,
                                   lat = geocodes$lat)

#use a loop to find missing values
counter <- 0
while (sum(is.na(addresses_and_coords$lon)) > 0 && counter < 10) {
  
  #filter for missing address
  missing_addresses <- addresses_and_coords %>%
    filter(is.na(lon)==TRUE)
  
  #search again
  addresses <- missing_addresses$address
  geocodes <- geocode(as.character(addresses), source = "google")
  
  #remove missing addresess from data frame
  addresses_and_coords <- addresses_and_coords %>%
    filter(is.na(lon)==FALSE)
  
  #create a new data frame with the new addresses
  new_addresses <- data.frame(address = addresses, 
                              lon = geocodes$lon,
                              lat = geocodes$lat)
  
  #combine the new and missing data
  addresses_and_coords <- rbind(addresses_and_coords, new_addresses)
  counter <- counter + 1
}


#we no longer need geocodes, addresses, missing_addresses, new_addresses, counter
rm(geocodes)
rm(addresses)
rm(missing_addresses)
rm(new_addresses)
rm(counter)

#add lon and lat to ak data
ak <- left_join(ak, addresses_and_coords, by = "address")


# Add Crime Data to Map ---------------------------------------------------

#let's filter our data so that we're only looking at unsolved murders and
#manslaughters
ak_unsolved <- ak %>%
  filter(Crime.Solved == "No") %>%
  filter(Crime.Type == "Murder or Manslaughter")

m <- leaflet() %>%
  setView(lng = -149.4937, lat = 64.2008, zoom = 3) %>%
  addProviderTiles(providers$Stamen.Toner) %>%
  addPolygons(data = ak_counties,
              color = "#660000", 
              weight = 1, 
              smoothFactor = 0.5) %>%
  addCircleMarkers(lng = ak_unsolved$lon, lat = ak_unsolved$lat)
m


#problem of overlapping markers
#use jitter to add some slight random noise to our coordinates

ak$lon <- jitter(ak$lon, factor = 1)
ak$lat <- jitter(ak$lat, factor = 1)


m <- leaflet() %>%
  setView(lng = -149.4937, lat = 64.2008, zoom = 3) %>%
  addProviderTiles(providers$Stamen.Toner) %>%
  addPolygons(data = ak_counties,
              color = "#660000", 
              weight = 1, 
              smoothFactor = 0.5) %>%
  addCircleMarkers(lng = ak_unsolved$lon, 
                   lat = ak_unsolved$lat,
                   color = "ffffff",
                   weight = 1,
                   radius = 5,
                   label = ak_unsolved$Year)
m

#note that the label appears blank. this is because Year is an integer, needs to
#be text.

m <- leaflet() %>%
  setView(lng = -149.4937, lat = 64.2008, zoom = 3) %>%
  addProviderTiles(providers$Stamen.Toner) %>%
  addPolygons(data = ak_counties,
              color = "#660000", 
              weight = 1, 
              smoothFactor = 0.5) %>%
  addCircleMarkers(lng = ak_unsolved$lon, 
                   lat = ak_unsolved$lat,
                   color = "ffffff",
                   weight = 1,
                   radius = 5,
                   label = as.character(ak_unsolved$Year))
m



#create character vectors in HTML that contain what we want to show in the
#labels

ak_unsolved$label <- paste("<p>", ak_unsolved$City, "</p>",
                           "<p>", ak_unsolved$Month, " ", ak_unsolved$Year, "</p>",
                           "<p>", ak_unsolved$Victim.Sex, ", ", ak_unsolved$Victim.Age, "</p>",
                           "<p>", ak_unsolved$Victim.Race, "</p>",
                           "<p>", ak_unsolved$Weapon, "</p>",
                           sep="")


#map has HTML label but it reads it as plain text.  Need to treat text like HTML code.
m <- leaflet() %>%
  setView(lng = -149.4937, lat = 64.2008, zoom = 3) %>%
  addProviderTiles(providers$Stamen.Toner) %>%
  addPolygons(data = ak_counties,
              color = "#660000", 
              weight = 1, 
              smoothFactor = 0.5) %>%
  addCircleMarkers(lng = ak_unsolved$lon, 
                   lat = ak_unsolved$lat,
                   color = "gold",
                   weight = 1,
                   radius = 5, 
                   opacity = 0.75,
                   label = ak_unsolved$label)
m


#map has HTML label but it reads it as plain text.  Need to treat text like HTML
#code.  To do this, we use the HTML function from the htmltools package.
m <- leaflet() %>%
  setView(lng = -149.4937, lat = 64.2008, zoom = 3) %>%
  addProviderTiles(providers$Stamen.Toner) %>%
  addPolygons(data = ak_counties,
              color = "#660000", 
              weight = 1, 
              smoothFactor = 0.5) %>%
  addCircleMarkers(lng = ak_unsolved$lon, 
                   lat = ak_unsolved$lat,
                   color = "gold",
                   weight = 1,
                   radius = 5, 
                   opacity = 0.75,
                   label = lapply(ak_unsolved$label, HTML))
m

#add clustering
m <- leaflet() %>%
  setView(lng = -149.4937, lat = 64.2008, zoom = 3) %>%
  addProviderTiles(providers$Stamen.Toner) %>%
  addPolygons(data = ak_counties,
              color = "#660000", 
              weight = 1, 
              smoothFactor = 0.5) %>%
  addCircleMarkers(lng = ak_unsolved$lon, 
                   lat = ak_unsolved$lat,
                   color = "gold",
                   weight = 1,
                   radius = 5, 
                   opacity = 0.75,
                   label = lapply(ak_unsolved$label, HTML),
                   clusterOptions =  markerClusterOptions())
m


# Interactive Layers -----------------------------------------


#Show both solved and unsolved cases
ak_solved <- ak %>%
  filter(Crime.Solved == "Yes") %>%
  filter(Crime.Type == "Murder or Manslaughter")

#Create label for solved cases
ak_solved$label <- paste("<p>", ak_solved$City, "</p>",
                         "<p>", ak_solved$Month, " ", ak_solved$Year, "</p>",
                         "<p>", ak_solved$Victim.Sex, ", ", ak_solved$Victim.Age, "</p>",
                         "<p>", ak_solved$Victim.Race, "</p>",
                         "<p>", ak_solved$Weapon, "</p>",
                         sep="")


#add checkbox control.
m <- leaflet() %>%
  setView(lng = -149.4937, lat = 64.2008, zoom = 3) %>%
  addProviderTiles(providers$Stamen.Toner) %>%
  addPolygons(data = ak_counties,
              color = "#660000", 
              weight = 1, 
              smoothFactor = 0.5) %>%
  addCircleMarkers(lng = ak_unsolved$lon, 
                   lat = ak_unsolved$lat,
                   color = "gold",
                   weight = 1,
                   radius = 5, 
                   opacity = 0.75,
                   label = lapply(ak_unsolved$label, HTML),
                   group = "Unsolved") %>%
  addCircleMarkers(lng = ak_solved$lon, 
                   lat = ak_solved$lat,
                   color = "blue",
                   weight = 1,
                   radius = 5, 
                   opacity = 0.75,
                   label = lapply(ak_solved$label, HTML),
                   group = "Solved") %>%
  addLayersControl(overlayGroups = c("Unsolved", "Solved"),
                   options = layersControlOptions(collapsed = FALSE))
m



# Chloropleths ------------------------------------------------------------


#Let's create a chloropleth to show the number of solved and unsolved murders
#for each state

#Get shapefile for states
states <- readOGR("data/cb_2016_us_state_500k/cb_2016_us_state_500k.shp")

m <- leaflet() %>%
  setView(-96, 37.8, 4) %>%
  addProviderTiles(providers$Stamen.Toner) %>%
  addPolygons(data = states,
              weight = 1, 
              smoothFactor = 0.5)
m



#We need to calculate our summary stats
us <- fbi_data %>%
  mutate(Solved = ifelse(Crime.Solved == "Yes", 1, 0)) %>%
  filter(Crime.Type == "Murder or Manslaughter") %>%
  group_by(State) %>%
  summarise(Num.Murders = n(),
            Num.Solved = sum(Solved)) %>%
  mutate(Num.Unsolved = Num.Murders - Num.Solved,
         Solve.Rate = Num.Solved/Num.Murders)

#line up the states between our data and the shapefile
is.element(us$State, states$NAME)

#rename Rhodes Island
levels(us$State)[40] <- "Rhode Island"

#should now be all true
is.element(us$State, states$NAME)

#now check that all shapefile states are in us data
is.element(states$NAME, us$State)

#we see that we're missing the following: 33, 34, 54, 55, 56
states <- subset(states, is.element(states$NAME, us$State))

#now order the crime stats so that its the same order as the shapefile states
us <- us[order(match(us$State, states$NAME)),]

bins <- c(0.30, 0.40,0.50,0.60,0.70,0.80,0.90, 1.0)
pal <- colorBin("RdYlBu", domain = us$Solve.Rate, bins = bins)

#add our color pallete to our map
m <- leaflet() %>%
  setView(-96, 37.8, 4) %>%
  addProviderTiles(providers$Stamen.Toner) %>%
  addPolygons(data = states,
              fillColor = pal(us$Solve.Rate),
              weight = 1, 
              smoothFactor = 0.5,
              color = "white",
              fillOpacity = 0.8)
m


#add highlight options
m <- leaflet() %>%
  setView(-96, 37.8, 4) %>%
  addProviderTiles(providers$Stamen.Toner) %>%
  addPolygons(data = states,
              fillColor = pal(us$Solve.Rate),
              weight = 1, 
              smoothFactor = 0.5,
              color = "white",
              fillOpacity = 0.8,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE))
m


labels <- paste("<p>", us$State, "</p>",
                "<p>", "Solve Rate: ", round(us$Solve.Rate, digits = 3), "</p>",
                sep="")

m <- leaflet() %>%
  setView(-96, 37.8, 4) %>%
  addProviderTiles(providers$Stamen.Toner) %>%
  addPolygons(data = states,
              fillColor = pal(us$Solve.Rate),
              weight = 1, 
              smoothFactor = 0.5,
              color = "white",
              fillOpacity = 0.8,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE),
              label = lapply(labels, HTML))
m


#add a legend
m <- leaflet() %>%
  setView(-96, 37.8, 4) %>%
  addProviderTiles(providers$Stamen.Toner) %>%
  addPolygons(data = states,
              fillColor = pal(us$Solve.Rate),
              weight = 1, 
              smoothFactor = 0.5,
              color = "white",
              fillOpacity = 0.8,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE),
              label = lapply(labels, HTML)) %>%
  addLegend(pal = pal, 
            values = us$Solve.Rate, 
            opacity = 0.7, 
            title = NULL,
            position = "topright")
m


#save static version of map
#important to run webshot::install_phantomjs() if haven't already installed phantomjs()
mapshot(m, file ="static_map.png")

#save dynamic version of map
saveWidget(m, file = "dynamic_map.html")

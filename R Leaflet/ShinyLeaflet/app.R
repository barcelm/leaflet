#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shinydashboard)
library(rgdal)
library(leaflet)
library(dplyr)
library(DT)
#load data
crime_data <- readRDS("../data/database.rds")

crime_data <- crime_data %>%
  mutate(Solved = ifelse(Crime.Solved == "Yes", 1, 0)) %>%
  filter(Crime.Type == "Murder or Manslaughter")

states <- readOGR("../data/cb_2016_us_state_500k/cb_2016_us_state_500k.shp")
#line up the states between our data and the shapefile
is.element(crime_data$State, states$NAME)

#rename Rhodes Island
levels(crime_data$State)[40] <- "Rhode Island"

#should now be all true
is.element(crime_data$State, states$NAME)

#now check that all shapefile states are in us data
is.element(states$NAME, crime_data$State)

#we see that we're missing the following: 33, 34, 54, 55, 56
missing_states <- states$NAME[c(33, 34, 54, 55, 56)]

states <- subset(states, is.element(states$NAME, crime_data$State))

bins <- c(0.30, 0.40,0.50,0.60,0.70,0.80,0.90, 1.0)
pal <- colorBin("RdYlBu", domain = c(0,1), bins = bins)

ui <- dashboardPage(
  skin = "red",
  dashboardHeader(title = "Homicide Dashboard"),
  dashboardSidebar(
    sliderInput("date_range", "Date Range:", 
                min = min(crime_data$Year), 
                max = max(crime_data$Year), 
                value = c(min(crime_data$Year),max(crime_data$Year)), 
                sep = "",
                step = 1
                )
    ),
  dashboardBody(
    fluidRow( box(width = 12,
      leafletOutput("mymap")
    )),
    fluidRow( box(width = 12,
                  dataTableOutput("summary_table")))
  )
)

server <- function(input, output) {
  
  data_input <- reactive({
    crime_data %>%
      filter(Year >= input$date_range[1]) %>%
      filter(Year <= input$date_range[2]) %>%
      group_by(State) %>%
      summarise(Num.Murders = n(),
                Num.Solved = sum(Solved)) %>%
      mutate(Num.Unsolved = Num.Murders - Num.Solved,
             Solve.Rate = Num.Solved/Num.Murders)
  })
  
  data_input_ordered <- reactive({
    data_input()[order(match(data_input()$State, states$NAME)),]
  })
  
  labels <- reactive({
    paste("<p>", data_input_ordered()$State, "</p>",
          "<p>", "Solve Rate: ", round(data_input_ordered()$Solve.Rate, digits = 3), "</p>",
          sep="")
  })
  
  # leaflet_map <- reactive({
  #   leaflet() %>%
  #     setView(-96, 37.8, 4) %>%
  #     addProviderTiles(providers$Stamen.Toner) 
  # })
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      setView(-96, 37.8, 4) %>%
      addProviderTiles(providers$Stamen.Toner) %>%
      addPolygons(data = states,
                  fillColor = pal(data_input_ordered()$Solve.Rate),
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
                  label = lapply(labels(), HTML)) %>%
      addLegend(pal = pal,
                values = data_input_ordered()$Solve.Rate,
                opacity = 0.7,
                title = NULL,
                position = "topright")
    
  })
  
  output$summary_table = DT::renderDataTable(
    data_input(), options = list(lengthChange = FALSE)
  )
  
  
  }

shinyApp(ui, server)

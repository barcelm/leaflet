#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(terra)
library(DT)
library(htmlwidgets)

fbi_data <- readRDS("../R Leaflet/data/database.rds")

us <- fbi_data %>% 
  mutate(Solved = ifelse(Crime.Solved == "Yes", 1, 0)) %>% 
  filter(Crime.Type == "Murder or Manslaughter")

states <- vect("../R Leaflet/data/cb_2016_us_state_500k/cb_2016_us_state_500k.shp") #terra
#states1 <- st_read("R Leaflet/data/cb_2016_us_state_500k/cb_2016_us_state_500k.shp") #sf

is.element(us$State, states$NAME)

levels(us$State)[40] <- "Rhode Island"

states <- subset(states, is.element(states$NAME, us$State))

#us <- us[order(match(us$State, states$NAME))]

bins <- seq(0.3, 1.0, 0.1)
pal <- colorBin("RdYlBu", domain = c(0,1), bins = bins)






# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "Homicide Dashboard"),
  skin = "red",
  dashboardSidebar(
    sliderInput("date_range", "Date Range", 
                min = min(us$Year),
                max = max(us$Year),
                value = c(min(us$Year), max(us$Year)),
                sep = "",
                step = 1)
  ),
  dashboardBody(
    fluidRow(box(width = 12, leafletOutput(outputId = "my_map"))),
    fluidRow(box(width = 12, dataTableOutput(outputId = "summary_table")))
  )
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  data_input <- reactive({
    us %>% 
      filter(Year >= input$date_range[1], Year <= input$date_range[2]) %>% 
      group_by(State) %>% 
      summarise(Num.Murders = n(),
                Num.Solved = sum(Solved)) %>% 
      mutate(Num.Unsolved = Num.Murders - Num.Solved,
             Solve.Rate = Num.Solved/Num.Murders)
  })
  
  label <- reactive({
    paste("<p>", "State: ", data_input()$State, "</p>",
          "<p>", "Solve Rate: ", round(data_input()$Solve.Rate, 3), "</p>", sep = "")
  })
  
  output$my_map <- renderLeaflet(
    leaflet() %>% 
      setView(-96, 37.8, 4) %>% 
      addProviderTiles(providers$OpenStreetMap) %>% 
      addPolygons(data = states,
                  weight = 1,
                  smoothFactor = 0.5,
                  color = "white",
                  fillOpacity = 0.8,
                  fillColor = pal(data_input()$Solve.Rate),
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666666",
                    #dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE
                  ),
                  label = lapply(label(), HTML)) %>% 
      addLegend(pal = pal,
                values = data_input()$Solve.Rate,
                opacity = 0.7,
                position = "topright",
                title = "Solve Rate")
  )
  
  output$summary_table <- renderDT(data_input())
}

# Run the application 
shinyApp(ui = ui, server = server)

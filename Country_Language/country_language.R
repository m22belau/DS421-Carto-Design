library(shiny)
library(sf)
library(viridis)
library(tidyverse)
library(leaflet)
library(here)

# Read county data
county_data <- st_read(here("data/county_data.gpkg"))

# Ensure geometry is in lon/lat
county_data <- st_transform(county_data, 4326)

# Unique languages for dropdown
languages <- sort(unique(county_data$language))

ui <- fluidPage(
  titlePanel("Language Speakers by County"),
  sidebarLayout(
    sidebarPanel(
      selectInput("language", "Select a language:", choices = languages, selected = languages[1])
    ),
    mainPanel(
      leafletOutput("map", height = "700px")
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive filtered data
  filtered_data <- reactive({
    req(input$language)
    data <- county_data %>% filter(language == input$language)
    req(nrow(data) > 0)
    data
  })
  
  output$map <- renderLeaflet({
    data <- filtered_data()
    
    # Create color bins
    pal <- colorBin(
      palette = viridis(7, option = "D"),
      domain = county_data$percent_speakers,
      bins = 7,
      na.color = "transparent"
    )
    
    # Build leaflet map
    leaflet(data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal(percent_speakers),
        weight = 1,
        color = "#333333",
        fillOpacity = 0.7,
        popup = ~paste0(
          "<b>", geoname, "</b><br>",
          "Language: ", language, "<br>",
          "Percent: ", round(percent_speakers, 2), "%"
        )
      ) %>%
      addLegend(
        "bottomright",
        pal = pal,
        values = ~percent_speakers,
        title = "Percent Speakers",
        opacity = 0.7
      )
  })
  
  # Update map when language changes
  observe({
    data <- filtered_data()
    
    pal <- colorBin(
      palette = viridis(7, option = "D"),
      domain = county_data$percent_speakers,
      bins = 7,
      na.color = "transparent"
    )
    
    leafletProxy("map", data = data) %>%
      clearShapes() %>%
      addPolygons(
        fillColor = ~pal(percent_speakers),
        weight = 1,
        color = "#333333",
        fillOpacity = 0.7,
        popup = ~paste0(
          "<b>", geoname, "</b><br>",
          "Language: ", language, "<br>",
          "Percent: ", round(percent_speakers, 2), "%"
        )
      )
  })
}

shinyApp(ui, server)

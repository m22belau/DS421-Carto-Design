library(shiny)
library(mapgl)
library(mapboxapi)
library(sf)

# Maui House 
property_coords <- c(-156.51489464777018, 20.877774980734063)

# Create sf object for the property
property_sf <- st_as_sf(data.frame(
  id = "house",
  name = "Beryls House",
  lon = property_coords[1],
  lat = property_coords[2]
), coords = c("lon", "lat"), crs = 4326)

# Generate isochrone polygon using Mapbox API
isochrone <- mb_isochrone(property_coords, profile = "driving", time = 30)

# UI
ui <- fluidPage(
  tags$link(href = "https://fonts.googleapis.com/css2?family=Poppins:wght@300;400;600&display=swap", rel = "stylesheet"),
  story_map(
    map_id = "map",
    font_family = "Poppins",
    sections = list(
      "intro" = story_section(
        title = "Hello! Lets go to Maui",
        content = list(
          p("Aloha! Let's explore Beryl's Home"),
          img(src = "house.png", width = "300px")
        ),
        position = "center"
      ),
      "marker" = story_section(
        title = "Wailuku, Maui",
        content = list(
          p("Welcome to Maui!My home is located in Wailuku, on the island of Maui. Nestled just west of Kahului and at the mouth of the lush ʻĪao Valley, Wailuku offers a blend of natural beauty and rich history. My house sits on the mountain slope, where I often enjoy cool, breezy winds and sweeping panoramic views of downtown Wailuku, Lahaina, Upcountry Maui, and the sparkling Pacific Ocean.")
        )
      ),
      "isochrone" = story_section(
        title = "From Mauka to Makai",
        content = list(
          p("In just 30 scenic minutes from Wailuku, I can be winding through the cool mist of Makawao, relaxing on the shores of Kihei, or nearing the historic charm of Lahaina. Whether I head mauka or makai, every route offers sweeping views and a different piece of Maui’s story.")
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  output$map <- renderMapboxgl({
    mapboxgl(
      scrollZoom = FALSE,
      center = c(-156.51489464777018, 20.877774980734063),
      zoom = 12
    )
  })
  
  on_section("map", "intro", {
    mapboxgl_proxy("map") |>
      clear_layer("property_layer") |>
      clear_layer("isochrone") |>
      fly_to(
        center = c(-156.51489464777018, 20.877774980734063),
        zoom = 12,
        pitch = 0,
        bearing = 0
      )
  })
  
  on_section("map", "marker", {
    proxy <- mapboxgl_proxy("map")
    
    proxy |>
      clear_layer("isochrone") |>
      add_source(id = "property", data = property_sf) |>
      add_circle_layer(
        id = "property_layer",
        source = "property",
        circle_color = "#CC5500",
        circle_radius = 10,
        circle_opacity = 0.8,
        popup = "name"
      ) |>
      fly_to(
        center = property_coords,
        zoom = 16,
        pitch = 45,
        bearing = -90
      )
    
  })
  
  on_section("map", "isochrone", {
    mapboxgl_proxy("map") |>
      add_fill_layer(
        id = "isochrone",
        source = isochrone,
        fill_color = "#CC5500",
        fill_opacity = 0.5
      ) |>
      fit_bounds(
        isochrone,
        animate = TRUE,
        duration = 8000,
        pitch = 75
      )
  })
}

# Run the app
shinyApp(ui, server)
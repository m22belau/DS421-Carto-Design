library(tidyverse)
library(sf)
library(here)
library(shiny)
library(mapgl)

# --- Load GeoJSON ---
data <- st_read(here("data/ahupuaa.geojson"))

# --- Filter for Maui ---
maui_data <- data %>% filter(mokupuni == "Maui")
maui_data <- st_make_valid(maui_data)

# --- General Moku History ---
moku_history <- "<b>Moku (Districts) - General History:</b><br><br>
<b>Districts:</b> Were the traditional Hawaiian districts, which were large, geographically defined sections of an island.<br><br>
<b>Political and Administrative Units:</b> The primary function of Moku was as political and administrative units.<br><br>
<b>Aliʻi Governance:</b> The aliʻi governed a moku, holding authority in overseeing resource distribution, maintaining order through the kapu system, and advising the ruling chief of the island.<br><br>
<b>Nested System:</b> The moku were part of a larger, nested system of land division and governance, including the entire island (mokupuni). They were further subdivided into smaller units like ahupuaʻa and ʻili."

# --- General Ahupuaʻa History ---
ahupuaa_history <- "<b>Ahupuaʻa - General History:</b><br><br>
<b>Definition:</b> An ahupuaʻa was a land division that typically extended from the mountains (mauka) to the sea (makai). Each was designed to support a self-sufficient community.<br><br>
<b>Integrated Ecosystems:</b><br>
- <b>Mauka (mountain) region:</b> Source of fresh water, forest resources, and materials like wood for building.<br>
- <b>Wao (midland) region:</b> Fertile lands used for agriculture, especially taro cultivation in loʻi.<br>
- <b>Makai (ocean) region:</b> Coastal and marine areas used for fishing, aquaculture, and salt production.<br><br>
<b>Sustainability:</b> Ahupuaʻa were managed to ensure long-term resource availability through practices like crop rotation and controlled fishing.<br><br>
<b>Governance:</b> Each ahupuaʻa was overseen by an aliʻi or konohiki, responsible for resource distribution and enforcing kapu (sacred laws).<br><br>
<b>Cultural Significance:</b> Ahupuaʻa reflected the Hawaiian reverence for the land (ʻāina) and the interconnectedness of all living things."

# --- Shiny UI ---
ui <- fluidPage(
  titlePanel("Maui Moku & Ahupuaʻa Map"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Moku History"),
      htmlOutput("moku_text"),
      
      selectInput("moku_select", "Select a Moku:",
                  choices = c("None", unique(maui_data$moku)),
                  selected = "None"),
      
      h4("Ahupuaʻa History"),
      htmlOutput("ahupuaa_text"),
      
      selectInput("ahupuaa_select", "Select an Ahupuaʻa:",
                  choices = c("None"),
                  selected = "None")
    ),
    
    mainPanel(
      maplibreOutput("map", height = "600px")
    )
  )
)

# --- Shiny UI ---
ui <- fluidPage(
  titlePanel("Maui Moku & Ahupuaʻa Map"),
  
  sidebarLayout(
    sidebarPanel(
      style = "height: 600px; overflow-y: auto;",  # Makes sidebar scrollable
      
      h4("Moku History"),
      htmlOutput("moku_text"),
      
      selectInput("moku_select", "Select a Moku:",
                  choices = c("None", unique(maui_data$moku)),
                  selected = "None"),
      
      br(),
      h4("Ahupuaʻa History"),
      htmlOutput("ahupuaa_text"),
      
      selectInput("ahupuaa_select", "Select an Ahupuaʻa:",
                  choices = c("None"),
                  selected = "None")
    ),
    
    mainPanel(
      maplibreOutput("map", height = "600px")
    )
  )
)

# --- Shiny Server ---
server <- function(input, output, session) {
  
  # --- Initial map (Step 0) ---
  output$map <- renderMaplibre({
    maplibre(carto_style("positron"), scrollZoom = FALSE) %>%
      fit_bounds(maui_data, animate = FALSE) %>%
      
      # Base layer for all moku/ahupuaʻa
      add_fill_layer(
        id = "moku_all",
        source = maui_data,
        fill_color = "#cfd8dc",
        fill_opacity = 0.3,
        tooltip = "ahupuaa"
      ) %>%
      add_line_layer(
        id = "moku_all_outline",
        source = maui_data,
        line_color = "#37474f",
        line_width = 1
      ) %>%
      add_fill_layer(
        id = "moku_sel",
        source = maui_data,
        fill_color = "#2ecc71",
        fill_opacity = 0.6,
        filter = list("==", "moku", "__none__"),
        tooltip = "ahupuaa"
      ) %>%
      add_line_layer(
        id = "moku_sel_outline",
        source = maui_data,
        line_color = "#1b5e20",
        line_width = 3,
        filter = list("==", "moku", "__none__")
      ) %>%
      add_fill_layer(
        id = "ahu_sel_click",
        source = maui_data,
        fill_color = "#f39c12",
        fill_opacity = 0.6,
        filter = list("==", "ahupuaa", "__none__"),
        tooltip = "ahupuaa"
      )
  })
  
  # --- Step 1: Update Moku selection ---
  observeEvent(input$moku_select, {
    selected_moku <- input$moku_select
    
    if(selected_moku == "None") {
      maplibre_proxy("map") %>% fit_bounds(maui_data, animate = TRUE)
      
      # Reset ahupuaʻa dropdown and text
      updateSelectInput(session, "ahupuaa_select", choices = c("None"), selected = "None")
      output$moku_text <- renderUI({ "" })
      output$ahupuaa_text <- renderUI({ "" })
    } else {
      selected_geom <- maui_data %>% filter(moku == selected_moku)
      
      # Highlight moku and zoom
      maplibre_proxy("map") %>%
        set_filter("moku_sel", list("==", "moku", selected_moku)) %>%
        set_filter("moku_sel_outline", list("==", "moku", selected_moku)) %>%
        fit_bounds(selected_geom, animate = TRUE)
      
      # Show general moku history
      output$moku_text <- renderUI({ HTML(moku_history) })
      
      # Update ahupuaʻa dropdown for that moku
      ahupuaas <- unique(selected_geom$ahupuaa)
      updateSelectInput(session, "ahupuaa_select",
                        choices = c("None", ahupuaas),
                        selected = "None")
      
      # Reset ahupuaʻa text
      output$ahupuaa_text <- renderUI({ "" })
    }
  })
  
  # --- Step 2: Update Ahupuaʻa selection ---
  observeEvent(input$ahupuaa_select, {
    selected_ahu <- input$ahupuaa_select
    
    if(selected_ahu == "None") {
      maplibre_proxy("map") %>%
        set_filter("ahu_sel_click", list("==", "ahupuaa", "__none__"))
      output$ahupuaa_text <- renderUI({ "" })
    } else {
      selected_geom <- maui_data %>% filter(ahupuaa == selected_ahu)
      
      maplibre_proxy("map") %>%
        set_filter("ahu_sel_click", list("==", "ahupuaa", selected_ahu)) %>%
        fit_bounds(selected_geom, animate = TRUE)
      
      # Show general Ahupuaʻa history
      output$ahupuaa_text <- renderUI({ HTML(ahupuaa_history) })
    }
  })
  
  # --- Step 3: Click on map to show specific Ahupuaʻa info ---
  observeEvent(input$map_click, {
    click <- input$map_click
    if(!is.null(click)) {
      # Find the ahupuaʻa nearest to the click
      nearest <- st_nearest_feature(st_sfc(st_point(c(click$lng, click$lat)), crs = st_crs(maui_data)), maui_data)
      selected_geom <- maui_data[nearest,]
      selected_name <- selected_geom$ahupuaa
      
      # Highlight clicked ahupuaʻa
      maplibre_proxy("map") %>%
        set_filter("ahu_sel_click", list("==", "ahupuaa", selected_name))
      
      # Show specific info
      output$ahupuaa_text <- renderUI({
        HTML(paste0("<b>Specific Ahupuaʻa:</b> ", selected_name))
      })
    }
  })
}

# --- Run App ---
shinyApp(ui, server)
library(shiny)
library(httr)
library(jsonlite)
library(dplyr)
library(leaflet)
library(ggplot2)  # For map_data

setwd('..')
  
key_file <- "config/api_keys.txt"
subscription_key <- readLines(key_file, warn = FALSE) # Replace with your actual API subscription key

url <- "https://api.at.govt.nz/realtime/legacy/vehiclelocations"

ui <- fluidPage(
  tags$style(HTML("
    body {
      background-color: #262626; /* Dark shade */
      color: white; /* Text color for better contrast */
    }
    h1 {
      text-align: center; /* Center the title */
    }
    #map-container {
      display: flex;            /* Flexbox for centering */
      justify-content: center;  /* Center horizontally */
      align-items: center;      /* Center vertically */
      height: 90vh;             /* Container height: 90% of viewport */
    }
    #map {
      width: 80%;               /* Map width */
      height: 80%;              /* Map height */
    }
  ")),
  h1("Auckland Transport Bus Locations"),
  div(id = "map-container", 
      leafletOutput("map", width = "100%", height = "80%")
  ),
  h1("Test"),
  p("Current busses on the road:")
)

server <- function(input, output, session) {
  # Reactive Timer: Trigger every 15 seconds (15000 ms)
  autoInvalidate <- reactiveTimer(15000)
  
  # Reactive function to fetch and process data
  get_map_data <- reactive({
    autoInvalidate()  # Re actively triggers the block every 30 seconds
    
    # API call
    response <- GET(url, add_headers(`Ocp-Apim-Subscription-Key` = subscription_key))
    
    if (http_status(response)$category == "Success") {
      data <- content(response, "text")
      realtime_data <- fromJSON(data)
      
      # Extract relevant information
      aresponse <- realtime_data$response
      aentity <- aresponse$entity
      
      # Extract data into a dataframe
      newdf <- aentity$vehicle$position %>%
        select(longitude, latitude) %>%
        mutate(
          id = aentity$id,
          route_id = aentity$vehicle$trip$route_id,
          occupancy = aentity$vehicle$occupancy_status
        ) %>%
        subset(longitude >= 80) %>%
        na.omit()
      
      return(newdf)
    } else {
      stop("Failed to fetch data: ", http_status(response)$message)
    }
  })
  
  # Render the initial leaflet map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.DarkMatter") %>%
      setView(lng = 174.78, lat = -36.89, zoom = 10)  # Adjust center/zoom as needed
  })
  
  # Observe the data and update the map
  observe({
    # Fetch updated data
    cleaned_df <- get_map_data()
    
    # Define bins and color palette
    mybins <- seq(0, 6, by = 1)
    mypalette <- colorBin(palette = "YlOrBr", domain = cleaned_df$occupancy, na.color = "transparent", bins = mybins)
    
    # Update the map with new markers and legend
    leafletProxy("map", data = cleaned_df) %>%
      clearMarkers() %>% 
      clearControls() %>%  # Clear previous legends
      addProviderTiles("CartoDB.DarkMatter") %>%
      addCircleMarkers(
        ~longitude,
        ~latitude,
        label = ~paste("Route ID:", route_id, "Occupancy:", occupancy),
        fillColor = ~mypalette(occupancy),
        fillOpacity = 0.7,
        color = "white",
        radius = 8,
        stroke = FALSE,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        pal = mypalette,
        values = ~occupancy,
        opacity = 0.9,
        title = "Occupancy",
        position = "bottomright"
      )
  })
}

shinyApp(ui = ui, server = server)
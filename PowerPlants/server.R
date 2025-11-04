# Libraries
library(shiny)
library(bslib)

# Sourcing ui to know what plot is being called
source("ui.R")
source("set_code.R")


server = function(input, output, session){
  
  map_df = reactive({
    
    mn_powerplants %>%
      filter(first_op_year == as.numeric(input$numeric[1]))%>%
      filter(!is.na(Longitude) & !is.na(Latitude)) %>%
      group_by(plant_code, Longitude,Latitude) %>%
      summarize (count = n()) %>%
      st_as_sf(coords = c('Longitude', 'Latitude')) %>%
      st_set_crs(4326)
    
  })
  
  output$map = renderLeaflet({
    
    leaflet() %>%
      addTiles() %>%
      setView(lng = -94.6, lat = 46.4, zoom = 6) %>%
      addCircleMarkers(data = map_df())
    
  })
  
  
}

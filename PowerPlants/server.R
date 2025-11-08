# Libraries
library(shiny)
library(bslib)
library(leaflegend)


# Sourcing ui to know what plot is being called
source("ui.R")
source("set_code.R")


server = function(input, output, session){
  
###================================ Opening ===============================###
  
  
  map_df = reactive({
    
    mn_powerplants %>%
      filter(first_op_year == as.numeric(input$numeric[1]))%>%
      filter(!is.na(longitude) & !is.na(latitude)) %>%
      group_by(plant_code, longitude,latitude) %>%
      summarize (count = n()) %>%
      st_as_sf(coords = c('longitude', 'latitude')) %>%
      st_set_crs(4326)
    
  })
  
  
  map_lp = reactive({
    
    mn_powerplants %>%
      filter(first_op_year < as.numeric(input$numeric[1]))%>%
      filter(!is.na(longitude) & !is.na(latitude)) %>%
      group_by(plant_code, longitude,latitude) %>%
      summarize (count = n()) %>%
      st_as_sf(coords = c('longitude', 'latitude')) %>%
      st_set_crs(4326)
    
  })
  
  output$map = renderLeaflet({
    
    leaflet() %>%
      addTiles() %>%
      setView(lng = -94.6, lat = 46.4, zoom = 6) %>%
      addCircleMarkers(data = map_df(), color = "red", radius = 3) %>%
      addCircleMarkers(data = map_lp(), color = "grey", radius = 1) %>%
    addLegendFactor(
      pal = colorFactor(my_colors, domain = values),
      values = values,
      orientation = "horizontal",
      opacity = 0.75,
      position = "topright",
      width = 12,
      height = 12
    )

  })
  
  ###================================ Power Plants ===============================###
  
  output$pp_dates_barplot = renderPlot({

    mn_powerplants %>%
      ggplot(aes(x = first_op_year, fill = fossil_fuel)) +
      geom_bar() +
      facet_wrap(~fossil_fuel) +
      labs(title = "When Have Different Kinds of Power Plants Been Built in Minnesota?",
           x="First Year of Operation",
           y = "Number of Powerplants",
           fill = "Powerplant Type") +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 90, vjust = 1))
    # TO DO: change colors

  })
  
  output$pp_type_barplot = renderPlot({
    
    mn_powerplants %>% 
      ggplot(aes(x = fct_infreq(prim_source))) +
      geom_bar() +
      labs(x = "Energy Source", y = "Number of Powerplants", title = "Minnesota Powerplants' Energy Sources",
           fill = "Powerplant Type") +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    
  })
  
  output$pp_type_by_mw_barplot = renderPlot({
    
    mn_powerplants %>% 
      group_by(prim_source) %>% 
      summarise(prod_by_type = sum(total_mw)) %>% 
      mutate(fossil_fuel = ifelse(prim_source %in% c("coal", "petroleum", "natural gas"), "Fossil Fuel", "Renewable")) %>% 
      ggplot(aes(x = fct_reorder(prim_source, prod_by_type, .desc = TRUE), y = prod_by_type, fill = fossil_fuel)) +
      geom_bar(stat = 'identity') +
      labs(x = "Energy Source", y = "Megawatts Electricity Produced", title = "Energy produced by Source in MN Powerplants",
           fill = "Powerplant Type") +  
      theme_classic() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    
  })
  

}
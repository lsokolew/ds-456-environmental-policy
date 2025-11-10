# Libraries
library(shiny)
library(bslib)
library(leaflegend)
library(sf)
library(tigris)

options(tigris_use_cache = TRUE)


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
      theme_1    

  }, bg = "transparent")
  
  output$pp_type_barplot = renderPlot({
    
    mn_powerplants %>% 
      ggplot(aes(x = fct_infreq(prim_source))) +
      geom_bar() +
      labs(x = "Energy Source", y = "Number of Powerplants", title = "Minnesota Powerplants' Energy Sources",
           fill = "Powerplant Type") +
      theme_classic() +
      theme_1    +
      fuel_colors
  }, bg = "transparent")
  
  output$pp_type_by_mw_barplot = renderPlot({
    
    mn_powerplants %>% 
      group_by(prim_source) %>% 
      summarise(prod_by_type = sum(total_mw)) %>% 
      mutate(fossil_fuel = ifelse(prim_source %in% c("coal", "petroleum", "natural gas"), "Fossil Fuel", "Renewable")) %>% 
      ggplot(aes(x = fct_reorder(prim_source, prod_by_type, .desc = TRUE), y = prod_by_type, fill = fossil_fuel)) +
      geom_bar(stat = 'identity') +
      fuel_colors +
      labs(x = "Energy Source", y = "Megawatts Electricity Produced", title = "Energy produced by Source in MN Powerplants",
           fill = "Powerplant Type") +  
      theme_classic() +
      theme_1 
    }, bg = "transparent")
  
  
  ###================================ Health ===============================###
  
  output$asthma_map <- renderLeaflet({
    
    # ---- Color palette for polygons ----
    pal <- colorFactor(
      palette = c("lightblue", "steelblue", "royalblue4", "navy"),
      levels = c("0-2", "2-4", "4-7", "7+"),
      na.color = "grey"
    )
    
    # ---- Palette for power plants ----
    pal3 <- colorFactor(
      palette = c("red", "green"),
      domain = mn_powerplants$fossil_fuel
    )
    
    leaflet(zcta_joined) %>%
      setView(lng = -93.265, lat = 44.9778, zoom = 8) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(value_cat),
        color = "black",
        weight = 1,
        fillOpacity = 0.7,
        opacity = 1,
        highlight = highlightOptions(
          weight = 2,
          color = "white"
        ),
        label = ~paste0(
          "Zipcode: ", ZCTA5CE20, "<br>",
          "Rate: ",
          ifelse(is.na(`Age-adjusted rate per 10,000`),
                 "Not given due to small population",
                 `Age-adjusted rate per 10,000`)
        ),
        labelOptions = labelOptions(
          style = list("white-space" = "pre-line")
        )
      ) %>%
      addLegend(
        pal = pal,
        values = zcta_joined$value_cat,
        opacity = 0.7,
        title = "Asthma hospitalizations per 10,000 (2017–2021)",
        position = "bottomright",
        na.label = "Not given"
      ) %>%
      addLegend(
        data = mn_powerplants,
        pal = pal3,
        values = ~fossil_fuel,
        opacity = 0.7,
        title = "Renewable or Fossil Fuel",
        position = "bottomright",
        na.label = "Not given") %>%
      addCircleMarkers(
        data = mn_powerplants,
        lng = ~longitude,
        lat = ~latitude,
        color = ~pal3(fossil_fuel),
        radius = 3,        # 0.25 is too small to see
        fillOpacity = 1,
        label = ~paste0(
          "Plant Name: ", plant_name, "<br>",
          "Plant Code: ", plant_code
        )
      )
  })
}
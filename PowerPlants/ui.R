
# Libraries
library(shiny)
library(bslib)
source("set_code.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    h1(style = "text-align:center; font-size:30px;", strong("Positive adaptations of power plants in Minnesota")),
    
    # theme
    theme = bs_theme(
      # background color
      bg = light_blue,
      # font color
      fg = "#0C181F",
      base_font = font_google("Tinos"),
      code_font = font_google("JetBrains Mono")
    ),
###=========================layout of shiny=========================###

    # side
    sidebarLayout(
      sidebarPanel = sidebarPanel(sliderInput('numeric', 'Year', min = 1907, max = 2025, value = c(2022))),
      
    # main
      mainPanel = mainPanel(
        leafletOutput(outputId = 'map')
      ) # closing main panel
    ) # closing sidebar Layout
  ) # closing UI

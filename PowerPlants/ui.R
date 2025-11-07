
# Libraries
library(shiny)
library(bslib)
source("set_code.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
  ###=========================theme=========================###
  
  # theme
  theme = bs_theme(
    # background color
    bg = light_blue,
    # font color
    fg = "#0C181F",
    base_font = font_google("Tinos"),
    code_font = font_google("JetBrains Mono")
  ),
  
  ###=========================Introduction=========================###
  

    # Application title
    br(),
    br(), 
    br(),
    br(),
    h1(style = "text-align:center; font-size:80px;", strong("Power Plants in Minnesota")),
    h2(style = "text-align:center; font-size:22px;", "Where are they? Whom do they impact? How do they impact people?"),
    h2(style = "text-align:center; font-size:13px;", "By: Alicia Severiano Perez, Sydney Ohr, and Lilabeth Sokolewicz"),
    

    # main
      column(12, align = "center", leafletOutput(outputId = 'map', height = 400, width = 600)),
      column(12, align = "center", sliderInput('numeric', 'Year', min = 1906, max = 2025, value = c(1907), sep = "")),


    ##=================Context=================##

  fluidRow(column(width = 12, h3(style = "text-align:center; font-size:18px;", context)),)

  ) # closing UI
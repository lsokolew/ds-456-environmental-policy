
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

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)


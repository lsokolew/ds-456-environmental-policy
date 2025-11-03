
library(shiny)
library(bslib)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("<strong>Positive adaptation of power plants in Minnesota<strong>"),
    
    # theme
    theme = bs_theme(
      # background color
      bg = "#9ECADE",
      # font color
      fg = "#0C181F",
      primary = "#E69F00",
      secondary = "#0072B2",
      success = "#009E73",
      base_font = font_google("Roboto"),
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


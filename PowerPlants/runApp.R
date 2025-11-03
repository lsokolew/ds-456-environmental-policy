# Note: Nothing has to be changed here
library(shiny)

# Call the other files
source("ui.R")
source("server.R")

# Run the application 
shinyApp(ui = ui, server = server)
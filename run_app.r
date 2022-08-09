library(tidyverse)
library(lubridate)
library(sf)
library(shiny)
library(shinyWidgets)
library(leaflet)
library(htmlwidgets)
library(leaflegend)


#library(reactlog)
# Get the underlying data
source("gather_data.r")

# Source the app
source("functions.r")
source("ui.r")
source("server.r")

# Run the application 

shinyApp(ui = ui, server = server)



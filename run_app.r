library(tidyverse)
library(lubridate)
library(sf)
library(shiny)
library(leaflet)
#library(shinyWidgets)
#library(htmlwidgets)
#library(leaflegend)
<<<<<<< HEAD
=======


>>>>>>> 1bf7ff8189496b7d593f6826e193ef5db10a2189
#library(reactlog)

# Get the underlying data
source("gather_data.r")

# Source the app
source("functions.r")
source("ui.r")
source("server.r")

# Run the application 

shinyApp(ui = ui, server = server)



library(shiny)

map_tab_ui <- tabPanel(
  "Map",
  titlePanel(h1("Fixed Receiver Locations in the Peace River Basin", align = 'center')),
  div("The fixed array has been operated from April through October each year since 2019.", style = 'text-align: center;'),
  br(),
  # STUDY AREA MAP FROM REPORT WITH FIXED RECEIVER LOCATIONS
  div(img(src = '2021Site C Report.png', height = "55%", width = "55%"), style = "text-align: center;")
)

# If you want to include this in your server logic, you might also define a server function here

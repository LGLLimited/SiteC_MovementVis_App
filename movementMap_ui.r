library(shiny)
library(leaflet)  # Ensure to include required packages


movementMap_ui <- 
 tabPanel("Plots",
 lapply(list.files("www/css", pattern = "\\.css$", full.names = TRUE), includeCSS),
 #lapply(list.files("www/js", pattern = "\\.js$", full.names = TRUE), includeScript),
  sidebarLayout(
     sidebarPanel(width=3,
       fluidRow(
    #       tags$head(tags$style(HTML("
    #        hr {border-top: 0.5px solid #000000;}
    #        .shiny-input-container {
    #          font-size: 14px;
    #          width: 100%; /* Ensure it uses the full width */
    #        }
    #      .selectize-input {
    #      height: 20px; /* Set a fixed height */
    #      line-height: 20px;
    #      padding: 0 4px; /* Add some horizontal padding */
    #    }
    #        .selectize-input input {
    #     height: 20px; /* Ensure the input area matches */
    #     padding: 0; /* Remove padding from input */
    #     margin: 0; /* Remove margin */
    #     line-height: 20px; /* Align input text */
    # }
    
    #       "))),

#          tags$head(tags$style(HTML("
#     .shiny-input-select {
#         font-size: 14px; /* Ensure consistent font size */
#         width: 100%; /* Ensure it uses the full width */
#     }
#     .selectize-input {
#         height: 10 !important; /* Allow height to adjust automatically */
#         padding: 4px; /* Adjust padding to fit content */
#         line-height: 1.5; /* Ensure line height for readability */
#         border: 0px solid #ccc; /* Add a border for visibility */
#         box-shadow: none; /* Remove any box shadow */
#     }
#     .selectize-input [class*='item'] {
#         line-height: 1.5; /* Ensure dropdown items have appropriate line height */
#     }

# "))),
         # play button size
         tags$head(tags$style(type='text/css', ".slider-animate-button { font-size: 12pt !important;}")),
         # shift play button to left and pad away from slider
         tags$head(tags$style(type='text/css', ".slider-animate-container { text-align: left; margin-top: -2px; padding:0}")),
         # remove minor ticks on animation slider
         tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),
         h1("Plot settings"),
         hr(),
         radioButtons("basemap", "Select basemap.", choices = c("Terrain", "Satellite"), selected = "Terrain"),
         actionButton("center_map", "Re-center Map"),
         checkboxInput("receivers",label=HTML("<b>Show fixed-station receivers.</b>"),value=FALSE),
         #br(),
         #br(),
         uiOutput("choiceUI"),
         uiOutput("lifestageUI"),
         #br(),
         sliderTextInput("anim_speed","Select animation speed.",choices = c("Slowest","Slower","Faster","Fastest"), 
                         selected ="Faster",grid = TRUE),
         #br(),
         uiOutput("dateUI"),
       ),
       fluidRow(HTML("<b>Plot description.</b>"), tags$div(style = "font-size: 12px;",htmlOutput("plot_desc")),
       br(),tags$div(style = "font-size: 12px;",textOutput("data_desc")))
     ),
     mainPanel(width=9,
       tags$head(tags$style(HTML('.irs-from, .irs-to, .irs-min, .irs-max{visibility: hidden !important;}'))),
       tabsetPanel(id="tabs",
         tabPanel("Individual Movements", mainPanel(leafletOutput("map1"))),#,width="150vh",height="87vh"))),
         tabPanel("Seasonal Distribution", mainPanel(leafletOutput("map2")))#,width="150vh",height="87vh")))
       )
     )
   )
 )

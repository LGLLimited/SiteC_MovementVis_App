# UI ####
ui <- fluidPage(titlePanel(title=img(src="BC-Hydro-Logo.png",height="56px",width="100px",style="float: right;"),
                           windowTitle="Site C Fish Movement"),
                tabsetPanel(id="main",
                            tabPanel("Info",
                                     fluidPage(
                                       titlePanel(h1("Site C Fish Movement Assessment",align='center')),
                                       br(),br(),br(),
                                     fluidRow(column(width=5, data_description(),style="text-align: justify;font-size: 16px"),
                                     column(width=7,img(src="Project.png",width="75%",style="float: left; padding-left: 200px",title="Site C Project"))),
                                     br(),
                                     fluidRow(column(width=5),
                                     column(width=7,img(src="Antenna.png",title="A fixed-station antenna on the Peace River.", width="75%",style="float: left; padding-left: 200px"))))),
                            # TEXT OUTPUT FOR WELCOME PAGE
                            tabPanel("Map",
                                     titlePanel(h1("Fixed Receiver Locations in the Peace River Basin",align='center')),
                                     div("The fixed array has been operated from April through October each year since 2019.",style='text-align: center;'), 
                                     br(), 
                                     div(img(src='2021Site C Report.png',height="55%",width="55%"),style="text-align: center;")), # STUDY AREA MAP FROM REPORT WITH FIXED RECEIVER LOCATIONS
                            tabPanel("Plots",
                                     sidebarLayout(
                                       sidebarPanel(width=3,
                                        # make hr() black
                                       tags$head(tags$style(HTML("hr {border-top: 1px solid #000000;}"))),
                                       # play button size
                                       tags$head(tags$style(type='text/css', ".slider-animate-button { font-size: 20pt !important;}")),
                                       # shift play button to left and pad away from slider
                                       tags$head(tags$style(type='text/css', ".slider-animate-container { text-align: left; margin-top: -2px}")),
                                       # remove minor ticks on animation slider
                                       tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),
                                       h1("Plot settings"),
                                       hr(),
                                       radioButtons("basemap", "Select basemap.", choices = c("Terrain", "Satellite"), selected = "Terrain"),
                                       actionButton("center_map", "Re-center Map"),
                                       br(),
                                       br(),
                                       uiOutput("choiceUI"),
                                       uiOutput("lifestageUI"),
                                       br(),
                                       sliderTextInput("anim_speed","Select animation speed.",choices = c("Slowest","Slower","Faster","Fastest"), selected ="Faster",grid = TRUE),
                                       br(),
                                       uiOutput("dateUI"),
                                       ),
    mainPanel(
      tags$head(tags$style(HTML('.irs-from, .irs-to, .irs-min, .irs-max{visibility: hidden !important;}'))),
      tabsetPanel(id="tabs",
        tabPanel("Individual Movements",
                mainPanel(leafletOutput("map1", width = "85vh", height = "85vh"))),
        tabPanel("Seasonal Distribution",
                mainPanel(leafletOutput("map2",height="85vh",width = "85vh")))
                )
             )
        )
    )
)
)
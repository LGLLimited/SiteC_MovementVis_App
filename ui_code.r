# UI ####
ui <- fluidPage(titlePanel(title=img(src="BC-Hydro-Logo.png",height="56px",width="100px",style="float: right;"),
                           windowTitle="Site C Fish Movement Assessment"),
                tabsetPanel(id="main",
                            tabPanel("Info",
                                     fluidPage(
                                       titlePanel(h1("Site C Fish Movement Assessment",align='center')),
                                       br(),br(),
                                     fluidRow(
                                     column(width=4, data_description(),style="text-align: justify;font-size: 16px;padding-right:30px"),
                                     column(width=4,style="padding:0px;text-align:center;",offset=0,
                                            img(src="Project.png",width="90%",title="Aerial view of the Site C project.",style="padding-bottom:10px;"), 
                                            img(src="airplane.png",width="90%",title="Fixed-wing aircraft with radio antenna for mobile tracking.",style="padding-bottom:10px;")),
                                            #br(),
                                      column(width=4,offset=0,style="padding:0px;text-align:center;",
                                            img(src="Antenna.png",width="90%",title="A fixed-station antenna on the Peace River.",style="padding-bottom:10px;"),
                                            img(src="Moberly River 2.JPG",width="90%",title="Helicopter for mobile tracking.",style="padding-bottom:10px;"))
                                            ))),#style="float: left; padding-left: 20px",title=,#)),
                                    
                            # TEXT OUTPUT FOR WELCOME PAGE
                            tabPanel("Map",
                                     titlePanel(h1("Fixed Receiver Locations in the Peace River Basin",align='center')),
                                     div("The fixed array has been operated from April through October each year since 2019.",style='text-align: center;'), 
                                     br(), 
                                     div(img(src='2021Site C Report.png',height="55%",width="55%"),style="text-align: center;")), # STUDY AREA MAP FROM REPORT WITH FIXED RECEIVER LOCATIONS
                            tabPanel("Plots",
                                     sidebarLayout(
                                       sidebarPanel(width=3,
                                            fluidRow(
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
                                          fluidRow(HTML("<b>Plot description.</b>"),textOutput("plot_desc"))),
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
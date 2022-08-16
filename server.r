
server <- function(input, output, session) {
  
# Basemap parameters ####
  
  basemap <- reactive({if_else(input$basemap=="Satellite", "Esri.WorldImagery","Esri.WorldStreetMap")})
  
  fps <- reactive({case_when(input$anim_speed=="Slowest" ~ 2000,
                             input$anim_speed=="Slower"  ~ 1500,
                             input$anim_speed=="Faster"  ~ 1000, # 1 second per row of data
                             input$anim_speed=="Fastest" ~ 500)
  })
  
  observeEvent(input$center_map, {
    if(input$tabs=="Individual Movements"){
    center_map("map1")
     }else{
      center_map("map2")
       }
  })
  
# Individual Map Params ####
  
  output$map1 <- renderLeaflet({make_map(basemap, peace_network, location_pts) %>% 
      addLegendAwesomeIcon(iconSet, orientation = 'horizontal', position="topright") %>%  
      addLegend(colors=paste0(c('black','red'),"; border-radius: 50%; width:",10,"px; height:",10,"px;"),
                labels=c("Site C - Pre-diversion","Site C - Diverted"), opacity=1,position = "topright") %>% 
      addControl(layerId="ind_title",
                 html = ind_title(),
                 className = 'map-title')
    })
  
  filterFish <- reactive({
    req(input$fish)
    ind_d %>% dplyr::filter(fish_name==input$fish)
    
  })

  filterDate <- reactive({
    req(input$index)
    filterFish() %>% dplyr::filter(Datetime==as.POSIXct(input$index, tz="UTC"))
  })
  
  ind_title <- reactive({
    sp <- unique(filterFish()$Species)
    tag <- unique(filterFish()$tagcode)
    ls <- unique(filterFish()$Life_Stage)
    
    tags$div(
      map_title_tag, HTML(sp,"<br> Tag #:",tag,"<br>",ls)#,"<br>",format(as.POSIXct(input$index,tz="UTC"),"%b %d, %Y"))
    )
  })
  
  date_title <- reactive({
    tags$div(map_title_tag,HTML(format(as.POSIXct(input$index,tz="UTC"),"%b %d, %Y")))
    })
  
  iconSet <- awesomeIconList(
    `Hauled`=makeAwesomeIcon(icon="fish", library = "fa", text=fontawesome::fa('fish'), markerColor = "red", iconColor = "black"),
    `Movement`=makeAwesomeIcon(icon="fish", library="fa",text=fontawesome::fa('fish'),markerColor = "lightgray", iconColor = "black")
  )
  
 
  
  observeEvent({input$index
    input$basemap
    input$tabs}, {
      leafletProxy("map1", data = filterDate()) %>%
        clearGroup("fish") %>%
        clearGroup("ref") %>% 
        removeControl("date_title") %>% 
        addAwesomeMarkers(lng = ~ Longitude,  
                          lat = ~ Latitude,
                          icon= ~iconSet[haul],
                          label= ~HTML(Species,"<br>", tagcode,"<br>",Life_Stage),
                          group="fish") %>% 
        addCircleMarkers(data=location_pts,
                         lng=~long,
                         lat=~lat,
                         group = "ref",
                         stroke=FALSE,
                         fillOpacity = 1,
                         fillColor = ifelse(filterDate()$Datetime>=as.POSIXct("2020-10-03",tz="UTC"),'red','black'),
                         label = ~StreamName,
                         labelOptions = labelOptions(direction = 'top'),
                         radius=5) %>% 
        addControl(layerId="date_title",
                   html = date_title(),
                   className = 'map-title')
       
    })
# Seasonal Map Params ####
  
  output$map2 <- renderLeaflet({
    make_map(basemap, peace_network, location_pts) %>% 
      addLegend(title = "Detection Type",
                labels=factor(c("Release", "Station", "Mobile"), levels=c("Release", "Station", "Mobile")),
                colors=paste0(colors(),"; border-radius: 50%; width:",15,"px; height:",15,"px;"),opacity = 1) %>% 
    addLegend(colors=paste0(c('black','red'),"; border-radius: 50%; width:",10,"px; height:",10,"px;"),labels=c("Site C - Pre-diversion","Site C - Diverted"),opacity = 1)
      #addControl(layerId="sp_title",
      #           html = sp_title(),
        #         className = 'map-title')
    })
  
    dataset <- reactive({
      req(input$interval)
    d <-  if(input$interval=="Monthly"){1}else{2}
      d_seas[[d]]
      })
 
    
dat1 <-  reactive({
  req(input$species)
  dataset() %>% filter(Species==input$species)})

choices <- reactiveValues()

observeEvent(input$species, {
  choices$lifestage <- sort(unique(dat1()$Life_Stage))
  })

dat <- reactive({
  req(input$lifestage)
  dat1() %>% filter(Life_Stage==input$lifestage)
  })


  filterTime <- reactive({
     req(input$month)
    dat() %>% dplyr::filter(Time==input$month)
  })
  
  
n <- reactive({
  req(input$species,input$month,input$interval)
  if(input$interval=="Monthly"){
  n_month %>% dplyr::filter(Species==input$species, Time==input$month, Life_Stage==input$lifestage) %>% pull(n)
   }else{
  n_week %>% dplyr::filter(Species==input$species, Time==input$month, Life_Stage==input$lifestage) %>% pull(n) }
  
  })

#sp_title <-  reactive({
 # tags$div(map_title_tag, HTML(input$lifestage, input$species))})
  
  seas_title <- reactive({
    tags$div(map_title_tag,HTML(input$lifestage, input$species,"<br>",input$month,"<br>",paste0("n=",n())))})
  
colors <- reactive({
    req(input$basemap)
    if(input$basemap=="Terrain"){
      c("#1aeb02","#ff9305","#ff66fc")#"#ff9305")
    }else{
      c("#1aeb02","#ff9305","#ff66fc")#"#ff9305")
     # c("#1aeb02","#ff66fc","#f0ea4d")
    }
  })
  
  observeEvent({input$month
                input$species
                input$lifestage
                input$basemap
                input$tabs
                }, 
    {
    
      pal <- colorFactor(colors(),
                         levels=factor(c("Release","Station","Mobile"),
                                       levels = c("Release","Station","Mobile")),ordered = TRUE)
                         
      leafletProxy("map2") %>%
        clearGroup("fish") %>%
        clearGroup("ref") %>% 
        removeControl("seas_title") %>% 
        addCircleMarkers(data = filterTime(),
                         lng = ~Longitude,  
                         lat = ~Latitude,
                         fillOpacity = .7,
                         stroke=FALSE,
                         label = paste0("",filterTime()$n), # causes crash when changing species found the paste0("") solution on SO:https://gis.stackexchange.com/questions/333997/error-while-rendering-leaflet-map-on-shiny
                         fillColor = ~pal(Type),#if_else(input$basemap=="Terrain",'blue',"#f0ea4d"),
                         radius = ~suppressWarnings(((n/max(n))*20)+5), # would throw warnings when switching certain species
                         group="fish") %>% 
        addCircleMarkers(data=location_pts,
                         lng=~long,
                         lat=~lat,
                         group = "ref",
                         stroke=FALSE,
                         fillOpacity = 1,
                         fillColor = case_when(input$interval=="Weekly" & all(filterTime()$Timestart >= as.POSIXct("2020-10-03",tz="UTC")) | 
                                               input$interval=="Monthly" & all(filterTime()$Timestart >= as.POSIXct("2020-10-01 00:00:00",tz="UTC")) ~  'red',
                                               TRUE ~ 'black'),
                         label = ~StreamName,
                         labelOptions = labelOptions(direction = 'top'),
                         radius=5) %>%
              addControl(layerId="seas_title",html = seas_title(), className = 'map-title')
    })
  
# Map title HTML style ####  
map_title_tag <-  tags$style(HTML("
  .leaflet-control.map-title {
    transform: translate(-50%,20%);
    position: relative !important;
    left: 50%;
    width:300px;
    text-align: center;
    padding-left: 5px;
    padding-right: 5px;
    background: rgba(255,255,255,1);
    font-weight: bold;
    font-size: 24px;
  }
"))
  

# UI outputs ####  
  output$choiceUI <- renderUI({
    if(input$tabs=="Individual Movements"){
      selectInput("fish","Select a fish.", choices=sort(unique(ind_d$fish_name)), selected = "Bull Trout 149.360 496")
    }else{
      selectInput("species","Select a species.",choices=unique(d2$Species), selected="Bull Trout")
    }
  })
  
  output$lifestageUI <- renderUI({
    if(input$tabs=="Seasonal Distribution"){
      tagList(
        selectInput("lifestage","Select life stage.", choices=choices$lifestage),
        selectInput("interval","Choose time step.",choices=c("Monthly", "Weekly"), selected="Monthly")
        )
        
        } 
    else {NULL}
    
    })

 # seas_choices <- reactive({unique(filterLifestage()$Time)})
   #  #if(input$interval=="Monthly"){
   #    unique(filterLifestage()$MoYr)#} 
   #  #else {
   #  #  unique(filterLifestage()$WkYr)}
   #  })
   
  #output$choices <- renderText({seas_choices()})
  
  output$dateUI <- renderUI({
    if(input$tabs=="Individual Movements"){
      sliderTextInput(inputId = "index", "Play animation:",
                      choices=unique(filterFish()$Datetime),
                      animate=animationOptions(interval=fps(),loop=FALSE),
                      dragRange=FALSE,
                      hide_min_max = TRUE,
                      force_edges = TRUE)
    }else{
      sliderTextInput("month",label="Play animation:",
                      choices=unique(dat()$Time),
                      force_edges = TRUE,
                      animate=animationOptions(interval=fps(),loop=FALSE))
    }
    
  })
  
}

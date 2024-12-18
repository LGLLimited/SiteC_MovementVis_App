server <- function(input, output, session) {
  output$data_desc <- renderText(paste0("Data current as of ",max_data_date,"."))

# Basemap parameters ########################################################################################################

# Individual Map Params ###############################################################################################################
  output$map1 <- renderLeaflet({
    make_map(basemap, peace_network, location_pts) %>% 
      addLegendAwesomeIcon(iconSet, orientation = 'horizontal', position="topright") 
  })
## Basemap observer ===============================================================================================================  
  observe({ 
    leafletProxy(map())  %>% 
      addProviderTiles(basemap(),layerId = "base") %>% 
      clearGroup("Rivers") %>% 
      addPolylines(
        data = peace_network,
        opacity=case_when(basemap()=="Esri.WorldImagery"~.6, TRUE ~ 0.3),
        label = ~StreamName,
        weight = ~if_else(lwd=="Peace",8,2),
        labelOptions = labelOptions(
          textOnly = TRUE, noHide = FALSE,textsize = 12,direction = 'top',
          style=list("color"=if_else(basemap()=="Esri.WorldImagery","white","black"))
        ),
        group="Rivers",
        highlightOptions = highlightOptions(bringToFront = TRUE, weight = 8,color ='#1c8eff', sendToBack = TRUE)
      ) %>%
      addCircleMarkers(
        data=location_pts,
        lng=~long,
        lat=~lat,
        group = "ref",
        #stroke=FALSE,
        fillOpacity = 1,
        color='black',
        weight=2,
        fillColor = 'white',
        label = ~StreamName,
        labelOptions = labelOptions(direction = 'top'),
        radius=5
      )
  })
# Receiver observer  ===============================================================================================================
  observe({
    color <- if_else(input$basemap=="Terrain","#545353","white")
    if(input$receivers){
      leafletProxy(map()) %>% 
        addCircleMarkers(
          data=receivers,group="receivers",color = color, lng = ~lon,
          lat = ~lat,label=~dp_StatnNm,radius = 1,opacity=1
        ) %>% 
        addLegend(
            colors = paste0(color,"; border-radius: 50%; width:",8,"px; height:",8,"px;"),opacity = 1,
            labels="Fixed-station receivers",
            layerId="receiver_legend",
            group = "receivers",
            position='topright'
          )
    }else{
      leafletProxy(map()) %>% clearGroup("receivers") %>% removeControl("receiver_legend")
    }
  })

  filterFish <- reactive({
    req(input$fish)
    ind_d %>% dplyr::filter(fish_name==input$fish)
  })

  filterDate <- reactive({
    req(input$index)
    filterFish() %>% 
      dplyr::filter(Datetime==as.POSIXct(input$index, tz="UTC"))
  })

  date_title <- reactive({
    sp <- unique(filterFish()$Species)
    tag <- unique(filterFish()$tagcode)
    ls <- unique(filterFish()$Life_Stage)
    tags$div(
        map_title_tag,
        HTML(sp,"<br> Tag #",tag,"<br>",ls,"<br>",format(as.POSIXct(input$index,tz="UTC"),"%b %d, %Y"))
      )
    })

  iconSet <- awesomeIconList(
    `Hauled`=makeAwesomeIcon(
      icon="fish", library = "fa", text=fontawesome::fa('fish'), 
      markerColor = "red", iconColor = "black"
    ),
    `Movement`=makeAwesomeIcon(
      icon="fish", library="fa",text=fontawesome::fa('fish'),
      markerColor = "lightgray", iconColor = "black"
    )
  )

  output$plot_desc <- renderText({
    req(input$tabs)
    if(input$tabs=="Individual Movements"){
      text <- "This plot animates detections of tagged individuals through time.
      The fish marker appears as gray for volitional movements, and red for movements 
      after the individual has been trap and hauled at the temporary upstream fish passage facility."
    }else{
      text <- "This plot animates counts of tagged individuals at each detection location through time at 
      the selected time step. 
      The size of the circle is proportional to the number of unique individuals detected at a location. 
      If <b>Show detections by type</b> is selected, circles are coloured to distinguish between release locations, 
      detections at fixed receivers, mobile detections, and recapture PIT tag detections."
    }
    text
  })


  observeEvent({
    input$index
    input$basemap
    input$tabs
  },{
  leafletProxy("map1", data = filterDate()) %>%
    addAwesomeMarkers(
      layerId = "fish", lng = ~ Longitude, lat = ~ Latitude,
      icon= ~ iconSet[haul], label= ~HTML(Species,"<br>", tagcode,"<br>",Life_Stage),
      group="fish"
    ) %>% 
    addCircleMarkers(
      layerId="points",
      data=location_pts,
      lng=~long,
      lat=~lat,
      color = 'black',
      weight = 2,
      opacity = 1,
      #stroke='black',
      fillOpacity = 1,
      fillColor = ifelse(filterDate()$Datetime>=as.POSIXct("2020-10-03",tz="UTC"),'black','white'),
      label = ~StreamName,
      labelOptions = labelOptions(direction = 'top'),
      radius=5
    )%>% 
    addControl(
      layerId="date_title",
      html = date_title(),
      className = 'map-title'
    )
  })

# Seasonal Map Params #######################################################################################################################################
  output$map2 <- renderLeaflet({
    make_map(basemap, peace_network,  location_pts) 
  })
  
## Det_type observer  ================================================================================================================================
  observe({
    req(!is.null(input$det_type))
    if(input$det_type & map()=="map2"){
      leafletProxy(map()) %>%
      addLegend(
        title = "Detection Type",layerId = "det_type",
        labels=factor(c("Release", "Station", "Mobile", "PIT"), levels=c("Release", "Station", "Mobile", "PIT")),
        colors=paste0(colors(),"; border-radius: 50%; width:",15,"px; height:",15,"px;"),opacity = 1
      )
    }else{
      leafletProxy(map()) %>% removeControl("det_type")
    }
  })

  dataset <- reactive({
    req(input$interval)
    d_seas[[input$interval]]
  })

  dat1 <-  reactive({
    req(input$species)
    dataset() %>% filter(Species==input$species)
  })

  choices <- reactiveValues()

  observeEvent(input$species, {
    choices$lifestage <- sort(unique(dat1()$Life_Stage))
  })

  dat <- reactive({
    req(input$lifestage)
    dat1() %>% filter(Life_Stage==input$lifestage) %>% 
    # Point radius on map- scale n from 5px to 30 px max
      mutate(radius=((n-min(n)) / (max(n)-min(n))) *(40-8) + 8)
  })

  filterTime <- reactive({
      req(input$month)
      dat() %>% dplyr::filter(Time==input$month)
  })
  
  n <- reactive({
    req(input$species,input$month,input$interval)
    n_seas[[input$interval]] %>% 
      dplyr::filter(Species==input$species, Time==input$month, Life_Stage==input$lifestage) %>% 
      pull(n)
  })

  seas_title <- reactive({
    tags$div(
      map_title_tag,
      HTML(input$lifestage, input$species,"<br>",input$month,"<br>", paste0("Number of tagged fish = ",n()))
    )
  })

  colors <- reactive({
    req(input$basemap,!is.null(input$det_type))
    if(input$det_type){
        c("#1aeb02","#ff9305","#ff66fc", "#e70505")
    } else if (input$basemap=="Terrain"){
      c("blue","blue","blue", "blue")
    }else{
      c("yellow","yellow","yellow" ,"yellow")
    }
  })

  observeEvent({
    filterTime()
    colors()
    input$basemap
    input$tabs
  }, {
    pal <- colorFactor(
      colors(), 
      levels=factor(c("Release","Station","Mobile", "PIT"),
      levels = c("Release","Station","Mobile", "PIT")),
      ordered = TRUE
    )
    leafletProxy("map2") %>%
      clearGroup("fish") %>%
      addCircleMarkers(
        group="fish", data = filterTime(), lng = ~Longitude, lat = ~Latitude,
        fillOpacity = .7, stroke=FALSE,
        label = paste0("",filterTime()$n), # causes crash when changing species found the paste0("") 
        # ^^^ solution on SO:https://gis.stackexchange.com/questions/333997/error-while-rendering-leaflet-map-on-shiny
        fillColor = ~pal(Type),#if_else(input$basemap=="Terrain",'blue',"#f0ea4d"),
        radius = ~radius
      ) %>% #~suppressWarnings(((n/max(n))*20)+5)) %>% # would throw warnings when switching certain species
      addCircleMarkers(
        layerId="points", data=location_pts,lng=~long, lat=~lat,
        group = "ref", color='black', opacity=1, fillOpacity = 1,
        weight=2,
        fillColor = case_when(
          nrow(filterTime())==0~"white",
          input$interval=="Weekly" & all(filterTime()$Timestart >= as.POSIXct("2020-10-03",tz="UTC")) | 
            input$interval=="Monthly" & all(filterTime()$Timestart >= as.POSIXct("2020-10-01 00:00:00",tz="UTC")) 
              ~  'black',
          TRUE ~ 'white'
        ),
        label = ~StreamName,
        labelOptions = labelOptions(direction = 'top'),
        radius=5
      ) %>%
      addControl(layerId="seas_title",html = seas_title(), className = 'map-title')
  })

# Map title HTML style #########################################################################################################################################
map_title_tag <-  tags$style(HTML("
  .leaflet-control.map-title {
    transform: translate(-50%,20%);
    position: relative !important;
    left: 50%;
    width: 275px;
    text-align: center;
    padding-left: 5px;
    padding-right: 5px;
    background: rgba(255,255,255,1);
    font-weight: bold;
    font-size: 18px;
  }
"))
  
# UI outputs ######################################################################################################################################################
  
  output$choiceUI <- renderUI({
    if(input$tabs=="Individual Movements"){
      selectInput(
        "fish","Select individual.", 
        choices=sort(unique(ind_d$fish_name)), 
        selected = "Bull Trout 149.360 496"
      )
    }else{
      selectInput(
        "species","Select species.",
        choices=unique(dataset()$Species), 
        selected="Bull Trout"
      )
    }
  })

  output$lifestageUI <- renderUI({
    if(input$tabs=="Seasonal Distribution"){
      tagList(
        selectInput("lifestage","Select life stage.", choices=choices$lifestage),
        selectInput("interval","Select time step.",choices=c("Monthly", "Weekly"), selected="Monthly"),
        checkboxInput("det_type",HTML("<b>Show detections by type.</b>"),value = FALSE)
      )
    } else{NULL}
  })

  output$dateUI <- renderUI({
    if(input$tabs=="Individual Movements"){
      sliderTextInput(
        inputId = "index", "Play animation.",
        choices=unique(filterFish()$Datetime),
        animate=animationOptions(interval=fps(),loop=FALSE),
        dragRange=FALSE,
        hide_min_max = TRUE,
        force_edges = TRUE
      )
    }else{
      sliderTextInput(
        "month",label="Play animation.",
        choices=unique(dat()$Time),
        force_edges = TRUE,
        animate=animationOptions(interval=fps(),loop=FALSE)
      )
    }
  })
}

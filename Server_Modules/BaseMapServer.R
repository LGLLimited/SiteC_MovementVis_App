BaseMapServ <- function(id) {
  moduleServer(id, function(input, output, session) {
    basemap <- reactive({
      if_else(input$basemap == "Satellite", "Esri.WorldImagery", "Esri.WorldStreetMap")
    })

    map <- reactive({
      if_else(input$tabs == "Individual Movements", "map1", "map2")
    })

    fps <- reactive({
      case_when(
        input$anim_speed == "Slowest" ~ 2000,
        input$anim_speed == "Slower"  ~ 1500,
        input$anim_speed == "Faster"  ~ 1000, # 1 second per row of data
        input$anim_speed == "Fastest" ~ 500
      )
    })

    observe({ #If the `map` value changes, center the map
      center_map(map())
    })

    observeEvent(input$center_map, {
      center_map(map()) #If the center_map input button is pushed, center map
    })

    observeEvent(fps(), {
      if (input$tabs == "Individual Movements") {
        updateSliderTextInput(session, inputId = "index", selected = input$index)
      } else {
        updateSliderTextInput(session, inputId = "month", selected = input$month)
      }
    })
  })
}

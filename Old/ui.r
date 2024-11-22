ui <- shiny::htmlTemplate(
  "www/index.html",
  stationMap = stationMap_ui,
  movementMap = movementMap_ui)

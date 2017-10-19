#wenzel-app_3

library(shiny)
library(leaflet)
library(RColorBrewer)
library(sp)
library(leaflet.extras)
ausstellungsorte <- dbGetQuery(conn = con, "SELECT DISTINCT place FROM regesten_wenzel")
ausstellungsorte <- rbind(ausstellungsorte, data.frame(place = "Eingabe"))
orte <- dbGetQuery(conn = con, "SELECT DISTINCT Name, Latitude AS 'lat', Longitude AS 'long' FROM verortung_2 WHERE Longitude<>''")
orte$long <- as.numeric(orte$long)
orte$lat <- as.numeric(orte$lat)
info <- dbGetQuery(conn = con, "SELECT e1_place AS 'Name', place, e1 AS 'empfänger', freq, start, end, abstract FROM regesten_wenzel")
info$start <- as.Date(info$start)
info$end <- as.Date(info$end)
info$freq <- as.integer(info$freq)
info <- merge(info, orte, by = 'Name')

ausstellungsorte_2 <- dbGetQuery(conn = con, "SELECT place, Longitude AS 'long', Latitude AS 'lat' FROM wenzel_verortung")
ausstellungsorte_2$long <- as.numeric(ausstellungsorte_2$long)
ausstellungsorte_2$lat <- as.numeric(ausstellungsorte_2$lat)
info_2 <- dbGetQuery(conn = con, "SELECT place, start, end, e1 AS 'empfänger', freq FROM regesten_wenzel")
info_2$start <- as.Date(info_2$start)
info_2$end <- as.Date(info_2$end)
info_2$freq <- as.integer(info_2$freq)
info_2 <- merge(info_2, ausstellungsorte_2, by = 'place')

empfänger <- dbGetQuery(conn = con, "SELECT DISTINCT e1 FROM regesten_wenzel")
empfänger <- rbind(empfänger, data.frame(e1 = "Eingabe"))

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                dateInput("start", label = h3("Startdatum"), value = "1375-12-31"),
                dateInput("end", label = h3("Enddatum"), value = "1400-01-01"),
                selectInput("place", label = h3("Ausstellungsort"), 
                            choices = ausstellungsorte$place, selected = "Eingabe"),
                selectInput("empfänger", label = h3("Empfänger"), 
                            choices = empfänger$e1, selected = "Eingabe"),
                numericInput("freq_min", label = h3("Minimale Frequenz"), value = 1),
                numericInput("freq_max", label = h3("Maximale Frequenz"), value = 150)
  )
)


server <- function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    if (input$place == 'Eingabe' && input$start == '1375-12-30' && input$end == '1399-12-31' && input$empfänger == 'Eingabe' && input$freq_min == 1 && input$freq_max == 150) {
      data <- info[ which(info$end <= input$end), ]
    }
    else if (input$place != 'Eingabe' && input$start == '1375-12-30' && input$end == '1399-12-31' && input$empfänger == 'Eingabe'&& input$freq_min == 1 && input$freq_max == 150) {
      data <- info[ which(info$place == input$place), ]
    }
    else if (input$place == 'Eingabe' && input$start != '1375-12-30' && input$end == '1399-12-31' && input$empfänger == 'Eingabe'&& input$freq_min == 1 && input$freq_max == 150) {
      data <- info[ which(info$start >= input$start), ]
    }
    else if (input$place == 'Eingabe' && input$start == '1375-12-30' && input$end != '1399-12-31' && input$empfänger == 'Eingabe'&& input$freq_min == 1 && input$freq_max == 150) {
      data <- info[ which(info$end <= input$end), ]
    }
    else if (input$place != 'Eingabe' && input$start != '1375-12-30' && input$end == '1399-12-31' && input$empfänger == 'Eingabe'&& input$freq_min == 1 && input$freq_max == 150) {
      data <- info[ which(info$place == input$place & info$start >= input$start), ]
    }
    else if (input$place != 'Eingabe' && input$start == '1375-12-30' && input$end != '1399-12-31' && input$empfänger == 'Eingabe'&& input$freq_min == 1 && input$freq_max == 150) {
      data <- info[ which(info$end <= input$end & info$end <= input$end), ]
    }
    else if (input$place == 'Eingabe' && input$start != '1375-12-30' && input$end != '1399-12-31' && input$empfänger == 'Eingabe'&& input$freq_min == 1 && input$freq_max == 150) {
      data <- info[ which(info$start >= input$start & info$end <= input$end), ]
    }
    else if (input$place != 'Eingabe' && input$start != '1375-12-30' && input$end != '1399-12-31' && input$empfänger == 'Eingabe'&& input$freq_min == 1 && input$freq_max == 150) {
      data <- info[ which(info$place == input$place & info$start >= input$start & info$end <= input$end), ]
    }
    else if (input$place == 'Eingabe' && input$start == '1375-12-30' && input$end == '1399-12-31' && input$empfänger != 'Eingabe'&& input$freq_min == 1 && input$freq_max == 150) {
      data <- info[ which(info$empfänger == input$empfänger), ]
    }
    else if (input$place != 'Eingabe' && input$start == '1375-12-30' && input$end == '1399-12-31' && input$empfänger != 'Eingabe'&& input$freq_min == 1 && input$freq_max == 150) {
      data <- info[ which(info$place == input$place & info$empfänger == input$empfänger), ]
    }
    else if (input$place == 'Eingabe' && input$start != '1375-12-30' && input$end == '1399-12-31' && input$empfänger != 'Eingabe'&& input$freq_min == 1 && input$freq_max == 150) {
      data <- info[ which(info$start == input$start & info$empfänger == input$empfänger), ]
    }
    else if (input$place == 'Eingabe' && input$start == '1375-12-30' && input$end != '1399-12-31' && input$empfänger != 'Eingabe'&& input$freq_min == 1 && input$freq_max == 150) {
      data <- info[ which(info$end == input$end & info$empfänger == input$empfänger), ]
    }
    else if (input$place != 'Eingabe' && input$start == '1375-12-30' && input$end != '1399-12-31' && input$empfänger != 'Eingabe'&& input$freq_min == 1 && input$freq_max == 150) {
      data <- info[ which(info$place == input$place & info$start == input$start & info$empfänger == input$empfänger), ]
    }
    else if (input$place != 'Eingabe' && input$start != '1375-12-30' && input$end == '1399-12-31' && input$empfänger != 'Eingabe'&& input$freq_min == 1 && input$freq_max == 150) {
      data <- info[ which(info$place == input$place & info$end == input$end & info$empfänger == input$empfänger), ]
    }
    else if (input$place == 'Eingabe' && input$start != '1375-12-30' && input$end != '1399-12-31' && input$empfänger != 'Eingabe'&& input$freq_min == 1 && input$freq_max == 150) {
      data <- info[ which(info$start == input$start & info$end == input$end & info$empfänger == input$empfänger), ]
    }
    else if (input$place == 'Eingabe' && input$start == '1375-12-30' && input$end == '1399-12-31' && input$empfänger == 'Eingabe'&& input$freq_min != 1 && input$freq_max == 150) {
      data <- info[ which(info$freq >= input$freq_min & info$freq <= input$freq_max), ]
    }
    else if (input$place == 'Eingabe' && input$start == '1375-12-30' && input$end == '1399-12-31' && input$empfänger == 'Eingabe'&& input$freq_min == 1 && input$freq_max != 150) {
      data <- info[ which(info$freq >= input$freq_min & info$freq <= input$freq_max), ]
    }
    else if (input$place == 'Eingabe' && input$start == '1375-12-30' && input$end == '1399-12-31' && input$empfänger == 'Eingabe'&& input$freq_min != 1 && input$freq_max != 150) {
      data <- info[ which(info$freq >= input$freq_min & info$freq <= input$freq_max), ]
    }
    else if (input$place != 'Eingabe' && input$start != '1375-12-30' && input$end != '1399-12-31' && input$empfänger != 'Eingabe'&& input$freq_min == 1 && input$freq_max == 150) {
      data <- info[ which(info$place == input$place & info$start == input$start & info$end == input$end & info$empfänger == input$empfänger & info$freq >= input$freq_min & info$freq <= input$freq_max), ]
    }
    data
    if (nrow(data) == 0) {
      data <- info
      showModal(modalDialog(
        title = "Fehler",
        "Keine Regesten in der Auswahl vorhanden.",
        easyClose = TRUE,
        footer = NULL
      ))
    }
    data
  })
  
  filteredData2 <- reactive({
    if (input$place == 'Eingabe' && input$start == '1375-12-30' && input$end == '1399-12-31' && input$empfänger == 'Eingabe' && input$freq_min == 1 && input$freq_max == 150) {
      data <- info_2[ which(info_2$end <= input$end), ]
    }
    else if (input$place != 'Eingabe' && input$start == '1375-12-30' && input$end == '1399-12-31' && input$empfänger == 'Eingabe' && input$freq_min == 1 && input$freq_max == 150) {
      data <- info_2[ which(info_2$place == input$place), ]
    }
    else if (input$place == 'Eingabe' && input$start != '1375-12-30' && input$end == '1399-12-31' && input$empfänger == 'Eingabe' && input$freq_min == 1 && input$freq_max == 150) {
      data <- info_2[ which(info_2$start >= input$start), ]
    }
    else if (input$place == 'Eingabe' && input$start == '1375-12-30' && input$end != '1399-12-31' && input$empfänger == 'Eingabe' && input$freq_min == 1 && input$freq_max == 150) {
      data <- info_2[ which(info_2$end <= input$end), ]
    }
    else if (input$place != 'Eingabe' && input$start != '1375-12-30' && input$end == '1399-12-31' && input$empfänger == 'Eingabe' && input$freq_min == 1 && input$freq_max == 150) {
      data <- info_2[ which(info_2$place == input$place & info_2$start >= input$start), ]
    }
    else if (input$place != 'Eingabe' && input$start == '1375-12-30' && input$end != '1399-12-31' && input$empfänger == 'Eingabe' && input$freq_min == 1 && input$freq_max == 150) {
      data <- info_2[ which(info_2$place == input$place & info_2$end <= input$end), ]
    }
    else if (input$place == 'Eingabe' && input$start != '1375-12-30' && input$end != '1399-12-31' && input$empfänger == 'Eingabe' && input$freq_min == 1 && input$freq_max == 150) {
      data <- info_2[ which(info_2$start >= input$start & info_2$end <= input$end), ]
    }
    else if (input$place != 'Eingabe' && input$start != '1375-12-30' && input$end != '1399-12-31' && input$empfänger == 'Eingabe' && input$freq_min == 1 && input$freq_max == 150) {
      data <- info_2[ which(info_2$place == input$place & info_2$start >= input$start & info_2$end <= input$end), ]
    }
    else if (input$place == 'Eingabe' && input$start == '1375-12-30' && input$end == '1399-12-31' && input$empfänger != 'Eingabe' && input$freq_min == 1 && input$freq_max == 150) {
      data <- info_2[ which(info_2$empfänger == input$empfänger), ]
    }
    else if (input$place != 'Eingabe' && input$start == '1375-12-30' && input$end == '1399-12-31' && input$empfänger != 'Eingabe' && input$freq_min == 1 && input$freq_max == 150) {
      data <- info_2[ which(info_2$place == input$place & info_2$empfänger == input$empfänger), ]
    }
    else if (input$place == 'Eingabe' && input$start != '1375-12-30' && input$end == '1399-12-31' && input$empfänger != 'Eingabe' && input$freq_min == 1 && input$freq_max == 150) {
      data <- info_2[ which(info_2$start == input$start & info_2$empfänger == input$empfänger), ]
    }
    else if (input$place == 'Eingabe' && input$start == '1375-12-30' && input$end != '1399-12-31' && input$empfänger != 'Eingabe' && input$freq_min == 1 && input$freq_max == 150) {
      data <- info_2[ which(info_2$end == input$end & info_2$empfänger == input$empfänger), ]
    }
    else if (input$place != 'Eingabe' && input$start == '1375-12-30' && input$end != '1399-12-31' && input$empfänger != 'Eingabe' && input$freq_min == 1 && input$freq_max == 150) {
      data <- info_2[ which(info_2$place == input$place & info_2$start == input$start & info_2$empfänger == input$empfänger), ]
    }
    else if (input$place != 'Eingabe' && input$start != '1375-12-30' && input$end == '1399-12-31' && input$empfänger != 'Eingabe' && input$freq_min == 1 && input$freq_max == 150) {
      data <- info_2[ which(info_2$place == input$place & info_2$end == input$end & info_2$empfänger == input$empfänger), ]
    }
    else if (input$place == 'Eingabe' && input$start != '1375-12-30' && input$end != '1399-12-31' && input$empfänger != 'Eingabe' && input$freq_min == 1 && input$freq_max == 150) {
      data <- info_2[ which(info_2$start == input$start & info_2$end == input$end & info_2$empfänger == input$empfänger), ]
    }
    else if (input$place == 'Eingabe' && input$start == '1375-12-30' && input$end == '1399-12-31' && input$empfänger == 'Eingabe'&& input$freq_min != 1 && input$freq_max == 150) {
      data <- info[ which(info_2$freq >= input$freq_min & info_2$freq <= input$freq_max), ]
    }
    else if (input$place == 'Eingabe' && input$start == '1375-12-30' && input$end == '1399-12-31' && input$empfänger == 'Eingabe'&& input$freq_min == 1 && input$freq_max != 150) {
      data <- info[ which(info_2$freq >= input$freq_min & info_2$freq <= input$freq_max), ]
    }
    else if (input$place == 'Eingabe' && input$start == '1375-12-30' && input$end == '1399-12-31' && input$empfänger == 'Eingabe'&& input$freq_min != 1 && input$freq_max != 150) {
      data <- info[ which(info_2$freq >= input$freq_min & info_2$freq <= input$freq_max), ]
    }
    else if (input$place != 'Eingabe' && input$start != '1375-12-30' && input$end != '1399-12-31' && input$empfänger != 'Eingabe' && input$freq_min == 1 && input$freq_max == 150) {
      data <- info_2[ which(info_2$place == input$place & info_2$start == input$start & info_2$end == input$end & info_2$empfänger == input$empfänger  & info_2$freq >= input$freq_min & info_2$freq <= input$freq_max), ]
    }
    data
    if (nrow(data) == 0) {
      data <- info_2
      showModal(modalDialog(
        title = "Fehler",
        "Keine Regesten in der Auswahl vorhanden.",
        easyClose = TRUE,
        footer = NULL
      ))
    }
    data
  })
  
  icon.red <- makeAwesomeIcon(icon = 'ios-close', markerColor = 'red')
  icon.blu <- makeAwesomeIcon(icon = 'ios-close', markerColor = 'blue')
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(info) %>% addTiles() %>%
      fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat)) %>%
      addLayersControl(overlayGroups = c("Ausstellungsorte", "Empfänger", "HRR 1400", "Heatmap"), position = 'bottomright') %>%
      addPolygons(data = HRR1376, group = "HRR 1400", color = "FF9900", fillColor = "grey", weight = 1, label = ~as.character(HRR1400m)) %>%
      hideGroup(c("Ausstellungsorte", "Heatmap")) 
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    leafletProxy("map") %>%
      clearMarkers() %>%
      clearMarkerClusters() %>%
      addAwesomeMarkers(data = filteredData(), popup = ~as.character(abstract), label = ~as.character(Name), icon = icon.red, clusterOptions = markerClusterOptions(), group = "Empfänger")
  })
  
  observe({
    leafletProxy("map") %>%
      clearMarkers() %>%
      addAwesomeMarkers(data = filteredData2(), label = ~as.character(place), icon = icon.blu, group = "Ausstellungsorte")
  })
  
  observe({
    leafletProxy("map") %>%
      clearHeatmap() %>%
      addHeatmap(data = filteredData(), group = "Heatmap", lng = ~long, lat = ~lat,
                 blur = 20, max = 0.05, radius = 15) 
  })
  
}

shinyApp(ui = ui, server = server)
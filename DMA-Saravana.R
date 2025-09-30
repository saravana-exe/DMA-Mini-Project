# ------------------------------
# EV Route Planner Shiny App
# ------------------------------

# Required Libraries
library(shiny)
library(leaflet)
library(dplyr)
library(osrm)
library(sf)
library(ggplot2)
library(DT)
library(RColorBrewer)
library(mapview) # for mapshot in downloads

# ------------------------------
# Sample Data
# ------------------------------

sources <- data.frame(
  id = c("Kapaleeshwarar Temple, Mylapore", "Chennai Central"),
  lat = c(13.0350, 13.0827),
  lon = c(80.2630, 80.2780)
)

destinations <- data.frame(
  id = c("Tirupati", "Mahabalipuram"),
  lat = c(13.6288, 12.6130),
  lon = c(79.4192, 80.1950)
)

stations <- data.frame(
  id = paste0("CS", 1:5),
  lat = c(13.150, 13.250, 13.400, 13.520, 13.550),
  lon = c(80.150, 80.050, 79.950, 79.850, 79.750),
  points = sample(3:5, 5, replace = TRUE),
  type = sample(c("Fast","Slow"), 5, replace = TRUE)
)

# Convert to sf objects
stations_sf <- st_as_sf(stations, coords = c("lon","lat"), crs = 4326)
sources_sf <- st_as_sf(sources, coords = c("lon","lat"), crs = 4326)
destinations_sf <- st_as_sf(destinations, coords = c("lon","lat"), crs = 4326)

# ------------------------------
# UI
# ------------------------------

ui <- fluidPage(
  titlePanel("⚡ EV Route Planner"),
  sidebarLayout(
    sidebarPanel(
      numericInput("range", "EV Range (km):", value = 50, min = 10, max = 200),
      selectInput("source", "Select Source:", choices = sources$id),
      selectInput("destination", "Select Destination:", choices = destinations$id),
      checkboxInput("manualCharge", "Manual Charging Station Selection?", FALSE),
      uiOutput("manualStationUI"),
      actionButton("planRoute", "Plan Route", class = "btn-primary"),
      hr(),
      h4("Add New Station"),
      textInput("newStationID", "Station ID:", value = ""),
      selectInput("newStationType", "Type:", choices = c("Fast","Slow")),
      numericInput("newStationPoints", "Charging Points:", value = 3, min = 1, max = 10),
      helpText("Click on map to place new station."),
      hr(),
      downloadButton("downloadMap", "Download Map (PNG)"),
      downloadButton("downloadReport", "Download Report (PDF)")
    ),
    mainPanel(
      leafletOutput("map", height = 500),
      h4("Route Summary"),
      verbatimTextOutput("routeSummary"),
      DTOutput("routeTable"),
      plotOutput("barPlot")
    )
  )
)

# ------------------------------
# Server
# ------------------------------

server <- function(input, output, session) {
  
  # Reactive storage for stations
  reactiveStations <- reactiveVal(stations_sf)
  
  # Manual station selection UI
  output$manualStationUI <- renderUI({
    if(input$manualCharge){
      st <- reactiveStations() %>% st_drop_geometry()
      checkboxGroupInput("selectedStations", "Choose Charging Stations:", choices = st$id)
    }
  })
  
  # Add station by clicking map
  observeEvent(input$map_click, {
    click <- input$map_click
    if(nzchar(input$newStationID)){
      new_id <- input$newStationID
      current <- reactiveStations()
      if(new_id %in% current$id){
        showNotification("Station ID exists!", type="error")
      } else {
        new_row <- data.frame(
          id = new_id,
          points = input$newStationPoints,
          type = input$newStationType,
          geometry = st_sfc(st_point(c(click$lng, click$lat)), crs=4326)
        )
        new_sf <- st_sf(new_row)
        reactiveStations(rbind(current, new_sf))
        updateTextInput(session, "newStationID", value = "")
        showNotification(paste("Station", new_id, "added!"), type="message")
      }
    } else {
      showNotification("Enter Station ID first.", type="warning")
    }
  })
  
  # -----------------------------
  # Route calculation function
  # -----------------------------
  
  calculateRoute <- function(sourceNode, destNode, evRange, stations_sf, manual=FALSE, selectedStations=NULL){
    
    src_sf <- sources_sf %>% filter(id == sourceNode$id)
    dst_sf <- destinations_sf %>% filter(id == destNode$id)
    
    path_sf <- src_sf
    stops <- c()
    
    # Manual route
    if(manual && !is.null(selectedStations)){
      selected_st_sf <- stations_sf %>% filter(id %in% selectedStations)
      path_sf <- rbind(path_sf, selected_st_sf, dst_sf)
      stops <- paste(selectedStations, collapse=" → ")
      
    } else {
      # Auto route: Simplified, direct source → destination for demo
      path_sf <- rbind(path_sf, dst_sf)
      stops <- "None"
    }
    
    # Leg distances: simplified as straight line distances
    legs_list <- list()
    path_nodes <- path_sf
    if(nrow(path_nodes) >= 2){
      for(i in 1:(nrow(path_nodes)-1)){
        p1 <- path_nodes[i,]
        p2 <- path_nodes[i+1,]
        dist_km <- as.numeric(st_distance(p1,p2))/1000
        legs_list[[i]] <- data.frame(from=p1$id, to=p2$id, km=round(dist_km,2), charge_time=0)
      }
      legs_df <- do.call(rbind, legs_list)
    } else { legs_df <- data.frame() }
    
    list(path=path_sf, stops=stops, legs=legs_df)
  }
  
  # Reactive route
  routeData <- eventReactive(input$planRoute,{
    src_row <- sources %>% filter(id==input$source)
    dst_row <- destinations %>% filter(id==input$destination)
    calculateRoute(src_row, dst_row, input$range, reactiveStations(), input$manualCharge, input$selectedStations)
  })
  
  # -----------------------------
  # Render Leaflet Map
  # -----------------------------
  
  output$map <- renderLeaflet({
    st <- reactiveStations()
    st_df <- st %>% st_drop_geometry() %>% mutate(lon=st_coordinates(st)[,1], lat=st_coordinates(st)[,2])
    src_df <- sources_sf %>% st_drop_geometry() %>% mutate(lon=st_coordinates(sources_sf)[,1], lat=st_coordinates(sources_sf)[,2])
    dst_df <- destinations_sf %>% st_drop_geometry() %>% mutate(lon=st_coordinates(destinations_sf)[,1], lat=st_coordinates(destinations_sf)[,2])
    
    m <- leaflet() %>% addTiles() %>%
      addCircleMarkers(data=st_df, ~lon, ~lat, color=~ifelse(type=="Fast","green","orange"), radius=8, label=~paste(id, "-", points, "pts [", type,"]"), fillOpacity=0.8) %>%
      addCircleMarkers(data=src_df, ~lon, ~lat, color="blue", radius=10, label=~id, fillOpacity=1) %>%
      addCircleMarkers(data=dst_df, ~lon, ~lat, color="purple", radius=10, label=~id, fillOpacity=1)
    
    # Draw route
    res <- routeData()
    if(!is.null(res$path) && nrow(res$path)>=2){
      coords <- st_coordinates(res$path)
      m <- m %>% addPolylines(lng=coords[,1], lat=coords[,2], color="red", weight=4, opacity=0.8)
    }
    
    m
  })
  
  # Route table
  output$routeTable <- renderDT({
    req(routeData())
    datatable(routeData()$legs, rownames=FALSE, options=list(dom='t'))
  })
  
  # Route summary
  output$routeSummary <- renderText({
    req(routeData())
    res <- routeData()
    total_dist <- sum(res$legs$km, na.rm=TRUE)
    total_charge <- sum(res$legs$charge_time, na.rm=TRUE)
    paste0("🛣 Total Distance: ", round(total_dist,2), " km\n",
           "🔌 Charging Stops: ", res$stops, "\n",
           "⏱ Estimated Charging Time: ", round(total_charge,1), " mins")
  })
  
  # Barplot of stations
  output$barPlot <- renderPlot({
    st <- reactiveStations() %>% st_drop_geometry()
    ggplot(st, aes(x=id, y=points, fill=type)) +
      geom_bar(stat="identity", color="black") +
      scale_fill_manual(values=c("Fast"="green","Slow"="orange")) +
      labs(title="Charging Points per Station", x="Station", y="Points", fill="Type") +
      theme_minimal()
  })
  
  # Download Map PNG
  output$downloadMap <- downloadHandler(
    filename=function(){paste0("EVRouteMap_",Sys.Date(),".png")},
    content=function(file){
      mapview::mapshot(output$map, file=file, remove_controls="all")
    }
  )
  
  # Download Report PDF
  output$downloadReport <- downloadHandler(
    filename=function(){paste0("EVRouteReport_",Sys.Date(),".pdf")},
    content=function(file){
      pdf(file)
      print(ggplot(reactiveStations() %>% st_drop_geometry(), aes(x=id, y=points, fill=type)) +
              geom_bar(stat="identity") + labs(title="Charging Stations") + theme_minimal())
      dev.off()
    }
  )
}

# -----------------------------
# Run App
# -----------------------------
shinyApp(ui, server)

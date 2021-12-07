#' Visualisation task
#' This code contains a Shiny app to
#' visualise reptiles in ACT.
#' Created by Francisca Maron
#' Date: 07/12/2021


# Dependencies
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)
library(leaflet)
library(sf)
library(plotly)
library(htmltools)


# Read the data
load("reptiles.RData")




plot_ly(reptiles, x = ~forest2013, y = ~scientificName, type = 'bar')


ui <- bootstrapPage(
  
  #Code for creating the navigation bar at the top of the web page
   navbarPage(
    windowTitle = "Reptiles",
    theme = shinytheme("united"),
    collapsible = TRUE,
    title = "Reptiles"),
   
   #Tab with leaflet map
   tabPanel(
     "Reptile Occurences",
     
     div(class = "outer", tags$head(includeCSS("style.css")),
       
       leafletOutput("reptileLocation", width = "100%", height = "100%"),
       
       absolutePanel(id = "controls",
                     top= 75, left = 70, width = 400,
                     fixed=TRUE,
                     height = "auto",
                     pickerInput(inputId = "selectSpecies",
                                   label = "Select species",
                                   choices = unique(reptiles$scientificName),
                                   multiple = TRUE,
                                   options = pickerOptions(actionsBox = TRUE,
                                                           deselectAllText = "Deselect",
                                                           selectAllText = "Select all",
                                                           noneSelectedText= "Please select a species")),
                     plotlyOutput("barPlot")
                     )
         )
     
   )
   
   )


server <- function(input, output, session){
  reactiveReptiles <- reactive({
    req(input$selectSpecies)
    if(!is.null(input$selectSpecies)){
      reptiles %>%
        filter(scientificName %in% input$selectSpecies) %>%
        mutate(id = as.character(row_number()))%>%
        st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), remove = FALSE) %>%
        st_sf(crs=4326)
    }else(
      return(NULL)
    )
  })
  reactiveForest <- reactive({
    req(input$selectSpecies)
    if(!is.null(input$selectSpecies)){
      reptiles %>%
        filter(scientificName %in% input$selectSpecies)%>%
        group_by(forest2013, scientificName)%>%
        summarise(numOccurrence = n(), .groups = "drop")
    }else{
      return(NULL)
    }
  })
  
  map <- reactive({
    leaflet()%>%
      addTiles() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addScaleBar(position = "bottomright")%>%
      setView(lng = 149, lat = -35.34, zoom = 10)
  })
  
  output$reptileLocation <- renderLeaflet({
   map()
  })
  
  observeEvent(input$selectSpecies, {
    proxy <- leafletProxy("reptileLocation") %>%
      clearMarkers()%>%
      clearShapes()%>%
      removeMarker(layerId = reactiveReptiles()$id)
      
    
    if(!is.null(input$selectSpecies) && !is.null(reactiveReptiles()) && (nrow(reactiveReptiles())>0)){
      proxy %>%
        addCircleMarkers(data = reactiveReptiles(),
                         fillColor = "orange",
                         stroke = FALSE,
                         fillOpacity = .6,
                         radius = 8,
                   layerId = ~id,
                         group = "speciesPoints") %>%
        flyToBounds(lng1 = min(reactiveReptiles()$decimalLongitude),
                    lng2 = max(reactiveReptiles()$decimalLongitude),
                    lat1 = min(reactiveReptiles()$decimalLatitude),
                    lat2 = max(reactiveReptiles()$decimalLatitude))
    }else{
      leafletProxy("reptileLocation")
    }
    
  }, ignoreNULL = TRUE)
  
  output$barPlot <- renderPlotly({
    plot_ly(reactiveForest(), x = ~forest2013, y = ~numOccurrence, type = 'bar', color = ~scientificName) %>%
      layout(yaxis = list(title = 'Number of occurrences'),
             xaxis = list(title='Type of forest in 2013'),
             barmode = 'stack')
    
  })
  
}

shinyApp(ui,server)

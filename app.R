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


#Read the data
load("reptiles.RData")



ui <- bootstrapPage(
  
  #Code for creating the navigation bar at the top of the web page
   navbarPage(
    windowTitle = "Reptiles",
    theme = shinytheme("united"),
    collapsible = TRUE,
    title = "Reptiles of the Australian Capital Territory"),
   
   #Tab with leaflet map
   tabPanel(
     "Reptile Occurences",
     
     div(class = "outer", tags$head(includeCSS("style.css")),
       leafletOutput("reptileLocation", width = "100%", height = "100%"),
       
       #Sidebar with filters and plots
       absolutePanel(id = "controls", #id for style.css
                     top = 75, left = 70, width = 400, height = "auto",
                     fixed = TRUE,
                     draggable = TRUE,
                     
                     #Reactive text
                     h3(textOutput("numSpecies"), align = "left"),
                     h3(textOutput("occurrences"), align = "left"),
                     
                     #Filter by species
                     #CSS for picker input text when nothing is selected
                     tags$style(".bs-placeholder {color: #FFFFFF !important;}"),
                     pickerInput(inputId = "selectSpecies",
                                   label = h5("Select species"),
                                   choices = unique(reptiles$scientificName),
                                   multiple = TRUE,
                                   options = pickerOptions(actionsBox = TRUE,
                                                           deselectAllText = "Deselect",
                                                           selectAllText = "Select all",
                                                         noneSelectedText= "Please select species")),
                     
                     #Create tab set inside left panel
                     tabsetPanel(
                       tabPanel("Type of forest",plotlyOutput("forest")),
                       tabPanel("Data resource name", plotlyOutput("resource", height = "200%")),
                       tabPanel("Basis of record", plotlyOutput("record")),
                       tabPanel("Records by month", plotlyOutput("month"))
                     ),
                     
                     #GitHub logo to the repo
                     absolutePanel(id = "logo", 
                                   bottom = 40, 
                                   left = 60, 
                                   width = 50, 
                                   fixed=TRUE, 
                                   draggable = FALSE, 
                                   height = "auto",
                                   tags$a(href = 'https://github.com/fmaron/ala_reptiles', 
                                          tags$img(src = 'github_logo.png', height = '50', width = '50')))
                     
                     )
         )
     
   )
   
   )


server <- function(input, output, session){
  
  
  ##### Lefleat map #####
  
  
  #Reactive data, filters by species selected and converts to spatial
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
  #Map text
  
  mapText <- reactive(paste(
    "Longitude: ", reactiveReptiles()$decimalLongitude, "<br/>",
    "Latitude: ", reactiveReptiles()$decimalLatitude, "<br/>",
    "Date: ", reactiveReptiles()$eventDate, "<br/>",
    "Species: ", reactiveReptiles()$scientificName, "<br/>",
    "Type of forest: ", reactiveReptiles()$forest2013, "<br/>",
    "Data resource: ",reactiveReptiles()$dataResourceName, "<br/>",
    "Basis of record: ",reactiveReptiles()$basisOfRecord,sep = "")%>%
      lapply(htmltools::HTML))
  
  #Base leaflet map 
  map <- reactive({
    leaflet()%>%
      addTiles() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addScaleBar(position = "bottomright")%>%
      setView(lng = 149, lat = -35.34, zoom = 10)
  })
  
  #Output leaflet 
  output$reptileLocation <- renderLeaflet({
    map()
  })
  
  #Output  leaflet, changes when user selects species
  observeEvent(input$selectSpecies, {
    proxy <- leafletProxy("reptileLocation") 
    
    
    if(!is.null(input$selectSpecies)){
      proxy %>%
        addCircleMarkers(data = reactiveReptiles(),
                         fillColor = "#F16648",
                         stroke = FALSE,
                         fillOpacity = .6,
                         radius = 8,
                         layerId = ~id,
                         label = mapText(),
                         group = "speciesPoints") %>%
        flyToBounds(lng1 = min(reactiveReptiles()$decimalLongitude)- 0.3,
                    lng2 = max(reactiveReptiles()$decimalLongitude),
                    lat1 = min(reactiveReptiles()$decimalLatitude),
                    lat2 = max(reactiveReptiles()$decimalLatitude))
    }else{
      leafletProxy("reptileLocation")%>%
        clearMarkers()%>%
        clearShapes()%>%
        removeMarker(layerId = reactiveReptiles()$id)
    }
    
  }, ignoreNULL = FALSE)
  
  
  
  
  
  ##### Plotly ####
  #Forest
  reactiveForest <- reactive({
    req(input$selectSpecies)
    if(!is.null(input$selectSpecies)){
      reptiles %>%
        filter(scientificName %in% input$selectSpecies) %>%
        group_by(forest2013) %>%
        summarise(numOccurrence = n(), .groups = "drop")
    }else{
      return(NULL) #This helps to return a null plot when nothing is selected
    }
  })
  
  
  output$forest <- renderPlotly({
    reactiveForest() %>%
      highlight_key(key = ~forest2013)%>%
      plot_ly(x = ~forest2013, y = ~numOccurrence, type = 'bar',
            opacity = .6,
            marker = list(color = "#F16648"),
            hovertemplate = paste('%{x}', '<br>Occurrence: %{y}<br>','<extra></extra>')) %>%
      layout(yaxis = list(title = 'Number of occurrences'),
             xaxis = list(title='Type of forest in 2013'),
             barmode = "overlay") %>%
      highlight(on = "plotly_hover", off = "plotly_deselect", color = "#F16648")
    
  })
  
  #Resource name
  reactiveResource <- reactive({
    req(input$selectSpecies)
    if(!is.null(input$selectSpecies)){
      reptiles %>%
        filter(scientificName %in% input$selectSpecies) %>%
        group_by(dataResourceName) %>%
        summarise(numOccurrence = n(), .groups = "drop")
    }else{
      return(NULL) #This helps to return a null plot when nothing is selected
    }
  })
  
  
  output$resource <- renderPlotly({
    reactiveResource() %>%
      mutate(dataResourceName = str_trunc(dataResourceName, 25, "right"))%>%
      # mutate(dataResourceName = sapply(dataResourceName, 
      #                                  function(x) gsub(" ", "</br>", x))) %>%
      highlight_key(key = ~dataResourceName)%>%
      plot_ly(x = ~dataResourceName, y = ~numOccurrence, 
              type = 'bar',
              opacity = .6,
              marker = list(color = "#F16648"),
              hovertemplate = paste('%{x}', '<br>Occurrence: %{y}<br>','<extra></extra>')) %>%
      layout(yaxis = list(title = 'Number of occurrences'),
             xaxis = list(title='Data resource name'),
             barmode = "overlay") %>%
      highlight(on = "plotly_hover", off = "plotly_deselect", color = "#F16648")
    
  })
  
  #Basis of record
  reactiveRecord <- reactive({
    req(input$selectSpecies)
    if(!is.null(input$selectSpecies)){
      reptiles %>%
        filter(scientificName %in% input$selectSpecies) %>%
        group_by(basisOfRecord) %>%
        summarise(numOccurrence = n(), .groups = "drop")
    }else{
      return(NULL) #This helps to return a null plot when nothing is selected
    }
  })
  
  
  output$record <- renderPlotly({
    reactiveRecord() %>%
      highlight_key(key = ~basisOfRecord)%>%
      plot_ly(x = ~basisOfRecord, y = ~numOccurrence, type = 'bar',
              opacity = .6,
              marker = list(color = "#F16648"),
              hovertemplate = paste('%{x}', '<br>Occurrence: %{y}<br>','<extra></extra>')) %>%
      layout(yaxis = list(title = 'Number of occurrences'),
             xaxis = list(title='Basis of record'),
             barmode = "overlay") %>%
      highlight(on = "plotly_hover", off = "plotly_deselect", color = "#F16648")
    
  })
  
  #Monthly records
  reactiveMonth <- reactive({
    req(input$selectSpecies)
    if(!is.null(input$selectSpecies)){
      reptiles %>%
        filter(scientificName %in% input$selectSpecies) %>%
        group_by(month) %>%
        summarise(numOccurrence = n(), .groups = "drop")
    }else{
      return(NULL) #This helps to return a null plot when nothing is selected
    }
  })
  
  
  output$month <- renderPlotly({
    suppressWarnings(
      reactiveMonth() %>%
      highlight_key(key = ~month)%>%
      plot_ly(x = ~month, y = ~numOccurrence, type = 'bar',
              opacity = .6,
              marker = list(color = "#F16648"),
              hovertemplate = paste('%{x}', '<br>Occurrence: %{y}<br>','<extra></extra>')) %>%
      layout(yaxis = list(title = 'Number of occurrences'),
             xaxis = list(title='Month'),
             barmode = "overlay") %>%
      highlight(on = "plotly_hover", off = "plotly_deselect", color = "#F16648")
      )
    
  })
  
  ##### Text output #####
  
  output$numSpecies <- renderText({
    paste0(prettyNum(length(input$selectSpecies)), " species selected")
  })
  
  output$occurrences <- renderText({
    paste0(prettyNum(nrow(reactiveReptiles())), " occurrences")
  })
}

shinyApp(ui,server)
library(shiny)
library(leaflet)
library(RColorBrewer)

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                sliderInput("range", "Magnitudes", min(quakes$mag), max(quakes$mag),
                            value = range(quakes$mag), step = 0.1
                ),
                selectInput("colors", "Color Scheme",
                            rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                ),
                checkboxInput("legend", "Show legend", TRUE)
  )
)

server <- function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    quakes[quakes$mag >= input$range[1] & quakes$mag <= input$range[2],]
  })
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
    colorNumeric(input$colors, quakes$mag)
  })
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(quakes) %>% addTiles() %>%
      fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  df.20 <- quakes[1:20,]
  getColor <- function(quakes) {
    sapply(quakes$mag, function(mag) {
      if(mag <= 4) {
        "green"
      } else if(mag <= 5) {
        "orange"
      } else {
        "red"
      } })
  }
  
  gettext <- function(quakes) {
    sapply(quakes$mag, function(mag) {
      if(mag <= 4) {
        "1"
      } else if(mag <= 5) {
        "2"
      } else {
        "3"
      } })
  }
  
  icons <- awesomeIcons(
    icon = 'ios-close',
    iconColor = 'black',
    library = 'ion',
    fontFamily = "system-ui",
    text = gettext(df.20),
    markerColor = getColor(df.20)
  )
  
  
  
  observe({
    pal <- colorpal()
    
    
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addTiles() %>%
      addAwesomeMarkers(~long, ~lat, icon=icons, label=~as.character(mag))
  })
  
  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map", data = quakes)
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = ~mag
      )
    }
  })
}

shinyApp(ui, server)


##############mine example###########

shinyApp(
    ui <-bootstrapPage(
                    
                    mainPanel(
                             div(class="outer",
                                 tags$style(
                                   # Include our custom CSS
                                   includeCSS("styles.css"),
                                   includeScript("gomap.js")
                                 ),
                                 
                                 # If not using custom CSS, set height of leafletOutput to a number instead of percent
                                 leafletOutput("map", width="100%", height="100%"),
                                 
                                 # Shiny versions prior to 0.11 should use class = "modal" instead.
                                 absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                               draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                               width = 330, height = "auto",
                                               
                                               h2("ZIP explorer"),
                                               
                                               selectInput("color", "Color", vars),
                                               selectInput("size", "Size", vars, selected = "adultpop"),
                                               conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
                                                                # Only prompt for threshold when coloring or sizing by superzip
                                                                numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
                                               )
                                 ),
                                 
                                 tags$div(id="cite",
                                          'Data compiled for ', tags$em('Coming Apart: The State of White America, 1960–2010'), ' by Charles Murray (Crown Forum, 2012).'
                                 )
                             )
                    ),

                    conditionalPanel("false", icon("crosshair"))
    ),
    

server <- function(input, output, session) {
#create map
output$map <- renderLeaflet({
  leaflet() %>%
    addTiles(
      urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
      attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
    ) %>%
    setView(lng = -78.8986, lat = 35.9940, zoom = 11) #-78.8986 35.9940
})

# A reactive expression that returns the set of zips that are
# in bounds right now
zipsInBounds <- reactive({
  if (is.null(input$map_bounds))
    return(zipdata[FALSE,])
  bounds <- input$map_bounds
  latRng <- range(bounds$north, bounds$south)
  lngRng <- range(bounds$east, bounds$west)
  
  subset(zipdata,
         latitude >= latRng[1] & latitude <= latRng[2] &
           longitude >= lngRng[1] & longitude <= lngRng[2])
})

# This observer is responsible for maintaining the circles and legend,
# according to the variables the user has chosen to map to color and size.
observe({
  colorBy <- input$color
  sizeBy <- input$size
  
  if (colorBy == "superzip") {
    # Color and palette are treated specially in the "superzip" case, because
    # the values are categorical instead of continuous.
    colorData <- ifelse(zipdata$centile >= (100 - input$threshold), "yes", "no")
    pal <- colorFactor("viridis", colorData)
  } else {
    colorData <- zipdata[[colorBy]]
    pal <- colorBin("viridis", colorData, 7, pretty = FALSE)
  }
  
  if (sizeBy == "superzip") {
    # Radius is treated specially in the "superzip" case.
    radius <- ifelse(zipdata$centile >= (100 - input$threshold), 30000, 3000)
  } else {
    radius <- zipdata[[sizeBy]] / max(zipdata[[sizeBy]]) * 30000
  }
  
  leafletProxy("map", data = zipdata) %>%
    clearShapes() %>%
    addCircles(~longitude, ~latitude, radius=radius, layerId=~zipcode,
               stroke=FALSE, fillOpacity=0.4, fillColor=pal(colorData)) %>%
    addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
              layerId="colorLegend")
})





# Show a popup at the given location
showZipcodePopup <- function(zipcode, lat, lng) {
  selectedZip <- allzips[allzips$zipcode == zipcode,]
  content <- as.character(tagList(
    tags$h4("Score:", as.integer(selectedZip$centile)),
    tags$strong(HTML(sprintf("%s, %s %s",
                             selectedZip$city.x, selectedZip$state.x, selectedZip$zipcode
    ))), tags$br(),
    sprintf("Median household income: %s", dollar(selectedZip$income * 1000)), tags$br(),
    sprintf("Percent of adults with BA: %s%%", as.integer(selectedZip$college)), tags$br(),
    sprintf("Adult population: %s", selectedZip$adultpop)
  ))
  leafletProxy("map") %>% addPopups(lng, lat, content, layerId = zipcode)
}



observe({
  leafletProxy("map") %>% clearPopups()
  event <- input$map_shape_click
  if (is.null(event))
    return()
  
  isolate({
    showZipcodePopup(event$id, event$lat, event$lng)
  })
})


observe({
  if (is.null(input$goto))
    return()
  isolate({
    map <- leafletProxy("map")
    map %>% clearPopups()
    dist <- 0.5
    zip <- input$goto$zip
    lat <- input$goto$lat
    lng <- input$goto$lng
    showZipcodePopup(zip, lat, lng)
    map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
  })
})
}
)

#########################color example#########################
icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  fontFamily = "system-ui",
  text = gettext(),
  markerColor = sample(hi, 10)
)


hi = c('red', 'darkred', 'lightred', 'orange', 'beige', 'green', 'darkgreen', 'lightgreen', 'blue', 'darkblue', 'lightblue', 'purple', 'darkpurple', 'pink', 'cadetblue', 'white', 'gray', 'lightgray', 'black')


leaflet(data = df.20) %>% clearShapes() %>% addTiles() %>%
  addAwesomeMarkers(
    ~long, ~lat, icon=icons)

##################pop up image###########################
https://groups.google.com/forum/#!topic/shiny-discuss/1V4idLSjaBs

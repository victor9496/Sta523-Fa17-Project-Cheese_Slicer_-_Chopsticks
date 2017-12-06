library(shiny)
library(leaflet)
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
                        draggable = TRUE, top = 3, left = "auto", right = 20, bottom = "auto",
                        width = 330, height = "auto",
                        
                        h4("Top5-Top15"),
                        sliderInput("top", label=NULL,min = 5, max = 15,value=5, step=1),
                        h4("Price Range"),
                        radioButtons("price",label="Below:",choices= c("500", "700", "1000")),
                        h4("Distance"),
                        sliderInput("distance", label="Below", min=1, max=10, value=5, step=0.1),
                        h4("Floor_plan"),
                        selectInput("var", "Below", choices = unique(df.complete$plan), 
                                    selected = "1 Bedrooms, 1 Bathroom"),
                        h4("Uncertianty"),
                        sliderInput("uncertainty",label=NULL, min=0.1, max=0.75, 
                                    value=0.25, step=0.1)
                        
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
          # urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
          # attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
        ) %>%
        setView(lng = -78.8986, lat = 35.9940, zoom = 12) #-78.8986 35.9940
    })
    
    new_df = reactive({
      df.complete %>%
      dplyr::filter(plan == input$var) %>%
      dplyr::arrange(desc(avg_review)) %>%
      head(n = input$top)})

    
    
    zipsInBounds <- reactive({
      if (is.null(input$map_bounds))
        return(zipdata[FALSE,])
      bounds <- input$map_bounds
      latRng <- range(bounds$north, bounds$south)
      lngRng <- range(bounds$east, bounds$west)

      subset(new_df(),
             lat >= latRng[1] & lat <= latRng[2] &
               lon >= lngRng[1] & lon <= lngRng[2])
    })


    observe({
      col_var = c('red', 'darkred', 'lightred', 'orange', 'beige', 'green',
                  'lightgreen', 'blue',  'lightblue', 'purple',  'pink',
                  'cadetblue', 'white', 'gray', 'lightgray')


      icons <- awesomeIcons(
        icon = 'ios-close',
        iconColor = 'black',
        library = 'ion',
        fontFamily = "system-ui",
        text = 1:input$top,
        markerColor = sample(col_var)
      )

      leafletProxy("map", data = new_df()) %>%
        clearShapes() %>%
        addAwesomeMarkers(
          ~lon, ~lat, icon=icons)
    })
    
   
    #  showZipcodePopup <- function(plan, lat, lng) {
    #   selected <- df.complete()[df.complete()$plan == plan,]
    #   content <- as.character(tagList(
    #     tags$h4("Score:", as.integer(selected$avg_review)),
    #     # tags$strong(HTML(sprintf("%s, %s %s",
    #     #                          selectedZip$city.x, selectedZip$state.x, selectedZip$zipcode
    #     # ))), tags$br(),
    #     # sprintf("Median household income: %s", dollar(selectedZip$income * 1000)), tags$br(),
    #     # sprintf("Percent of adults with BA: %s%%", as.integer(selectedZip$college)), tags$br(),
    #     # sprintf("Adult population: %s", selectedZip$adultpop), tags$br(),
    #     tags$a(href = "http://www.baidu.com", 
    #            "baidu", target="_blank"), tags$br(),
    #     tags$img(src = "https://static1.squarespace.com/static/51156277e4b0b8b2ffe11c00/t/583ccafcbebafbc5c11fa6ec/1480379239088/RStudio-Ball.png", width = "200px", height = "200px")
    #   ))
    #   leafletProxy("map") %>% addPopups(lng, lat, content, layerId = plan)
    # }
    #  
    #  observe({
    #    leafletProxy("map") %>% clearPopups()
    #    event <- input$map_shape_click
    #    if (is.null(event))
    #      return()
    #    
    #    isolate({
    #      showZipcodePopup(event$id, event$lat, event$lng)
    #    })
    #  })
    #  
    #  observe({
    #    if (is.null(input$goto))
    #      return()
    #    isolate({
    #      map <- leafletProxy("map")
    #      map %>% clearPopups()
    #      dist <- 0.5
    #      zip <- input$goto$zip
    #      lat <- input$goto$lat
    #      lng <- input$goto$lng
    #      showZipcodePopup(zip, lat, lng)
    #      map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    #    })
    #  })
     
  }
)

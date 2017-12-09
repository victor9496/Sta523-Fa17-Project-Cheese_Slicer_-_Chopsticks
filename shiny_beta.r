library(shiny)
library(leaflet)

#for demonstrate 
# purl = urls[1:125]
#old df.complete with 125 rows
# df.complete = cbind(df.complete, purl)
load("data_shiny.Rdata")

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
                   'Data compiled for ', tags$em('Coming Apart: Copyright © 2017 Apartmentratings.com')
          )
      )
    ),
    
    conditionalPanel("false", icon("crosshair"))
  ),
  
  
  server <- function(input, output, session) {
    #create map
    
    # output$map <- renderLeaflet({
    #   leaflet() %>%
    #     addTiles() %>%
    #     setView(lng = -78.8986, lat = 35.9940, zoom = 12) #-78.8986 35.9940
    # })
    
    
   
    #  small_df = reactive({
    #   if (is.null(input$map_bounds))
    #     return(df.complete[FALSE,])
    #   bounds <- input$map_bounds
    #   latRng <- range(bounds$north, bounds$south)
    #   lngRng <- range(bounds$east, bounds$west)
    #   
    #   subset(new_df(),
    #          lat >= latRng[1] & lat <= latRng[2] &
    #            lon >= lngRng[1] & lon <= lngRng[2])
    # })

    observe({
      
      new_df = reactive({
        df = get(load(paste0("classprb",gsub(" ","",input$var),".Rdata")))
        rm(classprb)
        samps = sapply(df,function(x) apply(x,2,function(i) quantile(i,input$uncertainty)))
        weighted_mean = apply(samps,1,function(x) weighted.mean(0:5,x))
        val = sort(weighted_mean,decreasing = TRUE)[1:input$top]
        rank_df = data.frame(name = names(val),val)
        merge(rank_df,df.complete,by = "name")
          #change need!
          # dplyr::arrange(desc(avg_review)) %>%
        })
      
      col_var = c('red', 'white', 'lightblue', 'orange', 'beige', 'green',
                  'lightgreen', 'blue',  'lightred', 'purple',  'pink',
                  'cadetblue',  'darkred','gray', 'lightgray')


      icons <- awesomeIcons(
        icon = 'ios-close',
        iconColor = 'black',
        library = 'ion',
        fontFamily = "system-ui",
        text = 1:input$top,
        markerColor = col_var[1:input$top]
      )

      
      content <- paste0(
                       "<b><a href=",new_df()$purl,">",new_df()$name,"</a></b><br/>",
                       "Floor_plan: ",new_df()$plan,"<br/>",
                       "Rent: ",round(new_df()$rent),"<br/>",
                       "<img src=", new_df()$image, " height = '200', width = '200'>")
    
    
      
      output$map <- renderLeaflet({
        leaflet() %>%
          addTiles() %>%
        clearShapes() %>%
        addAwesomeMarkers(
          new_df()$lon, new_df()$lat, icon=icons, 
          popup = content)
    })
    
    })
  }
)
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
  

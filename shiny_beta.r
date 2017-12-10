library(shiny)
library(leaflet)
library(dplyr)


load("df_complete.Rdata")

#change column name for easier future use
colnames(df.complete)[grepl("url",colnames(df.complete))] = "purl"

#if image is NA, change to another link instead
df.complete$image = df.complete$image %>%ifelse(is.na(.),"https://www.internetbrands.com/wp-content/uploads/2014/05/hometravel_aptrating_2x.png"
                                                ,.)
#calculating how many people can accomodate in each floor plan
#based on numbers of bedrooms
per_room = df.complete$plan %>% 
  gsub("Studio", "1 Bedrooms", .) %>% 
  str_extract_all("\\d Bedrooms") %>% 
  unlist() %>% 
  str_extract_all("\\d") %>% 
  unlist() %>% 
  as.numeric()

#calculating unit rent by dividing total rent by number of people
df.complete$rent = df.complete$rent / per_room

#change distance unit from m to km
df.complete$distance = df.complete$distance/ 1000

#create vector variable contains info for each floor plan
plan_list = list.files(path = ".", pattern = "class.*\\.Rdata", all.files = FALSE,
                       full.names = FALSE, recursive = FALSE,
                       ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE) %>% 
  str_extract( "\\dBedrooms,\\dBathroom(s)?|Studio,\\dBathroom")

#create variable for future use, shiny initiation
first = 0

#https://github.com/rstudio/shiny-examples/tree/master/063-superzip-example
#Many thanks to this superzip example to help us construct the barebones of our shiny app
shinyApp(
  ui <-bootstrapPage(
    
    mainPanel(
      div(class="outer",
          tags$style(
            # Include our custom CSS
            includeCSS("styles.css"),
            includeScript("gomap.js")
          ),
          
         
          leafletOutput("map", width="100%", height="100%"),
          
          #set up of the panel
          absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                        draggable = TRUE, top = 3, left = "auto", right = 20, bottom = "auto",
                        width = 330, height = "auto",
           #add logo of duke and center             
          tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/e/e6/Duke_University_logo.svg/1280px-Duke_University_logo.svg.png", 
                   width = "250px", height = "100px",
                   style="display: block; margin-left: auto; margin-right: auto;"),
                        
          h1("Apt Rankings", 
             style = "font-family: 'Lobster', cursive;
             font-weight: 500; line-height: 1.1;", align = "center" 
          ),
                        h4("Top5-Top15"),
                        radioButtons("top",NULL, 
                                     choices = c("5" ,"10","15"),
                                     selected = "5"),
                        h4("Price Range"),
                        selectInput("price", "Below", 
                                    choices = c(550,600, 650, 700, 800,
                                                "Above 800"), selected = "700"),
                        h4("Distance"),
                        selectInput("distance", label="Below", 
                                    choices = c(2*1:4, "Above 10"), selected = "8"),
                        h4("Floor_plan"),
                        selectInput("var", "Below", choices = plan_list, 
                                    selected = "1 Bedrooms, 1 Bathroom"),
                        h4("Uncertianty"),
                        sliderInput("uncertainty",label=NULL, min=0.1, max=0.75, 
                                    value=0.25, step=0.1)
                        
          ),
          #credit for apartmentratings.com for allow us to use their data
          tags$div(id="cite",
                   'Data compiled for ', tags$em('Coming Apart: Copyright © 2017 Apartmentratings.com')
          )
      )
    ),
    
    conditionalPanel("false", icon("crosshair"))
  ),
  
  
  server <- function(input, output, session) {
    
    #create map
    output$map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        setView(lng = -78.8986, lat = 35.9940, zoom = 13) #-78.8986 35.9940 center location of Durham
    })
    
    #create observe for each change in input update markers below
    observe({
      
      new_df = reactive({
        #below code for test purpose only
        # input = data.frame(var="1Bedrooms,1Bathroom",uncertainty = 0.3,price = 900,distance=10000,top = 5)
        price = input$price
        dist = input$distance
       #specify the ending value
        if(dist == "Above 10") dist = 30
        if(price == "Above 800") price = 2000
       
        #change floor_plan notation
        floor_plan = gsub("(\\d)(\\w)","\\1 \\2",input$var) %>% 
          gsub(",", ", ", .)
        
        #create or filter large dataframe
        df = get(load(paste0("classprb",input$var,".Rdata")))
        rm(classprb)
        samps = sapply(df,function(x) apply(x,2,function(i) quantile(i,input$uncertainty)))
        weighted_mean = apply(samps,1,function(x) weighted.mean(0:5,x))
        weighted_mean = data.frame(name = names(weighted_mean),val = weighted_mean,plan = floor_plan)
        return_df = merge(weighted_mean,df.complete,by = c("name","plan"))
        return_df = return_df%>%
          filter(!duplicated(name))%>%
          filter(rent< as.numeric(price))%>%
          filter(distance< as.numeric(dist))%>%
          arrange(desc(val))%>%
          slice(1:as.numeric(input$top))
     
        return_df
      })
      
      #filter a even small dataframe baseon the movement of map
      #special thanks to superzip expamle again
      small_df = reactive({
        if (is.null(input$map_bounds))
          return(new_df()[FALSE,])
        bounds <- input$map_bounds
        latRng <- range(bounds$north, bounds$south)
        lngRng <- range(bounds$east, bounds$west)
        
        subset(new_df(),
               lat >= latRng[1] & lat <= latRng[2] &
                 lon >= lngRng[1] & lon <= lngRng[2])
        
      })
      #print(nrow(small_df()))(test only)
    
    #if the above dataframe have zero row(empty dataframe) should give warning to user
    #could be 1.harsh input 2.no observation within the map they are looking at
    if(nrow(small_df()) == 0) {
      #since everytime we open the map, it gives us the warning, we assume by default the dataframe above
      #will have zero row no matter what, so we decide one more condition, if it is the second time have
      #zero row, we then have the warning instead
      if(first ==0){
       #update global value, so it is not 0 instead  
        first<<-1
        }else{
         #show model dialog with the warning message 
         showModal(modalDialog(
            title = HTML('<center><font color="red">Warning: No results found for this input</font></center>'),
            HTML("<center><img src=https://i.imgur.com/nmpYQx2.jpg height = '400', width = '300'></center>"),
            easyClose = TRUE,footer = NULL
          ))}
      }
      
      
      col_var = c('red', 'white', 'lightblue', 'orange', 'green', 'beige', 
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
        "<b><a href=",small_df()$purl,' target="_blank">',small_df()$name,"</a></b><br/>",
        "Floor_plan: ",small_df()$plan,"<br/>",
        "Rent: ",round(small_df()$rent),"<br/>",
        "<img src=", small_df()$image, " height = '200', width = '200'>")
      
      
      
      leafletProxy("map") %>%
        clearMarkers() %>%
        addAwesomeMarkers(
          small_df()$lon, small_df()$lat, icon=icons,
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

library(shiny)
library(leaflet)
library(dplyr)
library(stringr)


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
    
    #create custom color for markers below(only allowed color can not use rcolorbrewer 
    col_var = c('red', 'white', 'lightblue', 'orange', 'green', 'beige', 
                'lightgreen', 'blue',  'lightred', 'purple',  'pink',
                'cadetblue',  'darkred','gray', 'lightgray')
    
    #create markers with different colors and rankings(number)
    icons <- awesomeIcons(
      icon = 'ios-close',
      iconColor = 'black',
      library = 'ion',
      fontFamily = "system-ui",
      text = 1:input$top,
      markerColor = col_var[1:input$top]
    )
    
    #info with apartment name, floor plan and rent as well as image on the pop up of the marker when clicked
    content <- paste0(
      "<b><a href=",small_df()$purl,' target="_blank">',small_df()$name,"</a></b><br/>",
      "Floor_plan: ",small_df()$plan,"<br/>",
      "Rent: ",round(small_df()$rent),"<br/>",
      "<img src=", small_df()$image, " height = '200', width = '200'>")
    
    
    #update the markers each time when we change input or move/zoom the map
    leafletProxy("map") %>%
      clearMarkers() %>%
      addAwesomeMarkers(
        small_df()$lon, small_df()$lat, icon=icons,
        popup = content)
  })
  
}

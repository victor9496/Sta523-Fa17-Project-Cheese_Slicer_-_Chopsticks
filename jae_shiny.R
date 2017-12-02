
# Filter Function
filter_function = function(data,top,price,distance){
  ref_price = price
  ref_dist = distance
  ranking = top
  data = data %>% filter(price<ref_price,distance<ref_dist) %>% arrange(desc(score)) %>% top_n(ranking)
  return(data)
}



library(shiny)
#Shiny App
shinyApp(
  ui =  fluidPage(
    titlePanel("Silder"),
    sidebarLayout(
      sidebarPanel(
        h4("Top5-Top15"),
        sliderInput("top", label=NULL,min = 5, max = 15,value=5, step=5),
        h4("Price Range"),
        radioButtons("price",label="Below:",choices= c("500", "700", "1000")),
        h4("Distance"),
        sliderInput("distance", label="Below", min=1, max=10, value=5, step=0.1),
        h4("Uncertianty"),
        #sliderInput("uncertainty",label=NULL, min,max,)
        actionButton("search", label = "Search")
      ),
      mainPanel(uiOutput("apts"))
    )
  ),
  server = function(input, output, session)
  {
    #state = reactiveValues(observers = list())
    observeEvent(input$search,{
      #data = function_name()
      ref_price = as.numeric(input$price)
      ref_dist  = input$distance
      ranking = input$top
      data = apartment_finder(site)
      data %>% filter(price<ref_price,distance<ref_dist) %>% arrange(desc(score)) %>% top_n(ranking)
      n = NROW(na.omit(body))
      
      ## depending on what information you want to demonstrate on the main panel, just need to follow the code from midterm
      
      
      
    }
    )
  }
)


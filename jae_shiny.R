filter_function = function(data,top,price,distance){
  data %>% arrange(desc())
}



library(shiny)

shinyApp(
  ui =  fluidPage(
    titlePanel("Silder"),
    sidebarLayout(
      sidebarPanel(
        h4("Top5-Top15"),
        sliderInput("top", label=NULL,min = 5, max = 15,value=5, step=5),
        h4("Price Range"),
        checkboxGroupInput("price",label="Below:",choices= c("500", "700", "1000")),
        h4("Distance"),
        sliderInput("distance", label="Below", min=1, max=10, value=5, step=0.1),
        h4("Uncertianty"),
        #sliderInput("uncertainty",label=NULL, min,max,)
        actionButton("search", label = "Search"),
      )
    ),
    mainPanel(
      uiOutput("apts"))
    ),
  server = function(input, output, session)
  {
    #state = reactiveValues(observers = list())
    observeEvent(input$search,{
      #data = function_name()
      price = as.numeric(input$price)
      
      }
  )

library(shiny)
library(leaflet)
library(dplyr)
library(stringr)

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
)
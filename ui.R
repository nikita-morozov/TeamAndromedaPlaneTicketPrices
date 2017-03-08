library(shiny)
library(plotly)

ui <- fluidPage(
  titlePanel("Airline Ticket Price Trends"),

  sidebarLayout(
    sidebarPanel(
      sliderInput('sample.number', label = "How many samples?",
                  min = 10, max = 50, value = 50),
      dateInput("depart.data", label = "Departure Date", value = "2017-03-20",max = "2018-03-08", min = "2017-03-15", startview = "month", weekstart = 0),
      checkboxInput('add_lines', "Add Line of Best Fit")
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs", 
        tabPanel("Minimum Price Per Destination", plotOutput("world.map",width = "100%",height = "400px"), textOutput("timeline.comment")),
        tabPanel("Best Time to Buy", plotlyOutput("best.date.to.buy"), textOutput("summary.petal")),
        tabPanel("Should I buy now?", textOutput("buy.now"), tableOutput('table'))
      )  
    )
  )
)

shinyUI(ui)

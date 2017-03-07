library(shiny)
library(plotly)

ui <- fluidPage(
  titlePanel("Airline Ticket Price Trends"),

  sidebarLayout(
    sidebarPanel(
      #Sys.date()
      sliderInput('sample.number', label = "How many samples?",
                  min = 10, max = 50, value = 50),
      dateInput("depart.data", label = "Departure Date", value = NULL, min = NULL, startview = "month", weekstart = 0),
      checkboxInput('add_lines', "Add Line of Best Fit")
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs", 
        tabPanel("Timeline and Trends", plotlyOutput("timeline"), textOutput("timeline.comment")),
        tabPanel("Best Time to Buy", plotlyOutput("best.date.to.buy"), textOutput("summary.petal")),
        tabPanel("Should I buy now?", textOutput("buy.now"), tableOutput('table'))
      )  
    )
  )
)

shinyUI(ui)

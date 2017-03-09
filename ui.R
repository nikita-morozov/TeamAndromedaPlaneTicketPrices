library(shiny)
library(plotly)
library("xml2")

countriesList <- read.csv("./data/geo.csv", stringsAsFactors = FALSE)


ui <- fluidPage(
  titlePanel("Airline Ticket Price Trends"),

  sidebarLayout(
    sidebarPanel(
      #sliderInput('sample.number', label = "How many samples?",min = 10, max = 50, value = 50),
      dateInput("depart.data", label = "Departure Date", value = "2017-03-20",max = "2018-03-08", min = "2017-03-15", startview = "month", weekstart = 0),
      #checkboxInput('add_lines', "Add Line of Best Fit"),
      selectInput('select.country', "Select Departure Country", choices = countriesList$Name, selected = 'United States')
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs", 
        tabPanel("World Map", plotOutput("world.map",width = "100%",height = "400px"), textOutput("timeline.comment")),
        tabPanel("Where Should I Go?", dataTableOutput('table')),
        tabPanel("Tab3", textOutput("buy.now"))
      )  
    )
  )
)

shinyUI(ui)

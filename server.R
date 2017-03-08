library(shiny)
library("dplyr")
library("jsonlite")
library("knitr")
library("httr")
library("ggplot2")
library("XML")

server <- function(input, output, session) {

  
  #Variables:
  country = "US"
  currency = "USD"
  locale = "en-us"
  originplace="US"
  destinationplace="Anywhere"
  outbounddate="anytime"
  #inbounddate="2017-12"
  apikey="te892026803091243844897141219716"
  
  ####################
  ####### JSON #######
  ####################
  
  routes.url <- paste0("http://partners.api.skyscanner.net/apiservices/browsequotes/v1.0/",country,"/",currency,"/",locale,"/",originplace,"/",destinationplace,"/", outbounddate,"?apiKey=", apikey)
  
  #Static: response <- GET("http://partners.api.skyscanner.net/apiservices/browseroutes/v1.0/GB/GBP/en-GB/US/FR/2017-11/2017-12?apiKey=te892026803091243844897141219716")
  
  response <- GET(routes.url)
  query <- fromJSON(content(response, "text"))
  
  places <- flatten(query$Places)
  routes <- flatten(query$Routes)
  quotes <- flatten(query$Quotes)
  carriers <- flatten(query$Carriers)
  currencies <-flatten(query$Currencies)
}

shinyServer(server)

library("shiny")
library("dplyr")
library("jsonlite")
library("knitr")
library("httr")
library("ggplot2")
library("XML")
library("scales")
library("maps")

options(shiny.maxRequestSize=980*1024^2)
options(scipen = 15000)

memoryVector <- c("2","4","6") #global vector acting as a memory bank

server <- function(input, output, session) {
   
   memoryVector <<- c("x","y","z") #local vector acting as a memory bank during each session
   
   
   observe({ #Reactive environment for changing selectInput fields dynamically
      
      countriesList <- read.csv("./data/geo.csv", stringsAsFactors = FALSE) #Grab a list of all the countries in the world
      
      #Converts destination and origin into Skyscanner and ISO2 codes
      translatedoriginplace <- filter(countriesList, Name == input$select.country) %>% select(ID) 
      translatedDestPlace <- countriesList %>% rbind(c("Anywhere","Anywhere")) %>% filter(Name == input$select.dest) %>% select(ID)
      
      #Variables for this specific API call.
      country = "US"
      currency = "USD"
      locale = "en-us"
      originplace = translatedoriginplace[1,1]
      destinationplace = translatedDestPlace[1,1]
      outbounddate = input$depart.data
      #inbounddate="2017-12"
      apikey="te892026803091243844897141219716"
      
      #Retrieves Data
      routes.url <- paste0("http://partners.api.skyscanner.net/apiservices/browsequotes/v1.0/",country,"/",currency,"/",locale,"/",originplace,"/",destinationplace,"/", outbounddate,"?apiKey=", apikey)
      
      #Converts response into pliable data
      response <- GET(routes.url)
      query <- fromJSON(content(response, "text"))
      
      #Assigns different data frames
      places <- flatten(query$Places)
      quotes <- flatten(query$Quotes)
      
      #Filters the response locations by airports in the destination and origin countries
      places.stations.only <- filter(places, Type == "Station")
      from <- places.stations.only %>% filter(CountryName == input$select.country)
      to <- places.stations.only %>% filter(CountryName == input$select.dest)
      
      #Dynamic dropdowns activate only if the destination country is selected
      if (input$select.dest != "Anywhere" && isTRUE(memoryVector[1] != input$select.dest)){
         
         #Updates dropdowns to include relavant lists of airports.
         updateSelectInput(session, "depId", label = "Select Departure Airport:", choices = from$Name, selected = from$Name[1])
         updateSelectInput(session, "destId", label = "Select Destination Airport:", choices = to$Name, selected = to$Name[1])
         
         #Session memory remembers what the previous search query was so that it doesn't repeat querys if they are the same
         memoryVector <<- c(input$select.dest, input$depId, input$destId)
         
         #Converts the airport into a Skyscanner code for the next API call
         airportdep <- from %>% filter(Name == input$depId) %>% select(SkyscannerCode)
         airportdest <- to %>% filter(Name == input$destId) %>% select(SkyscannerCode)
         airportdepId <- airportdep[1,1]
         airportdestId <- airportdest[1,1]
         
         #Variables for this specific API call.
         country = "US"
         currency = "USD"
         locale = "en-us"
         originplace = airportdepId
         destinationplace = airportdestId
         outbounddate = input$depart.data
         #inbounddate="2017-12"
         apikey="te892026803091243844897141219716"
         
         #API call for a specific route
         routes.url <- paste0("http://partners.api.skyscanner.net/apiservices/browsequotes/v1.0/",country,"/",currency,"/",locale,"/",originplace,"/",destinationplace,"/", outbounddate,"?apiKey=", apikey)
         response <- GET(routes.url)
         query <- fromJSON(content(response, "text"))
         
         #Checks to make sure the query is valid otherwise returs user friendly message and blank screen
         if (is.data.frame(query$Quotes)){
            
            #Creates pliable data frame
            quotes <- flatten(query$Quotes)
            
            #Gets back names of origin and destination airports
            uniqueIDx <- quotes$OutboundLeg.OriginId
            uniqueIDy <- quotes$OutboundLeg.DestinationId
            xname <- places.stations.only %>% filter(PlaceId == uniqueIDx[1]) %>% select(Name)
            yname <- places.stations.only %>% filter(PlaceId == uniqueIDy[1]) %>% select(Name)
            
            #User friendly Route description message
            finalunique <- paste0("Route: ",xname, " -> ",yname) 
            output$message <- renderText({
               finalunique
            })
         }else{ #Asks user to ensure that the airports are correctly chosen before doing an API request.
            output$message <- renderText({
               paste0("Please confirm Airports and refresh.")
            })
         } 
         #Ensures that the airports have changed but not the destination and origin countries.
      } else if(input$select.dest != "Anywhere" && isTRUE(memoryVector[1] == input$select.dest)){
         
         #local session memory
         memoryVector <<- c(input$select.dest, input$depId, input$destId)
         
         #Converts the airport into a Skyscanner code for the next API call
         airportdep <- from %>% filter(Name == input$depId) %>% select(SkyscannerCode)
         airportdest <- to %>% filter(Name == input$destId) %>% select(SkyscannerCode)
         airportdepId <- airportdep[1,1]
         airportdestId <- airportdest[1,1]
         
         #Variables for this specific API call.
         country = "US"
         currency = "USD"
         locale = "en-us"
         originplace = airportdepId
         destinationplace = airportdestId
         outbounddate = input$depart.data
         apikey="te892026803091243844897141219716"
         
         #Api call
         routes.url <- paste0("http://partners.api.skyscanner.net/apiservices/browsequotes/v1.0/",country,"/",currency,"/",locale,"/",originplace,"/",destinationplace,"/", outbounddate,"?apiKey=", apikey)
         
         #Store API responses
         response <- GET(routes.url)
         query <- fromJSON(content(response, "text"))
         
         #Esnure that API response is a valid data frame
         if (is.data.frame(query$Quotes)){
            quotes <- flatten(query$Quotes)
            
            #Creates and renders custom route message (airports -> airport)
            uniqueIDx <- quotes$OutboundLeg.OriginId
            uniqueIDy <- quotes$OutboundLeg.DestinationId
            xname <- places.stations.only %>% filter(PlaceId == uniqueIDx[1]) %>% select(Name)
            yname <- places.stations.only %>% filter(PlaceId == uniqueIDy[1]) %>% select(Name)
            finalunique <- paste0("Route: ",xname, " -> ",yname) 
            output$message <- renderText({
               finalunique
            })
         }else{ #If the data.frams isn't viable then there were no routes with the selected airports.
            output$message <- renderText({
               paste0("Quotes is missing for Route ", input$depId, " -> ", input$destId,". Please select a new set of airports and try again.")
            })
         } 
         
      } else { #The Destination country has not been selected. Overwrites dropdowns to have no available airports and displayes user friendly message.
         updateSelectInput(session, "depId", label = "Please Select Destination Country", choices =  c("None"), selected = "None")
         updateSelectInput(session, "destId", label = "Please Select Destination Country", choices =  c("None"), selected = "None")
         output$message <- renderText({"Unavailable: Please Select Destination Country"})
      }
   })
   
   #Generates table on the "Where should I go tab"
   getTable <- function(input,output){
      #Save a list of possible countries.
      countriesList <- read.csv("./data/geo.csv", stringsAsFactors = FALSE)
      
      #Generate progess bar with messages.
      progress <- shiny::Progress$new()
      
      #Progress message 1
      progress$set(message = "Computing Data", value = 0)
      
      #Converts user selected countries into applicable codes for data frames
      translatedoriginplace <- filter(countriesList, Name == input$select.country) %>% select(ID)
      translatedDestPlace <- countriesList %>% rbind(c("Anywhere","Anywhere")) %>% filter(Name == input$select.dest) %>% select(ID)
      
      #Variables:
      country = "US"
      currency = "USD"
      locale = "en-us"
      originplace = translatedoriginplace[1,1]
      destinationplace = translatedDestPlace[1,1]
      outbounddate = input$depart.data
      apikey="te892026803091243844897141219716"
      
      #Progress message 2
      progress$set(message = "Routing API", value = 1)
      
      #API Call with new query
      routes.url <- paste0("http://partners.api.skyscanner.net/apiservices/browsequotes/v1.0/",country,"/",currency,"/",locale,"/",originplace,"/",destinationplace,"/", outbounddate,"?apiKey=", apikey)
      response <- GET(routes.url)
      query <- fromJSON(content(response, "text"))
      
      #Converts response into pliable data frames
      places <- flatten(query$Places)
      quotes <- flatten(query$Quotes) #%>% dplyr::select(2,7,8)
      carriers <- flatten(query$Carriers)
      
      #Resolved which two countries are in question and which airports are in which country.
      places.countries.only <- filter(places, Type == "Country") %>% dplyr::select(2,4)
      places.stations.only <- filter(places, Type == "Station") %>% dplyr::select(1,6,8) %>% 
         left_join(quotes, by = c("PlaceId" = "OutboundLeg.DestinationId"))
      #Gives a list of flights by cheapest price and merges it with stations
      places.min.price <- summarise(group_by(places.stations.only, PlaceId),m = min(MinPrice))
      places.stations.only <- places.stations.only %>%  dplyr::select(1:3,5,6,8) %>% 
         unique() %>% left_join(places.min.price)
      
      #Arranges the cheap flight suggestions by price and selects only relavant data.
      places.stations.only <- arrange(places.stations.only, m) %>% filter(CountryName != input$select.country)  %>% select(2:5) %>% na.omit()
      
      #User friendly column names
      colnames(places.stations.only) <- c("City Name","Country Name","Price ($ Dollars)","Direct Flight")
      
      #Progress message 4
      progress$set(message = "Constructing Table", value = 4)
      on.exit(progress$close())
      
      #Generate table
      return(places.stations.only)
   }   
   
   #Call for the function above ^ Renders the table by returning it to the UI.
   output$table <- renderDataTable({getTable(input,output)})
   
   #Creates a plot of how route prices change over time.
   output$route.chart <- renderPlot({
      
      #Checks to make sure that the dropdowns have selected airports. If not - displays a blank graph.
      if(input$depId == "None" ){
         ggplot() +
            geom_blank(mapping = NULL, data = NULL, stat = "identity", position = "identity")
      } else {
         #Saves list of countries for reference
         countriesList <- read.csv("./data/geo.csv", stringsAsFactors = FALSE)
         
         #Converts airports into Skyscanner codes.
         translatedoriginplace <- filter(countriesList, Name == input$select.country) %>% select(ID)
         translatedDestPlace <- countriesList %>% rbind(c("Anywhere","Anywhere")) %>% filter(Name == input$select.dest) %>% select(ID)
         
         #Variables:
         country = "US"
         currency = "USD"
         locale = "en-us"
         originplace = translatedoriginplace[1,1]
         destinationplace = translatedDestPlace[1,1]
         outbounddate = "Anytime"
         apikey="te892026803091243844897141219716"
         
         #New Query that isnt limited to any date.
         routes.url <- paste0("http://partners.api.skyscanner.net/apiservices/browsequotes/v1.0/",country,"/",currency,"/",locale,"/",originplace,"/",destinationplace,"/", outbounddate,"?apiKey=", apikey)
         response <- GET(routes.url)
         query <- fromJSON(content(response, "text"))
         
         #Converts response into pliable data
         places <- flatten(query$Places)
         quotes <- flatten(query$Quotes)
         
         #Finds relavant data to the stations in question
         places.stations.only <- filter(places, Type == "Station")
         from <- places.stations.only %>% filter(CountryName == input$select.country)
         to <- places.stations.only %>% filter(CountryName == input$select.dest)
         
         #Selects airport ID for new Query
         airportdep <- from %>% filter(Name == input$depId) %>% select(SkyscannerCode)
         airportdest <- to %>% filter(Name == input$destId) %>% select(SkyscannerCode)
         airportdepId <- airportdep[1,1]
         airportdestId <- airportdest[1,1]
         
         #Variables:
         country = "US"
         currency = "USD"
         locale = "en-us"
         originplace = airportdepId
         destinationplace = airportdestId
         outbounddate = "Anytime"
         apikey="te892026803091243844897141219716"
         
         #API request for routes
         routes.url <- paste0("http://partners.api.skyscanner.net/apiservices/browsequotes/v1.0/",country,"/",currency,"/",locale,"/",originplace,"/",destinationplace,"/", outbounddate,"?apiKey=", apikey)
         response <- GET(routes.url)
         query <- fromJSON(content(response, "text"))
         
         #Ensures that the data frame is initialized.
         if (is.data.frame(query$Quotes)){
            quotes <- flatten(query$Quotes)
            
            ggplot(data = quotes, aes(x = OutboundLeg.DepartureDate, y = MinPrice)) +
               geom_point()+
               geom_smooth(method = lm)+
               ggtitle("Price Fluctuation over time")+ # for the main title
               xlab("Date - Time")+ # for the x axis label
               ylab("Price ($)")+ # for the y axis label
               theme(axis.text.x = element_text(angle = 90, hjust = 1))
            
         }else{ #Otherwise displays error and blank chart.
            output$message <- "Query Error: No routes exist for this set of airports. Please change your selections and try again."
         }
         
      }
   })
   
   output$world.map <- renderPlot({
      
      require("maps")
      
      countriesList <- read.csv("./data/geo.csv", stringsAsFactors = FALSE)
      
      progress <- shiny::Progress$new()
      progress$set(message = "Computing Data", value = 0)
      
      translatedoriginplace <- filter(countriesList, Name == input$select.country) %>% select(ID)
      translatedDestPlace <- countriesList %>% rbind(c("Anywhere","Anywhere")) %>% filter(Name == input$select.dest) %>% select(ID)
      
      #Variables:
      country = "US"
      currency = "USD"
      locale = "en-us"
      originplace=translatedoriginplace[1,1]
      destinationplace=translatedDestPlace[1,1]
      outbounddate = input$depart.data
      #inbounddate="2017-12"
      apikey="te892026803091243844897141219716"
      
      progress$set(message = "Routing API", value = 1)
      
      routes.url <- paste0("http://partners.api.skyscanner.net/apiservices/browsequotes/v1.0/",country,"/",currency,"/",locale,"/",originplace,"/",destinationplace,"/", outbounddate,"?apiKey=", apikey)
      
      response <- GET(routes.url)
      query <- fromJSON(content(response, "text"))
      
      places <- flatten(query$Places)
      #routes <- flatten(query$Routes)
      quotes <- flatten(query$Quotes) %>% dplyr::select(2,7,8)
      #carriers <- flatten(query$Carriers)
      #currencies <-flatten(query$Currencies)
      
      progress$set(message = "Filtering Data", value = 3)
      
      places.countries.only <- filter(places, Type == "Country") %>% dplyr::select(2,4)
      places.stations.only <- filter(places, Type == "Station") %>% dplyr::select(1,6,8) %>% 
         left_join(quotes, by = c("PlaceId" = "OutboundLeg.DestinationId"))
      places.min.price <- summarise(group_by(places.stations.only, PlaceId),m = min(MinPrice))
      places.stations.only <- places.stations.only %>%  dplyr::select(1,3) %>% unique() %>% 
         left_join(places.min.price) %>% na.omit()
      
      progress$set(message = "Plotting World Map", value = 4)
      
      world <- map_data('world') %>% filter(region!='Antarctica')
      
      if(input$select.dest != "Anywhere"){
         places.countries.only <- places.countries.only %>% rbind(c(input$select.dest, destinationplace))
      }
      
      world <- mutate(world, code = iso.alpha(world$region, n=2)) %>%
         left_join(places.countries.only, by = c("code" = "SkyscannerCode")) %>% 
         unique()
      
      currentCountry <- filter(world, code == translatedoriginplace[1,1])
      
      world2 <- filter(world, code != translatedoriginplace[1,1])
      
      worldForHeatmap <- inner_join(world2, places.stations.only, by = c("Name" = "CountryName")) %>% dplyr::select(long,lat,m,group)%>%  na.omit()
      colnames(worldForHeatmap) <- c("x","y","m","group")
      
      breaks = c(0, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 10000)
      
      progress$set(message = "Rendering World Map", value = 5)
      progress$set(message = "Finalizing...", value = 6)
      
      on.exit(progress$close())
      
      g <- ggplot(data = world2) + 
         geom_polygon(aes(long,lat,group=group),fill="#dadee5",colour="black",size=0.05) +
         coord_equal() + 
         scale_x_continuous(expand=c(0,0)) + 
         scale_y_continuous(expand=c(0,0)) +
         labs(x='Longitude', y='Latitude') +
         theme(panel.background = element_rect(fill = "#dbecff"))+
         geom_polygon(data = worldForHeatmap, aes(x = x, y = y, group=group, fill = cut(m, breaks)),colour="#000000",size=0.05) +
         geom_polygon(data = currentCountry, aes(long,lat,group=group),fill="#d04efc",size=0.1, colour="#000000")+
         scale_fill_brewer(palette = "RdYlGn", direction = -1, name = "Minimum Price ($)", labels = c("0-100","100-200", "200-300","300-400","400-500","500-600","600-700","700-800","800-900","900-1000","1000+")) +
         coord_quickmap() + 
         labs(x='Longitude', y='Latitude', title = "Prices Based on Destination")
      
      g
   })
   
   output$top.min.price.summary <- renderText({
      table.summary <- ("The following table shows the top five cheapest countries you can fly to given the country of origin and
                       and the departure date that you selected. Within the table, you can also see the minimum price to fly
                       there at this current time. Remember that the data is live so it might change if you look back later.
                       This table can be useful for you as the user if you want to have a get away outside the country but
                       you want to fly for cheap. ")
      return(table.summary)
   })  
}

shinyServer(server)

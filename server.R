library("shiny")
library("dplyr")
library("jsonlite")
library("knitr")
library("httr")
library("ggplot2")
library("XML")
library("scales")

options(shiny.maxRequestSize=980*1024^2)
options(scipen = 15000)

memoryVector <- c("country","dept","dest")

server <- function(input, output, session) {
   
      
      
      getMemory <- function(){
         return(memoryVector)
      }
      
      setMemory <- function(store){
         memoryVector <- store
      }
   
      observe({
         
         countriesList <- read.csv("./data/geo.csv", stringsAsFactors = FALSE)
         
         translatedoriginplace <- filter(countriesList, Name == input$select.country) %>% select(ID)
         translatedDestPlace <- countriesList %>% rbind(c("Anywhere","Anywhere")) %>% filter(Name == input$select.dest) %>% select(ID)
         
         #Variables:
         country = "US"
         currency = "USD"
         locale = "en-us"
         originplace = translatedoriginplace[1,1]
         destinationplace = translatedDestPlace[1,1]
         outbounddate = input$depart.data
         #inbounddate="2017-12"
         apikey="te892026803091243844897141219716"
         
         routes.url <- paste0("http://partners.api.skyscanner.net/apiservices/browsequotes/v1.0/",country,"/",currency,"/",locale,"/",originplace,"/",destinationplace,"/", outbounddate,"?apiKey=", apikey)
         
         response <- GET(routes.url)
         query <- fromJSON(content(response, "text"))
         
         places <- flatten(query$Places)
         quotes <- flatten(query$Quotes)

         places.stations.only <- filter(places, Type == "Station")
         from <- places.stations.only %>% filter(CountryName == input$select.country)
         to <- places.stations.only %>% filter(CountryName == input$select.dest)
         
         if (input$select.dest != "Anywhere" && isTRUE(getMemory()[1] != input$select.dest)){
            
            if(isTRUE(getMemory()[1] != input$select.dest && isTRUE(getMemory()[2:3] == c(input$depId,input$destId)))){
               updateSelectInput(session, "depId", label = "Select Departure Airport:", choices = from$Name, selected = from$Name[1])
               updateSelectInput(session, "destId", label = "Select Destination Airport:", choices = to$Name, selected = to$Name[1])
            }
            
            if(isTRUE(getMemory()[2:3] != c(input$depId,input$destId)) || input$depId == "None"){
               updateSelectInput(session, "depId", label = "Select Departure Airport:", choices = from$Name, selected = from$Name[1])
               updateSelectInput(session, "destId", label = "Select Destination Airport:", choices = to$Name, selected = to$Name[1])
            }
            
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
            outbounddate = input$depart.data
            #inbounddate="2017-12"
            apikey="te892026803091243844897141219716"
            
            routes.url <- paste0("http://partners.api.skyscanner.net/apiservices/browsequotes/v1.0/",country,"/",currency,"/",locale,"/",originplace,"/",destinationplace,"/", outbounddate,"?apiKey=", apikey)
            
            response <- GET(routes.url)
            query <- fromJSON(content(response, "text"))
            
            if (is.data.frame(query$Quotes)){
               quotes <- flatten(query$Quotes)
               
               uniqueIDx <- quotes$OutboundLeg.OriginId
               uniqueIDy <- quotes$OutboundLeg.DestinationId
               
               xname <- places.stations.only %>% filter(PlaceId == uniqueIDx[1]) %>% select(Name)
               yname <- places.stations.only %>% filter(PlaceId == uniqueIDy[1]) %>% select(Name)
               
               finalunique <- paste0("Route: ",xname, " -> ",yname) 
               
               output$message <- renderText({
                  finalunique
               })
            }else{
               output$message <- renderText({
                  "Quotes is Missing"
               })
            } 
         } else{
            #x <- character(0)
            updateSelectInput(session, "depId", label = "Warning: Please Select Destination Country", choices =  c("None"), selected = "None")
            updateSelectInput(session, "destId", label = "Warning: Please Select Destination Country", choices =  c("None"), selected = "None")
         }
         setMemory(c(input$select.dest, input$depId, input$destId))
      })
   
   
      getTable <- function(input,output){
         countriesList <- read.csv("./data/geo.csv", stringsAsFactors = FALSE)
         
         progress <- shiny::Progress$new()
         progress$set(message = "Computing Data", value = 0)
         
         translatedoriginplace <- filter(countriesList, Name == input$select.country) %>% select(ID)
         translatedDestPlace <- countriesList %>% rbind(c("Anywhere","Anywhere")) %>% filter(Name == input$select.dest) %>% select(ID)
         
         #Variables:
         country = "US"
         currency = "USD"
         locale = "en-us"
         originplace = translatedoriginplace[1,1]
         destinationplace = translatedDestPlace[1,1]
         outbounddate = input$depart.data
         #inbounddate="2017-12"
         apikey="te892026803091243844897141219716"
         
         progress$set(message = "Routing API", value = 1)
         
         routes.url <- paste0("http://partners.api.skyscanner.net/apiservices/browsequotes/v1.0/",country,"/",currency,"/",locale,"/",originplace,"/",destinationplace,"/", outbounddate,"?apiKey=", apikey)
         
         response <- GET(routes.url)
         query <- fromJSON(content(response, "text"))
         
         places <- flatten(query$Places)
         quotes <- flatten(query$Quotes) #%>% dplyr::select(2,7,8)
         carriers <- flatten(query$Carriers)
         #currencies <-flatten(query$Currencies)
         
         places.countries.only <- filter(places, Type == "Country") %>% dplyr::select(2,4)
         places.stations.only <- filter(places, Type == "Station") %>% dplyr::select(1,6,8) %>% 
            left_join(quotes, by = c("PlaceId" = "OutboundLeg.DestinationId"))
         places.min.price <- summarise(group_by(places.stations.only, PlaceId),m = min(MinPrice))
         places.stations.only <- places.stations.only %>%  dplyr::select(1:3,5,6,8) %>% 
            unique() %>% left_join(places.min.price)
         
         places.stations.only <- arrange(places.stations.only, m) %>% filter(CountryName != input$select.country)  %>% select(2:5)
         
         colnames(places.stations.only) <- c("City Name","Country Name","Price ($ Dollars)","Direct Flight")
         
         progress$set(message = "Constructing Table", value = 4)
         on.exit(progress$close())
         
         return(places.stations.only)
      }   
   
   output$table <- renderDataTable({getTable(input,output)})
   
   output$world.map <- renderPlot({
      
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
     table.summary <- ("The above table shows the top five cheapest countries you can fly to given the country of origin and
                       and the departure date that you selected. Within the table, you can also see the minimum price to fly
                       there at this current time. Remember that the data is live so it might change if you look back later.
                       This table can be useful for you as the user if you want to have a get away outside the country but
                       you want to fly for cheap. ")
     return(table.summary)
   })  
  
}

shinyServer(server)

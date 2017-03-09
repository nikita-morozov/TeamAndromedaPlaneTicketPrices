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

server <- function(input, output, session) {
      
      getTable <- function(input,output){
         countriesList <- read.csv("./data/geo.csv", stringsAsFactors = FALSE)
         
         progress <- shiny::Progress$new()
         progress$set(message = "Computing Data", value = 0)
         
         translatedoriginplace <- filter(countriesList, Name == input$select.country) %>% select(ID)
         
         #Variables:
         country = "US"
         currency = "USD"
         locale = "en-us"
         originplace=translatedoriginplace[1,1]
         destinationplace="Anywhere"
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
      
      #Variables:
      country = "US"
      currency = "USD"
      locale = "en-us"
      originplace=translatedoriginplace[1,1]
      destinationplace="Anywhere"
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
        left_join(places.min.price)
      
      progress$set(message = "Plotting World Map", value = 4)
      
      world <- map_data('world') %>% filter(region!='Antarctica')
      
      world <- mutate(world, code = iso.alpha(world$region, n=2)) %>%
        left_join(places.countries.only, by = c("code" = "SkyscannerCode")) %>% 
        unique()
      
      currentCountry <- filter(world, code == translatedoriginplace[1,1])
      
      world <- filter(world, code != translatedoriginplace[1,1])
      
      worldForHeatmap <- inner_join(world, places.stations.only, by = c("Name" = "CountryName")) %>% dplyr::select(long,lat,m,group)%>%  na.omit()
      colnames(worldForHeatmap) <- c("x","y","m","group")
      
      breaks = c(0, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 10000)
      
      progress$set(message = "Rendering World Map", value = 5)
      progress$set(message = "Finalizing...", value = 6)
      
      on.exit(progress$close())
      
      ggplot(data = world) + 
       geom_polygon(aes(long,lat,group=group),fill="#dadee5",colour="black",size=0.05) +
       geom_polygon(data = currentCountry, aes(long,lat,group=group),fill="#d04efc",colour="#fca6f9",size=0.1) +
       coord_equal() + 
       scale_x_continuous(expand=c(0,0)) + 
       scale_y_continuous(expand=c(0,0)) +
       labs(x='Longitude', y='Latitude') +
       theme(panel.background = element_rect(fill = "#dbecff"))+
       geom_polygon(data = worldForHeatmap, aes(x = x, y = y, group=group, fill = cut(m, breaks))) +
       scale_fill_brewer(palette = "RdYlGn", direction = -1, name = "Minimum Price ($)", labels = c("0-100","100-200", "200-300","300-400","400-500","500-600","600-700","700-800","800-900","900-1000","1000+")) +
       coord_quickmap() + 
       labs(x='Longitude', y='Latitude', title = "Prices Based on Destination") 
      
      #Allows the plot to be interactive with mouseover
      #ggplotly(world.map)
  })
  
}

shinyServer(server)

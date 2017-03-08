####################
####### Prep #######
####################

library("dplyr")
library("jsonlite")
library("knitr")
library("httr")
library("ggplot2")
library("XML")
library("rgdal")
library("data.table")
library("raster")
library("grid")
library("sp")
library("ggmap")
library("get_map")
library("leaflet")

#Variables:
country = "US"
currency = "USD"
locale = "en-us"
originplace="US"
destinationplace="Anywhere"
outbounddate="2017-04-01"
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
#routes <- flatten(query$Routes)
quotes <- flatten(query$Quotes) %>% dplyr::select(2,7,8)
carriers <- flatten(query$Carriers)
currencies <-flatten(query$Currencies)

places.countries.only <- filter(places, Type == "Country") %>% dplyr::select(2,4)
places.stations.only <- filter(places, Type == "Station") %>% dplyr::select(1,6,8) %>% 
  left_join(quotes, by = c("PlaceId" = "OutboundLeg.DestinationId"))
places.min.price <- summarise(group_by(places.stations.only, PlaceId),m = min(MinPrice))
places.stations.only <- places.stations.only %>%  dplyr::select(1:3,5) %>% unique() %>% 
  left_join(places.min.price)

world <- map_data('world') %>% filter(region!='Antarctica')
world <- mutate(world, code = iso.alpha(world$region, n=2)) %>%
  left_join(places.countries.only, by = c("code" = "SkyscannerCode")) %>% 
  unique()


worldForHeatmap <- inner_join(world, places.stations.only, by = c("Name" = "CountryName")) %>% dplyr::select(long,lat,m,group) %>%  na.omit()
colnames(worldForHeatmap) <- c("x","y","m","group")

world.cloropleth.map <- left_join(world, worldForHeatmap)

#######################
########FORMAT#########
#######################

#Change data frame into raster or Data Frame Spacial Grid for use with HeatMap

sgdf_transform = function(sgdf){
  class(sgdf)
  coordinates(sgdf) <- ~x+y
  dim <- (points2grid(sgdf, tolerance = .95, round = NULL))@cells.dim  
  bbox <- sgdf@bbox
  r <- raster(xmn=bbox[1,1], xmx=bbox[1,2], ymn=bbox[2,1], ymx=bbox[2,2], ncols=dim[1], nrows=dim[2])
  r <- setValues(r,matrix(sgdf@data$m, nrow = dim[1], ncol = dim[2]) %>% t()) 
  data <- rasterToPoints(r) %>% data.table()
  return(data)
  
}

worldHeatMapDataSpatialGrid <- sgdf_transform(worldForHeatmap)


########################
#########PLOT###########
########################
breaks = c(0, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 10000)
ggplot(data = world.cloropleth.map) + 
  geom_polygon(aes(x = long, y = lat, group=group, fill = cut(m, breaks))) +
  coord_quickmap() + 
  #scale_x_continuous(expand=c(0,0)) + 
  #scale_y_continuous(expand=c(0,0)) +
  labs(x='Longitude', y='Latitude', title = "Prices Based on Destination") #+
  #theme_bw()

########################
####PLOT WITH GGMAPS####
########################
ggmap.world <- inner_join(places.stations.only, world.cloropleth.map)


breaks = c(0, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 10000)
ggmap(data = world.cloropleth.map) + 
  geom_polygon(aes(x = long, y = lat, group=group, fill = cut(m, breaks))) +
  coord_quickmap() + 
  #scale_x_continuous(expand=c(0,0)) + 
  #scale_y_continuous(expand=c(0,0)) +
  labs(x='Longitude', y='Latitude', title = "Prices Based on Destination") #+
#theme_bw()

########################
####PLOT WIH LEAFLET####
########################
breaks = c(0, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 10000)
world.map.leaflet <- leaflet(world.cloropleth.map) %>% 
  addPolygons(aes(x = long, y = lat, group=group, fill = cut(m, breaks))) +
  coord_quickmap() + 
  #scale_x_continuous(expand=c(0,0)) + 
  #scale_y_continuous(expand=c(0,0)) +
  labs(x='Longitude', y='Latitude', title = "Prices Based on Destination") #+
#theme_bw()


  
####################
#######  XML #######
####################

#Ignore

#lines   <- readLines("planes.xml")
#start   <- grep('<?xml version="1.0" encoding="UTF-8"?>',lines,fixed=T)
#end     <- c(start[-1]-1,length(lines))
#library(XML)
#get.xml <- function(i) {
#  txt <- paste(lines[start[i]:end[i]],collapse="\n")
#  # print(i)
#  xmlTreeParse(txt,asText=T)
#  # return(i)
#}
#docs <- lapply(1:10,get.xml)
#class(docs[[1]])

#xmlfile <- xmlTreeParse("./planes.xml")
#class(xmlfile)
#xmltop = xmlRoot(xmlfile)
#print(xmltop[[2]][[1]])

#xmlSize(xmltop[[1]]) #number of nodes in each child
#xmlSApply(xmltop[[1]], xmlName) #name(s)
#xmlSApply(xmltop[[1]], xmlAttrs) #attribute(s)
#xmlSApply(xmltop[[1]], xmlSize) #size


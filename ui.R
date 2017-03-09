library("shiny")
library("plotly")
library("xml2")

# Creates a data-table that is reads in the cvs file of the list of the countries.
countriesList <- read.csv("./data/geo.csv", stringsAsFactors = FALSE)

# Prints out various elements
print.introduction <- print(paste0("Our project uses a live flight price API from SkyScanner.net to track
                                   cost trends of airplane tickets around the globe. Using the data that we
                                   gain, we present it by letting our costumers interact with it in multiple
                                   ways. First, they will be introduced to our World Map that allows them to
                                   quickly get an idea of which countries it is cheap or expensive to fly into
                                   by interacting with the map. Once they have made their decision then they
                                   can select their preferred date and location of departure and choose an
                                   arrival location around the world. We also provide the opportunity for
                                   them to select the airports that they would like to depart from and directly
                                   fly into. We also visualize when a specific ticket should be bought and show
                                   our users how good of a price they are getting if they decide to buy on any
                                   given day. Our purpose is to optimize a user's ability to get a ticket for
                                   the best price and to provide an interface that helps users decide if it is
                                   the right time to buy tickets. This system might serve as a foundation for
                                   a more intricate process of purchasing in the future."))

print.target.audiance <- print(paste0("The target demographic for this product is the large population
                                      of users who travel by plane and purchase plane tickets frequently
                                      or consistently. It is directed at people who have trouble understanding
                                      the ways ticket prices fluctuate and who are not always getting the best
                                      price. This interface is meant to simplify a user's decision making
                                      dilemmas, lead to an understanding of ticket prices, and save them money."))

print.dataset <- print(paste0("As mentioned above we are using Skyscanner 's Business API for the live pricing
                              of flights. The Live Pricing Service returns all the LIVE flights available for a
                              specific route and date (or single date for one-way searches). The response is a
                              list of all the flights that can be booked for the selected airports and dates,
                              along with a deeplink to the provider's website (airline or travel agent).
                              The Browse Cache will give us access to aggregated data of flights from the past."))

print.worldmap <- print(paste0("Our World Map creates a visual and a dynamic map of the countries depending on the date and departing countries to experience
                               a way for our users to interact with the live data from SkyScanner.  With this
                               information, they can immediately observe the countries that are cheaper or more
                               expensive to fly depending on the date that they selected for departure.
                               We selected a green to red color palette to represent the prices of the tickets,
                               meaning that gradients of green displays affordable and cheap tickets compared
                               to other passes, between $0 to $400. Gradients of yellow displays regular priced
                               tickets, ranging from $400 to $900, while the gradients of red displays
                               tremendously expensive tickets, ranging from $900 to over $1000, these we do
                               not recommend purchasing at this moment. Prices go up and down for many reasons.
                               No one can really predict when or if a price will change. Only the airline knows
                               that. But there are four things that drive prices: competition, supply, demand,
                               and oil prices. The first and last items are the ones that really affect prices
                               the most. However, different observations can be made by inspecting airplane ticket,
                               for example, we can tell countries economical, health, safety and tourist state of being
                               by looking at the extreme sides of the prices. Finally, we present the current location of our user
                               by higlhiting the country in purple to give a more fluid user experience."))

ui <- fluidPage(
  titlePanel("Airline Ticket Price Trends"),

  # Include a `sidebarLayout()`
  sidebarLayout(
    sidebarPanel(
      dateInput("depart.data", label = "Departure Date", value = "2017-03-20",max = "2018-03-08", min = "2017-03-15", startview = "month", weekstart = 0),
      selectInput('select.country', "Select Departure Country", choices = countriesList$Name, selected = 'United States'),
      selectInput('select.dest', "Select Destination Country", choices = countriesList %>% rbind(c("Anywhere","Anywhere")) %>% select(Name), selected = 'Anywhere'),
      selectInput("depId", label = "Warning: Please Select Destination Country", choices =  c("None"), selected = "None"), 
      selectInput("destId", label = "Warning: Please Select Destination Country", choices =  c("None"), selected = "None"),
      submitButton("Update View", icon("refresh"))
      ),
    
    # Creates the main panel containing output elements such as setting the tabs for the program.
    # About Us explains the Mission Statement for the project
    # WorldMap contains the explanation and the extended legends with specified colors
    mainPanel(
       tabsetPanel(type = "tabs", 
                   tabPanel("About Us", titlePanel("Team Andromeda"), br(), h3("Introduction"),
                            p(print.introduction), br(), h3("Target Audience"), print.target.audiance, br(), br(), h3("Data Set"), print.dataset, br(), br(), h3("Team Members"),
                            img(src="nikita.jpg", height = 300, width = 300), h4("Nikita Morozov"), br(),
                            img(src="monica.jpg", height = 200, width = 350), h4("Monica Riley"), br(), img(src="mano.jpg", height = 200, width = 350), h4("Mano Barkovics")),
                   tabPanel("World Map", plotOutput("world.map",width = "100%",height = "400px"),
                            textOutput("timeline.comment"), h5("Legend"), h5(span("Light Blue", style = "color:#dbecff "), " = Oceans"),
                            h5(span("Purple", style = "color:#d04efc"), " = Current Location"),
                            h5(span("Gray", style = "color:#dadee5"), " = No information available"),
                            titlePanel("Visual Experience"), print.worldmap, br(), br()),
                   tabPanel("Where Should I Go?", dataTableOutput('table'), textOutput("top.min.price.summary")),
                   tabPanel("Route Ticket Price Fluctuation",textOutput("message"), plotOutput('route.chart'))
       ) 
    )
  )
)

# call `shinyUI()` to create the UI out of your `ui` value

shinyUI(ui)

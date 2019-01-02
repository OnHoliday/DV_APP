require(shiny)
require(leaflet)
require(htmltools)
require(ggplot2)
library(shinythemes)
library(shinydashboard)
require(dplyr)
library("magrittr")



df.station <- read.csv("station.csv", header=TRUE, sep=",")
df.trip <- read.csv("trip.csv", header=TRUE, sep=",")

#df <- left_join(df.trip,df.station, by = c("from_station_id"="station_id")

df.trip$starttime <- as.POSIXct(strptime(x = as.character(df.trip$starttime),
                                         format = "%m/%d/%Y %H:%M"))
df.trip$stoptime <- as.POSIXct(strptime(x = as.character(df.trip$stoptime),
                                        format = "%m/%d/%Y %H:%M"))

first = df.trip %>% summarise(sort(starttime)[1])

last = df.trip %>% summarise(sort(stoptime)[50791])


# library(RCurl)
# x <- getURL("https://raw.github.com/aronlindberg/latent_growth_classes/master/LGC_data.csv")
# y <- read.csv(text = x)shiny


hav.dist <- function(long1, lat1, long2, lat2) {
  R <- 6371
  diff.long <- (long2 - long1)
  diff.lat <- (lat2 - lat1)
  a <- sin(diff.lat/2)^2 + cos(lat1) * cos(lat2) * sin(diff.long/2)^2
  b <- 2 * asin(pmin(1, sqrt(a))) 
  d = R * b
  return(d)
}



ui <- navbarPage(title =  "CITY BIKE - Seattle", position = "fixed-top",  footer = 'AAA', collapsible = TRUE, fluid = TRUE,
                 theme = shinytheme("slate"),
                 tabPanel("HOMEPAGE",
                          #includeCSS("styles.css"),
                          headerPanel("New Application")),
                 tabPanel("STATIONS",
                          fluidPage(
                            titlePanel("Station - Analysis"),
                            sidebarLayout(
                              sidebarPanel("our inputs will go here", 
                                           sliderInput("numberInput", "Number", min = 1, max = 58,
                                                       value = c(10), pre = "nr "),
                                           radioButtons("typeInput", "Product type",
                                                        choices = c("Male", "Female"),
                                                        selected = "Male")),
                              mainPanel(br(), br(),
                                        leafletOutput("firstExample", height=700, width = 700))))),
                 tabPanel("TRIPS", 
                          sidebarLayout(position = "right",
                                        sidebarPanel(
                                          dateInput("date", "Choose a date:", value = format(first,"%Y-%m-%d")
                                                    , min = format(first,"%Y-%m-%d"), max= format(last,"%Y-%m-%d")
                                                    ,format = "yyyy-mm-dd", startview="month"
                                          ),

                                          sliderInput("hour", "Hour:", min=0, max=23, value=0, step = 1),
                                          radioButtons("station", "Direction",
                                                       choices = c("Start Station", "End Station"),
                                                       selected = "Start Station")),
                                        mainPanel(
                                          leafletOutput("secondExample", height=700)
                                        )
                          )
                 ),
                 tabPanel("ANLYSIS"),
                 navbarMenu("MORE",
                            tabPanel("SUMMARY"),
                            "----",
                            tabPanel("DATASET", dataTableOutput("mytable1")),
                            "----",
                            tabPanel("ABOUT US")
                            
                            
                            #Third example
                            #tabPanel()
                 )
                 
                 
)


server <- function(input, output) {
  
 
  numOfStation <- reactive({
    df.station[sample(nrow(df.station),input$numberInput[1]), ] })
  

  
  # tmp.df <- reactive ({df.trip[format(starttime,"%Y-%m-%d")==input$date]})
  # print(df.trip[format(df.trip$starttime,"%Y-%m-%d")==input$date][1])
  # filteredData <- reactive({df.trip[format(df.trip$starttime,"%Y-%m-%d")==input$date]})

  
      #tmp.df <- df.station[sample(nrow(df.station), 10), ]
  
  #distt <- hav.dist(long1, lat1, long2, lat2)
  
  output$firstExample <- renderLeaflet({
    
    
    leaflet(numOfStation()) %>%
      #Here is where tiles providers live
      addTiles(group="OSM") %>%#OSM is default tile providor
      addProviderTiles(providers$Stamen.TonerLite) %>%
      #addProviderTiles(providers$Thunderforest.TransportDark) %>%
      #This is Seattle
      setView(
        lng=-122.335167,
        lat=47.608013,
        zoom=12
      ) %>%
      #Adding airbnb houses
      addMarkers(~long, ~lat, popup=~htmlEscape(name))
    #Group of Layers
  })
  
  output$secondExample <- renderLeaflet({

    observeEvent(input$secondExample_marker_click, {
      click <- input$secondExample_marker_click
    })

    #Temporary dataframe
    
    
    #leaflet
    leaflet(tmp.df()) %>%
      #Here is where tiles providers live
      addTiles(group="OSM") %>%#OSM is default tile providor
      addProviderTiles(providers$CartoDB.Positron) %>%
      #addProviderTiles(providers$Stamen.TonerLite) %>%
      #addProviderTiles(providers$Esri.WorldImagery) %>%
      #This is Seattle
      setView(
        lng=-122.335167,
        lat=47.608013,
        zoom=12
      )%>%
      addCircleMarkers(lng = ~long, lat = ~lat, weight = 1,label=~name,
                       radius = ~n_trips)
  })

  filtered <- reactive({
    df.trip %>%
      filter(format(df.trip$starttime,"%Y-%m-%d") == input$date) %>%
      group_by(from_station_id) %>%
      summarize(n_trips = n())
    
  })
  
  tmp.df <- reactive({
    left_join(filtered(), df.station, by = c("from_station_id" = "station_id"))
  })
  
  
  output$mytable1 = renderDataTable({
    # df.trip 
    tmp.df()
  }, options = list(aLengthMenu = c(5, 35, 50), iDisplayLength = 8))
  
}

shinyApp(ui, server)
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
                              sidebarPanel(
                                           selectInput("stationInput", "Select Station",
                                                       choices = unique(df.station$name),
                                                       selected = "3rd Ave & Broad St"
                                           ),
                                           sliderInput("distInput", "Distance", min = 10, max = 500,
                                                       value = c(100), pre = "M ")),
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
                 )
                 
                 
)


server <- function(input, output) {
  
#### REACTIVE VARAIBELS ####################  
  numOfStation <- reactive({
    df.station[sample(nrow(df.station),input$numberInput[1]), ] })
  
  startStation <- reactive({
    df.station %>%
        filter(df.station$name == input$stationInput)
    })

  nearStation <- reactive({
    selLat = startStation()$lat
    selLong = startStation()$long
    for (i in 1:58) {
    df.station$dist[i] = hav.dist(selLong, selLat, df.station$long[i], df.station$lat[i])
    } 
    df.station %>%
      filter(df.station$dist <= input$distInput)
  })
  
  # tmp.df <- reactive ({df.trip[format(starttime,"%Y-%m-%d")==input$date]})
  # tmp.df <- df.station[sample(nrow(df.station), 10), ]

#### OUTOUT TO STATION TABSET ####################  
  
  output$firstExample <- renderLeaflet({
    
    leaflet(nearStation()) %>% #numOfStation()) %>%
      addTiles(group="OSM") %>%#OSM is default tile providor
      addProviderTiles(providers$Stamen.TonerLite) %>%
      setView(
        lng=-122.335167,
        lat=47.608013,
        zoom=12
      ) %>%
      addMarkers(~long, ~lat, popup=~htmlEscape(name))
  })

#### OUTOUT TO TRIP TABSET ####################  
  
  output$secondExample <- renderLeaflet({
    
    observeEvent(input$secondExample_marker_click, {
      click <- input$secondExample_marker_click
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
    
    leaflet(tmp.df()) %>%
      addTiles(group="OSM") %>%#OSM is default tile providor
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(
        lng=-122.335167,
        lat=47.608013,
        zoom=12
      )%>%
      addCircleMarkers(lng = ~long, lat = ~lat, weight = 1,label=~name,
                       radius = ~n_trips)
  })
  
#### OUTOUT TO DATA FRAME ####################  
  
# YOU CAN USE IT TO LOOK INTO YOUR DATA, PUT WHATEVER DF YOU NEED, CHECK WHETHER YOU CORRECTLY FILTERED DATA OR NEW COLUMN HAS PROPER VALUES AND SO ON
  
  output$mytable1 = renderDataTable({
     #df.trip
    nearStation()
  }, options = list(aLengthMenu = c(5, 35, 50), iDisplayLength = 8))
  
}

shinyApp(ui, server)

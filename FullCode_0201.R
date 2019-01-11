require(shiny)
require(leaflet)
require(htmltools)
require(ggplot2)
library(shinythemes)
library(shinydashboard)
require(dplyr)
library("magrittr")


df.station <- read.csv("dane/station.csv", header=TRUE, sep=",")
df.trip <- read.csv("dane/trip.csv", header=TRUE, sep=",")

#df <- left_join(df.trip,df.station, by = c("from_station_id"="station_id")

df.trip$starttime <-  as.Date(df.trip$starttime, format = "%m/%d/%Y")
df.trip$stoptime <- as.Date(df.trip$stoptime, format = "%m/%d/%Y")
df.trip$starttime2 <- as.POSIXct(strptime(x = as.character(df.trip$starttime),format = "%m/%d/%Y %H:%M"))	
df.trip$stoptime2 <- as.POSIXct(strptime(x = as.character(df.trip$stoptime),format = "%m/%d/%Y %H:%M"))

df.trip$starttime_weekend <- ifelse(is.element(weekdays(df.trip$starttime, abbreviate = FALSE), c("Saturday","Sunday")), "Weekend", "Weekday")
df.trip$stoptime_weekend <- ifelse(is.element(weekdays(df.trip$stoptime, abbreviate = FALSE), c("Saturday","Sunday")), "Weekend", "Weekday")


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
                          br(), br(),br(), 
                          titlePanel("New Cool Awesome Amazing Application"),
                          
                          headerPanel(
                            list(tags$head(tags$style()), 
                                 HTML('<img src="ofo-smart-city.png", height="600px", style="float:left"/>')
                            )),
                          tags$div(
                            tags$h1("CHECK OUT THE NEWSET POSSIBLE FEATURE")), 
                          tags$h2("PREPARE TO BE AMAZED !!!!"),
                          
                          
                          tags$a(href="www.github.com", tags$h4("Looking for a source code clik here!"))
                 ),
                 navbarMenu("CUSTOMER",
                            tabPanel("STATIONS",
                                     fluidPage(br(), br(),br(),
                                               titlePanel("Station - Analysis"),
                                               
                                               sidebarLayout(
                                                 # br(), br(),br(),br(), br(),br(),br(), br(),br(), 'a',
                                                 sidebarPanel(
                                                   
                                                   # "Here you can see sth",
                                                   radioButtons("stat_choice", "What do you want to check",
                                                                choices = c("Near station", "Distance"),
                                                                selected = "Near station"),
                                                   selectInput("stationInputA", "Select Start Station",
                                                               choices = unique(df.station$name)
                                                   ),
                                                   conditionalPanel(
                                                     condition = "input.stat_choice != 'Near station'",
                                                     
                                                     selectInput("stationInputB", "Select End Station",
                                                                 choices = unique(df.station$name)
                                                     )
                                                   ),
                                                   conditionalPanel(
                                                     condition = "input.stat_choice != 'Distance'",
                                                     sliderInput("distInput", "Stations within Distance", min = 10, max = 500,
                                                                 value = c(20), pre = "M ")
                                                   )),
                                                 
                                                 mainPanel(
                                                   leafletOutput("firstExample", height=700, width = 700)))
                                     )),
                            tabPanel("TRIPS",br(), br(),br(), 
                                     sidebarLayout(position = "right",
                                                   sidebarPanel(
                                                     
                                                     radioButtons("Day",
                                                                  label = "Select between days:",
                                                                  choices = c("Alldays","Weekday", "Weekend"),
                                                                  selected = "Alldays"),
                                                     
                                                     checkboxInput("specificday", "Select a specific Date"),
                                                     
                                                     conditionalPanel(
                                                       condition = "input.specificday != false",
                                                       
                                                       dateInput("date", "Choose a date:", value = format(first,"%Y-%m-%d")
                                                                 , min = format(first,"%Y-%m-%d"), max= format(last,"%Y-%m-%d")
                                                                 ,format = "yyyy-mm-dd", startview="month"
                                                       ),
                                                       
                                                       sliderInput("hour", "Hour:", min=0, max=23, value=0, step = 1),
                                                       
                                                       ,
                                                       radioButtons("station", "Direction",
                                                                    choices = c("Start Station", "End Station"),
                                                                    selected = "Start Station")
                                                     )
                                                   ),
                                                   
                                                   mainPanel(
                                                     leafletOutput("secondExample", height=700)
                                                   )
                                     )
                            )),
                 navbarMenu("COMPANY",
                            tabPanel("ANLYSIS-WHETHER",br(), br(),br()),
                            "----",
                            tabPanel("ANALYSIS-CHARTS",br(), br(),br())
                 ),
                 navbarMenu("MORE",
                            tabPanel("SUMMARY",br(), br(),br()),
                            "----",
                            tabPanel("DATASET",br(), br(),br(), dataTableOutput("mytable1")),
                            "----",
                            tabPanel("ABOUT US",br(), br(),br())
                 )
                 
                 
)


server <- function(input, output) {
  
  #### REACTIVE VARAIBELS ####################  
  numOfStation <- reactive({
    df.station[sample(nrow(df.station),input$numberInput[1]), ] })
  
  startStation <- reactive({
    df.station %>%
      filter(df.station$name == input$stationInputA)
  })
  
  stopStation <- reactive({
    df.station %>%
      filter(df.station$name == input$stationInputB)
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
  
  dist <- reactive({
    paste(toString(floor(hav.dist( startStation()$long, startStation()$lat, stopStation()$long, stopStation()$lat))), " meters")
  })
  
  tripStaton <- reactive({
    selLat1 = startStation()$lat
    selLong1 = startStation()$long
    selLat2 = stopStation()$lat
    selLong2 = stopStation()$long
    for (i in 1:58) {
      df.station$dist[i] = hav.dist(selLong1, selLat1, df.station$long[i], df.station$lat[i])
    } 
    df.station %>%
      filter(df.station$dist <= input$distInput)
    
  })
  
  selected <- reactive({
    if (toString(input$stat_choice)=="Near station") {
      return(TRUE)
    } else {
      return(FALSE)
    }
  })
  
  from_to <- reactive({
    if (toString(input$station)=="Start Station") {
      return(TRUE)
    } else {
      return(FALSE)
    }
  })
  
  date <- reactive({input$date})
  
  filtered <- reactive({
    if (toString(input$specificday) == 'FALSE') {
      if (input$Day == "Alldays"){
        
        var <- length(unique(df.trip$starttime))
        
        df.trip %>%
          filter()%>%
          group_by(from_station_id) %>%
          summarize(n_trips = n()/var) %>%
        left_join(df.station, by = c("from_station_id" = "station_id"))
      }
      
      else if (input$Day == "Weekend"){
        
        
        tempdf <- df.trip
        
        tempdf <- tempdf[tempdf$starttime_weekend == "Weekend", ]
        
        var <- length(unique(tempdf$starttime))
        
        df.trip %>%
          filter(df.trip$starttime_weekend == "Weekend") %>%
          group_by(from_station_id) %>%
          summarize(n_trips = n()/var) %>%
          left_join(df.station, by = c("from_station_id" = "station_id"))
      }
      else if (input$Day == "Weekday"){
        
        
        tempdf <- df.trip
        
        tempdf <- tempdf[tempdf$starttime_weekend == "Weekday", ]
        
        var <- length(unique(tempdf$starttime))
        
        df.trip %>%
          filter(df.trip$starttime_weekend == "Weekday") %>%
          group_by(from_station_id) %>%
          summarize(n_trips = n()/var) %>%
          left_join(df.station, by = c("from_station_id" = "station_id"))
      }
    }
    
    else {
      if (from_to() == TRUE) {
        df.trip %>%
          filter(format(df.trip$starttime,"%Y-%m-%d") == date()) %>%
          group_by(from_station_id) %>%
          summarize(n_trips = n()) %>% 
          left_join(df.station, by = c("from_station_id" = "station_id"))
      } else {
        df.trip %>% 
          filter(format(df.trip$endtime,"%Y-%m-%d") == date()) %>%
          group_by(to_station_id) %>%
          summarize(n_trips = n()) %>% 
          left_join(df.station, by = c("to_station_id" = "station_id"))
    }}
    
    
  })
  
  # tmp.df <- reactive ({df.trip[format(starttime,"%Y-%m-%d")==input$date]})
  # tmp.df <- df.station[sample(nrow(df.station), 10), ]
  
  #### OUTOUT TO STATION TABSET ####################  
  
  
  output$firstExample <- renderLeaflet({
    
    # observeEvent(input$secondExample_marker_click, {
    #   click <- input$secondExample_marker_click
    # })
    
    if (selected() == FALSE) {
      leaflet(tripStaton()) %>% #numOfStation()) %>%
        addTiles(group="OSM") %>%#OSM is default tile providor
        addProviderTiles(providers$Stamen.TonerLite) %>%
        setView(
          lng=-122.335167,
          lat=47.619113,
          zoom=12.46
        ) %>%
        addMarkers(startStation()$long, startStation()$lat) %>%
        addMarkers(stopStation()$long, stopStation()$lat) %>%
        addPolylines(lng = c(startStation()$long, stopStation()$long), lat = c(startStation()$lat, stopStation()$lat), layerId = NULL, color = "#03F", weight = 5, opacity = 0.5, fill = FALSE, fillColor = "#03F",
                     fillOpacity = 0.2, popup=dist())
      
    } else {
      leaflet(tripStaton()) %>% #numOfStation()) %>%
        addTiles(group="OSM") %>%#OSM is default tile providor
        addProviderTiles(providers$Stamen.TonerLite) %>%
        setView(
          lng=-122.335167,
          lat=47.619113,
          zoom=12.46
        ) %>%
        addMarkers(~long, ~lat, popup=~htmlEscape(name))
    }
    
    # %>%
    
    # addRectangles(
    #   lng1=startStation()$long, lat1=startStation()$lat,
    #   lng2=stopStation()$long, lat2=stopStation()$lat,
    #   fillColor = "transparent")
  })
  
  # output$firstExample <- renderLeaflet({
  #   
  #   leaflet(nearStation()) %>% #numOfStation()) %>%
  #     addTiles(group="OSM") %>%#OSM is default tile providor
  #     addProviderTiles(providers$Stamen.TonerLite) %>%
  #     setView(
  #       lng=-122.335167,
  #       lat=47.608013,
  #       zoom=12
  #     ) %>%
  #     addMarkers(~long, ~lat, popup=~htmlEscape(name))
  # 
  # })
  
  #### OUTOUT TO TRIP TABSET ####################  
  
  
  
  output$secondExample <- renderLeaflet({
    
    
    leaflet(filtered()) %>%
      addTiles(group="OSM") %>%#OSM is default tile providor
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(
        lng=-122.335167,
        lat=47.619113,
        zoom=12.46
      )%>%
      addCircleMarkers(lng = ~long, lat = ~lat, weight = 1,label=~name,
                       radius = ~n_trips)
  })
  
  #### OUTOUT TO DATA FRAME ####################  
  
  # YOU CAN USE IT TO LOOK INTO YOUR DATA, PUT WHATEVER DF YOU NEED, CHECK WHETHER YOU CORRECTLY FILTERED DATA OR NEW COLUMN HAS PROPER VALUES AND SO ON
  
  output$mytable1 = renderDataTable({
    #df.trip
    nearStation()
  }, options = list(aLengthMenu = c(5, 10, 15), iDisplayLength = 8))
  
}

shinyApp(ui, server)

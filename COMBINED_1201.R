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
df.weather <- read.csv("dane/weather.csv", header=TRUE, sep=",")

#df <- left_join(df.trip,df.station, by = c("from_station_id"="station_id")

df.trip$starttime_hours <-  as.numeric(format(strptime(df.trip$starttime, "%m/%d/%Y %H:%M"), "%H"))
df.trip$stoptime_hours <-  as.numeric(format(strptime(df.trip$stoptime, "%m/%d/%Y %H:%M"), "%H"))

df.trip$start_daytime <- ifelse(df.trip$starttime_hours >= 4 & df.trip$starttime_hours < 11,"Morning",
                                ifelse(df.trip$starttime_hours >= 11 & df.trip$starttime_hours < 17, "Afternoon",
                                       ifelse(df.trip$starttime_hours >= 17 & df.trip$starttime_hours < 22, "Evening", "Night")))


df.trip$stop_daytime <- ifelse(df.trip$stoptime_hours >= 4 & df.trip$stoptime_hours < 11,"Morning",
                               ifelse(df.trip$stoptime_hours >= 11 & df.trip$stoptime_hours < 17, "Afternoon",
                                      ifelse(df.trip$stoptime_hours >= 17 & df.trip$stoptime_hours < 22, "Evening", "Night")))



df.trip$starttime <-  as.Date(df.trip$starttime, format = "%m/%d/%Y")
df.trip$stoptime <- as.Date(df.trip$stoptime, format = "%m/%d/%Y")

df.trip$starttime_weekend <- ifelse(is.element(weekdays(df.trip$starttime, abbreviate = FALSE), c("Saturday","Sunday")), "Weekend", "Weekday")
df.trip$stoptime_weekend <- ifelse(is.element(weekdays(df.trip$stoptime, abbreviate = FALSE), c("Saturday","Sunday")), "Weekend", "Weekday")
df.weather$Date <- as.Date(df.weather$Date, format = "%m/%d/%Y")

trips_day <- df.trip %>%
  group_by(starttime) %>%
  summarize(count=n())%>%
  left_join(df.weather, by = c("starttime" = "Date"))%>%
  select(starttime,count,Mean_Temperature_F,Events)%>%
  filter( Events %in% c("Rain",""))%>%
  mutate(Rain = ifelse(Events == "", 0,1))



first = df.trip %>% summarise(sort(starttime)[1])
last = df.trip %>% summarise(sort(stoptime)[50791])

hav.dist <- function(long1, lat1, long2, lat2) {
  R <- 6371
  diff.long <- (long2 - long1)
  diff.lat <- (lat2 - lat1)
  a <- sin(diff.lat/2)^2 + cos(lat1) * cos(lat2) * sin(diff.long/2)^2
  b <- 20 * asin(pmin(1, sqrt(a))) 
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
                                     ))),
                 navbarMenu("COMPANY",
                            
                            tabPanel("TRIPS",br(), br(),br(), 
                                     sidebarLayout(position = "right",
                                                   sidebarPanel(
                                                     
                                                     conditionalPanel(
                                                       condition = "input.specificday == false",
                                                       
                                                       radioButtons("Day",
                                                                    label = "Select between days:",
                                                                    choices = c("Alldays","Weekday", "Weekend"),
                                                                    selected = "Alldays")
                                                     ),
                                                     
                                                     checkboxInput("specificday", "Select a specific Date"),
                                                     
                                                     conditionalPanel(
                                                       condition = "input.specificday != false",
                                                       
                                                       dateInput("date", "Choose a date:", value = format(first,"%Y-%m-%d")
                                                                 , min = format(first,"%Y-%m-%d"), max= format(last,"%Y-%m-%d")
                                                                 ,format = "yyyy-mm-dd", startview="month"
                                                       )
                                                       
                                                       
                                                       # ,
                                                       # radioButtons("station", "Direction",
                                                       #              choices = c("Start Station", "End Station"),
                                                       #              selected = "Start Station")
                                                     ),
                                                     radioButtons("station", "Direction",
                                                                  choices = c("Start Station", "End Station"),
                                                                  selected = "Start Station",inline = TRUE),
                                                     
                                                     selectInput("daytime","Time of Day",choices=c("All Day","Morning (6 a.m. - 11 a.m.)","Midday (11 a.m. - 5 p.m.)","Evening (5 p.m.- 10 p.m.)", "Night (10 p.m. - 4 a.m.)"),selected="All Day")
                                                     ,
                                                     h5("Top 5 most frequented stations")
                                                     ,
                                                     tableOutput('table')
                                                     ,
                                                     checkboxInput("top5", "Show on map")
                                                   )
                                                   ,
                                                   
                                                   mainPanel(
                                                     leafletOutput("secondExample", height=700)
                                                   )
                                     )),
                            "----",
                            
                            tabPanel("ANLYSIS-WHETHER",br(), br(),br(),
                                     sidebarLayout(position = "right",
                                                   sidebarPanel(
                                                     radioButtons("rain", "What kind of days do you want to inspect?",
                                                                  choices = c("Rainy days", "Sunny days"),
                                                                  selected = "Sunny days",inline = TRUE)
                                                     
                                                   ),
                                                   mainPanel(
                                                     plotOutput("plot")
                                                   )
                                     )),"----"
                           ,
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
  
  
  
  trips <- reactive({
    if (input$station == "Start Station") {
      if (toString(input$daytime) == "Morning (6 a.m. - 11 a.m.)") 
      {return(df.trip[df.trip$start_daytime == "Morning", ])}
      else if (toString(input$daytime) == "Midday (11 a.m. - 5 p.m.)") 
      {return(df.trip[df.trip$start_daytime == "Afternoon", ])}
      else if (toString(input$daytime) == "Evening (5 p.m.- 10 p.m.)")
      {return(df.trip[df.trip$start_daytime == "Evening", ])}
      else if (toString(input$daytime) == "Night (10 p.m. - 4 a.m.)")
      {return(df.trip[df.trip$start_daytime == "Night", ])}
      else 
      {return(df.trip)}}
    else {
      if (toString(input$daytime) == "Morning (6 a.m. - 11 a.m.)") 
      {return(df.trip[df.trip$stop_daytime == "Morning", ])}
      else if (toString(input$daytime) == "Midday (11 a.m. - 5 p.m.)") 
      {return(df.trip[df.trip$stop_daytime == "Afternoon", ])}
      else if (toString(input$daytime) == "Evening (5 p.m.- 10 p.m.)")
      {return(df.trip[df.trip$stop_daytime == "Evening", ])}
      else if (toString(input$daytime) == "Night (10 p.m. - 4 a.m.)")
      {return(df.trip[df.trip$stop_daytime == "Night", ])}
      else 
      {return(df.trip)}}
    
  })
  
  filtered <- reactive({
    if (toString(input$specificday) == 'FALSE') {
      
      if (toString(input$station) == 'Start Station') {
        
        
        if (input$Day == "Alldays"){
          
          var <- length(unique(trips()$starttime))
          
          trips() %>%
            filter()%>%
            group_by(from_station_id) %>%
            summarize(n_trips = n()/var) %>%
            left_join(df.station, by = c("from_station_id" = "station_id"))%>%
            mutate(example =  paste(name, as.integer(n_trips), sep = ", Average number of trips: "))
        }
        
        else if (input$Day == "Weekend"){
          
          
          tempdf <- trips()
          
          tempdf <- tempdf[tempdf$starttime_weekend == "Weekend", ]
          
          var <- length(unique(tempdf$starttime))
          
          trips() %>%
            filter(trips()$starttime_weekend == "Weekend") %>%
            group_by(from_station_id) %>%
            summarize(n_trips = n()/var) %>%
            left_join(df.station, by = c("from_station_id" = "station_id"))%>%
            mutate(example =  paste(name, as.integer(n_trips), sep = ", Average number of trips: "))
        }
        else if (input$Day == "Weekday"){
          
          
          tempdf <- trips()
          
          tempdf <- tempdf[tempdf$starttime_weekend == "Weekday", ]
          
          var <- length(unique(tempdf$starttime))
          
          trips() %>%
            filter(trips()$starttime_weekend == "Weekday") %>%
            group_by(from_station_id) %>%
            summarize(n_trips = n()/var) %>%
            left_join(df.station, by = c("from_station_id" = "station_id"))%>%
            mutate(example =  paste(name, as.integer(n_trips), sep = ", Average number of trips: "))          }
      }
      
      else {
        
        
        if (input$Day == "Alldays"){
          
          var <- length(unique(trips()$starttime))
          
          trips() %>%
            filter()%>%
            group_by(to_station_id) %>%
            summarize(n_trips = n()/var) %>% 
            left_join(df.station, by = c("to_station_id" = "station_id"))%>%
            mutate(example =  paste(name, as.integer(n_trips), sep = ", Average number of trips: ")) 
        }
        
        else if (input$Day == "Weekend"){
          
          
          tempdf <- trips()
          
          tempdf <- tempdf[tempdf$starttime_weekend == "Weekend", ]
          
          var <- length(unique(tempdf$starttime))
          
          trips() %>%
            filter(trips()$starttime_weekend == "Weekend") %>%
            group_by(to_station_id) %>%
            summarize(n_trips = n()/var) %>% 
            left_join(df.station, by = c("to_station_id" = "station_id"))%>%
            mutate(example =  paste(name, as.integer(n_trips), sep = ", Average number of trips: "))
        }
        else if (input$Day == "Weekday"){
          
          
          tempdf <- trips()
          
          tempdf <- tempdf[tempdf$starttime_weekend == "Weekday", ]
          
          var <- length(unique(tempdf$starttime))
          
          trips() %>%
            filter(trips()$starttime_weekend == "Weekday") %>%
            group_by(to_station_id) %>%
            summarize(n_trips = n()/var) %>% 
            left_join(df.station, by = c("to_station_id" = "station_id"))%>%
            mutate(example =  paste(name, as.integer(n_trips), sep = ", Average number of trips: "))
        }
      }
    }
    
    
    else {
      if (from_to() == TRUE) {
        trips() %>%
          filter(format(trips()$starttime,"%Y-%m-%d") == date()) %>%
          group_by(from_station_id) %>%
          summarize(n_trips = n()) %>% 
          left_join(df.station, by = c("from_station_id" = "station_id"))%>%
          mutate(example =  paste(name, as.integer(n_trips), sep = ", Number of trips: "))
      }
      else {
        trips() %>% 
          filter(format(trips()$stoptime,"%Y-%m-%d") == date()) %>%
          group_by(to_station_id) %>%
          summarize(n_trips = n()) %>% 
          left_join(df.station, by = c("to_station_id" = "station_id"))%>%
          mutate(example =  paste(name, as.integer(n_trips), sep = ", Number of trips: "))
      }
    }
    
  })
  
  tmp.df <- reactive ({
    if (toString(input$top5) == 'TRUE') 
    {filtered() %>%
        arrange(desc(n_trips)) %>%
        head(5)}
    else {filtered()}
  })
  
  top5 <- reactive ({
    
    filtered() %>%
      arrange(desc(n_trips)) %>%
      head(5)
    
  })
  
  
  weather_trips <- reactive({
    if (toString(input$rain) == "Sunny days")
    {trips_day %>%
        filter( Rain %in% c(0))}
    else{
      trips_day %>%
        filter( Rain %in% c(1))
    }
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
        addMarkers(~long, ~lat, popup=~htmlEscape(name, ))
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
    
    #  tmp.df <- left_join(filtered(), df.station, by = c("from_station_id" = "station_id"))
    
    leaflet(tmp.df()) %>%
      addTiles(group="OSM") %>%#OSM is default tile providor
      addProviderTiles(providers$Stamen.TonerLite) %>%
      setView(
        lng=-122.335167,
        lat=47.619113,
        zoom=12.46
      )%>%
      addCircleMarkers(lng = ~long, lat = ~lat, weight = 1,label=~example,
                       radius = ~log(n_trips)*10)%>%
      addCircleMarkers(lng = ~long, lat = ~lat, weight = 1, radius = 2,5,fillOpacity = 4)
    
  })
  
  output$table <- renderTable(top5()%>%
                                select(name, n_trips)%>%
                                mutate(n_trips = as.integer(n_trips))%>%
                                rename(Station = name)%>%
                                rename(Trips = n_trips), spacing = "xs")
  
  
  output$plot <- renderPlot({
    ggplot(weather_trips(), aes(x=Mean_Temperature_F, y=count )) + geom_point(color="#30D5C8", size = 3)  + 
      geom_rug(color="grey")+ labs( x= 'Mean Temperature in Fahrenheit', y= "Number of trips")+
      theme_black()+
      theme(plot.background = element_rect(color= "#272B31",fill = "#272B31"),
            panel.border = element_rect(color = "#272B31"),
            panel.background = element_rect(fill = '#272B31'),
            panel.grid.major = element_line(colour = "#272B31", size=1.5),
            panel.grid.minor = element_line(colour = "#272B31", 
                                            size=.25, 
                                            linetype = "dashed"),
            #panel.border = element_blank(),
            axis.line.x = element_line(colour = "#272B31", 
                                       size=1.5, 
                                       lineend = "butt"),
            axis.line.y = element_line(colour = "#272B31", 
                                       size=1.5),
            axis.text = element_text(colour = "grey", face = "bold"),
            axis.title.x = element_text(size=14, face="bold.italic"),
            axis.title.y = element_text(size=14, face="bold.italic")
            )
    })
  
  #### OUTOUT TO DATA FRAME ####################  
  
  # YOU CAN USE IT TO LOOK INTO YOUR DATA, PUT WHATEVER DF YOU NEED, CHECK WHETHER YOU CORRECTLY FILTERED DATA OR NEW COLUMN HAS PROPER VALUES AND SO fill = ON
  
  output$mytable1 = renderDataTable({
    #df.trip
    trips()
  }, options = list(aLengthMenu = c(5, 10, 15), iDisplayLength = 8))
  
}

shinyApp(ui, server)



#####ggplot theme_black#####

library(gridExtra)

theme_black = function(base_size = 12, base_family = "") {
  
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    
    theme(
      # Specify axis options
      axis.line = element_blank(),  
      axis.text.x = element_text(size = base_size*0.8, color = "white", lineheight = 0.9),  
      axis.text.y = element_text(size = base_size*0.8, color = "white", lineheight = 0.9),  
      axis.ticks = element_line(color = "white", size  =  0.2),  
      axis.title.x = element_text(size = base_size, color = "white", margin = margin(0, 10, 0, 0)),  
      axis.title.y = element_text(size = base_size, color = "white", angle = 90, margin = margin(0, 10, 0, 0)),  
      axis.ticks.length = unit(0.3, "lines"),   
      # Specify legend options
      legend.background = element_rect(color = NA, fill = "black"),  
      legend.key = element_rect(color = "white",  fill = "black"),  
      legend.key.size = unit(1.2, "lines"),  
      legend.key.height = NULL,  
      legend.key.width = NULL,      
      legend.text = element_text(size = base_size*0.8, color = "white"),  
      legend.title = element_text(size = base_size*0.8, face = "bold", hjust = 0, color = "white"),  
      legend.position = "right",  
      legend.text.align = NULL,  
      legend.title.align = NULL,  
      legend.direction = "vertical",  
      legend.box = NULL, 
      # Specify panel options
      panel.background = element_rect(fill = "black", color  =  NA),  
      panel.border = element_rect(fill = NA, color = "white"),  
      panel.grid.major = element_line(color = "grey35"),  
      panel.grid.minor = element_line(color = "grey20"),  
      panel.margin = unit(0.5, "lines"),   
      # Specify facetting options
      strip.background = element_rect(fill = "grey30", color = "grey10"),  
      strip.text.x = element_text(size = base_size*0.8, color = "white"),  
      strip.text.y = element_text(size = base_size*0.8, color = "white",angle = -90),  
      # Specify plot options
      plot.background = element_rect(color = "black", fill = "black"),  
      plot.title = element_text(size = base_size*1.2, color = "white"),  
      plot.margin = unit(rep(1, 4), "lines")
      
    )
  
}

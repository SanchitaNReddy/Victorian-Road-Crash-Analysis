## Importing libraries needed for this project
## Please uncomment the below if packages need to be installed
#install.packages("shiny")
#install.packages("ggplot2")
#install.packages("leaflet")
#install.packages("dplyr")
library(shiny)
library(ggplot2)
library(leaflet)
library(dplyr)

##DEFINING THE UI
ui <- fixedPage(
  #defining the title of the page
  div(style = "text-align: center;",
      h1("Accidents Trends in Eastern Victoria")
  ),
  
  #layout for static visualizations
  fluidRow(
    #Visualization 1 - stacked bar chart showing number of accidents at different light conditions and time zones
    column(
      width = 6,
      plotOutput("vis1", height = "400px"),
      p(HTML("<i>Image 1: A stacked bar chart for the number of accidents in different light conditions and speed zones.</i><br>
      Analyzing the accident data, we see a clear trend towards daytime and high-speed zones. The chart reveals that most accidents happen in higher speed zones and during daylight hours, followed by dusk and dawn. Regardless of light conditions, the 100 km/hr zone consistently has the highest number of accidents. It's particularly concerning that a significant number of accidents also occur in areas with no streetlights, highlighting the importance of proper nighttime illumination.
"), style = "font-size: 13px;")
    ),
    
    column(
      width = 6,
      plotOutput("vis2", height = "400px"),
      p(HTML("<i>Image 2: A line chart for number of accidents every hour in the top speed zones with the most accidents.</i><br>
The line chart focuses on the four-speed zones (100 km/hr, 80 km/hr, 60 km/hr, and 50 km/hr) that exhibited the highest frequency of accidents according to the previous chart. A significant spike in accidents between 7 am and 6 pm, coinciding with peak traffic hours suggests a strong correlation between the number of vehicles on the road and the likelihood of accidents. Furthermore, the chart highlights the role of speed zones - it's clear that accidents are consistently higher on lines representing higher speeds (purple line). This reinforces the importance of safe driving practices, especially during busy hours and while adhering to posted speed limits.
"), style = "font-size: 13px;")
    )
  ),
  
  # Display the UI for daynight filtering
  checkboxGroupInput("daynight_filter", label = "Filter by Day/Night",
                     choices = c("Day", "Dusk/Dawn", "Night"),
                     selected = c("Day", "Dusk/Dawn", "Night")),
  
  #MAP
  leafletOutput("map", height = "600px"),
  
  #MAP description
  p(HTML("<i>Image 3: A Victoria map showing the severity of accidents during different light conditions.</i><br>The traffic accident map reinforces the daytime trend we saw earlier. Most accidents cluster in eastern Victoria, particularly on Philip Island (along the strait near Western Port) during the day. This aligns with the high frequency of accidents in the 100 km/hr zone (red lines indicating highways) identified previously. Interestingly, the map reveals a shift in locations with high accident frequency based on time. Wonthaggi (south of the strait) sees more accidents at dawn and dusk, possibly due to lower visibility, while Cowes (in Philip Island) becomes the hotspot at night. This emphasizes the importance of proper lighting throughout Victoria. For daytime accidents, particularly in eastern Victoria, focusing on speed control measures in high-speed zones along the coast seems crucial.
"), style = "font-size: 13px;"),
  
  #Data Source
  p(HTML("Data Source: <a href='https://discover.data.vic.gov.au/dataset/victoria-road-crash-data'>Victoria Government, Australia - Road Crash Data</a>."))
)


##DEFINING THE SERVER
server <- function(input, output) {
  
  crash_data <- reactive({
    read.csv("Victoria_Accident_Data_FIT5147S12024PE2v2.csv")
  })
  
  #Visualization 1 - stacked bar chart showing number of accidents at different light conditions and time zones
  output$vis1 <- renderPlot({
    #subsetting the data for grouping
    vis1_data <- crash_data() %>%
      group_by(LIGHT_CONDITION_DESC, SPEED_ZONE) %>%
      summarise(NUMBER_OF_ACCIDENTS = n(), .groups = "drop")
    
    #plotting
    ggplot(vis1_data, aes(x = as.factor(SPEED_ZONE), y = NUMBER_OF_ACCIDENTS, fill = LIGHT_CONDITION_DESC)) +
      geom_bar(stat = "identity") + 
      coord_flip() +  # Creating horizontal bars
      scale_fill_brewer(palette = "Set3") + #choosing a professional color set
      labs(title = "Number of Accidents by Speed Zone and Light Conditions",
           x = "Speed Zones",
           y = "Number of Accidents",
           fill = "Lighting Conditions") +
      theme_minimal()
  })
  
  #Visualization 2 - No. of accidents that have occurred each hour 
  output$vis2 <- renderPlot({
    #using the data used for first visualization to identify top speed zones
    top_zones <- crash_data() %>%
      group_by(SPEED_ZONE) %>%
      summarise(NUMBER_OF_ACCIDENTS = n(), .groups = "drop") %>%
      arrange(desc(NUMBER_OF_ACCIDENTS)) %>%
      slice(1:4) %>%
      pull(SPEED_ZONE)
    
    #filtering data for only the top speed zones
    top_speed_zones <- crash_data() %>%
      filter(SPEED_ZONE %in% top_zones)
    
    #extracting hour data from the time column
    top_speed_zones$HOUR <- as.integer(format(strptime(top_speed_zones$ACCIDENT_TIME, "%H:%M"), "%H"))
    
    #grouping data by hour and speed zones
    vis2_data <- top_speed_zones %>%
      group_by(HOUR, SPEED_ZONE) %>%
      summarise(NUMBER_OF_ACCIDENTS = n(), .groups = "drop")
    
    #visualization 2 using ggplot
    ggplot(vis2_data, aes(x = HOUR, y = NUMBER_OF_ACCIDENTS, color = as.factor(SPEED_ZONE))) +
      geom_line() +
      labs( title = "Number of Accidents by the Hour for top four Speed Zones",
            x = "Hour of the Day",
            y = "Number of Accidents",
            color = "Speed Zones") +
      scale_x_continuous(breaks = unique(vis2_data$HOUR)) +
      theme_minimal()
  })
  
  #filtering data and grouping data based on the daylight attributes
  filtered_data <- reactive({
    daynight <- ifelse(crash_data()$LIGHT_CONDITION_DESC %in% c("Day"), "Day",
                       ifelse(crash_data()$LIGHT_CONDITION_DESC %in% c("Dusk/Dawn"), "Dusk/Dawn", "Night"))
    filter(crash_data(), daynight %in% input$daynight_filter)
  })
  
  #function to ensure the coloring is done based on the three attributes only
  color_mapping <- reactive({
    
    # Create a new daynight column for the map visualization
    daynight <- ifelse(filtered_data()$LIGHT_CONDITION_DESC %in% c("Day"), "Day",
                       ifelse(filtered_data()$LIGHT_CONDITION_DESC %in% c("Dusk/Dawn"), "Dusk/Dawn", "Night"))
    #daynight <- factor(daynight, levels = c("Day", "Dusk/Dawn", "Night"))
    #defining the colors based on the requirement
    color <- c("Day" = "yellow", "Dusk/Dawn" = "orange", "Night" = "darkblue")
    daynight <- ifelse(filtered_data()$LIGHT_CONDITION_DESC %in% c("Day"), "Day",
                       ifelse(filtered_data()$LIGHT_CONDITION_DESC %in% c("Dusk/Dawn"), "Dusk/Dawn", "Night"))
    color_vector <- leaflet::colorFactor(color, levels = c("Day", "Dusk/Dawn", "Night"))(daynight)
    color_vector
  })
  
  #function to map the radius based on severity_rank
  radius_mapping <- reactive({
    #overall radius of the circles
    max_radius <- 3
    #defining the minimum and maximum ranks based on the data for the radius
    min_rank <- min(filtered_data()$SEVERITY_RANK)
    max_rank <- max(filtered_data()$SEVERITY_RANK)
    
    # Scaling the severity rank values to a larger range
    scaled_radius <- scales::rescale(filtered_data()$SEVERITY_RANK, to = c(min_rank, max_rank))
    scaled_radius
  })
  
  #Adding description for all markers on the map (feature 1)
  formatted_labels <- function(data) {
    label <- paste(
      "Accident Date: ", data$ACCIDENT_DATE, ";",
      "Accident Description: ", data$ACCIDENT_TYPE_DESC, ",",
      "Light Condition: ", data$LIGHT_CONDITION_DESC, ";",
      "Road Geometry: ", data$ROAD_GEOMETRY_DESC, ";",
      "Speed Zone: ", data$SPEED_ZONE)
    return(label)
  }
  
  #plotting of the map
  output$map <- renderLeaflet({
    #creating the map
    leaflet(data = filtered_data()) %>%
      setView(lng = 145.465783, lat = -38.482461, zoom = 10) %>%
      addProviderTiles("CartoDB.Positron") %>% #map provider
      addCircleMarkers(~LONGITUDE, ~LATITUDE,
                       color = ~color_mapping(),
                       radius = ~radius_mapping(),
                       fillOpacity = 0.8,
                       label = formatted_labels(filtered_data())
      ) %>%
      addLegend("bottomright", colors = c("yellow", "orange", "darkblue"), 
                labels = c("Day", "Dusk/Dawn", "Night"), title = "Day/Night") %>%
      addTiles()
  })
}

##Calling the Shiny app function to link the UI and Server
shinyApp(ui = ui, server = server)
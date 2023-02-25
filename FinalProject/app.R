# Libraries
library(shiny)
library(shinythemes)
library(leaflet)
library(leaflet.extras)
library(sf)
library(shinyjs)
library(dplyr)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(plotly)
library(ggplot2)
library(hms)
library(DT)
library(stringr)
library(tools)
library(readr)
library(fontawesome)
library(geojsonR)

#library(tidyverse)


#Importing Housing Data

housing <- read.csv("miami_housing_data.csv") 

print(colnames(housing))

#Changing Column names 
colnames(housing) <- c("lat","long","parcel.no","sale.prc","lnd.sqft", "tot.lvg.area", "spec.feat.val", 
                       "rail.dist", "ocean.dist", "water.dist", "cntr.dist", "subcntr.di", "hw.dist", "age", 
                       "avno60plus", "month.sold", "struct.quality")

#class(housing$sale.prc) <-"Numeric"

housing$sale.prc <- as.numeric(gsub(",","",housing$sale.prc))

housing$price.range <- 0 

housing$price.range[housing$sale.prc < 100000] <- "Less than $100,000"
housing$price.range[housing$sale.prc >= 100000 & housing$sale.prc <= 250000] <- "$100,000 - $250,000"
housing$price.range[housing$sale.prc > 250000 & housing$sale.prc <= 5000000] <- "$250,001 - $500,000"
housing$price.range[housing$sale.prc > 500000 & housing$sale.prc <= 1000000] <- "$500,001 - $1,000,000"
housing$price.range[housing$sale.prc > 1000000] <- "More than $1,000,000"


housing$month.sold.name <- 0
housing$month.sold.name[housing$month.sold == 1] <- "January"
housing$month.sold.name[housing$month.sold == 2] <- "February"
housing$month.sold.name[housing$month.sold == 3] <- "March"
housing$month.sold.name[housing$month.sold == 4] <- "April"
housing$month.sold.name[housing$month.sold == 5] <- "May"
housing$month.sold.name[housing$month.sold == 6] <- "June"
housing$month.sold.name[housing$month.sold == 7] <- "July"
housing$month.sold.name[housing$month.sold == 8] <- "August"
housing$month.sold.name[housing$month.sold == 9] <- "September"
housing$month.sold.name[housing$month.sold == 10] <- "October"
housing$month.sold.name[housing$month.sold == 11] <- "November"
housing$month.sold.name[housing$month.sold == 12] <- "December"

housing$month.sold.name <- factor(housing$month.sold.name, 
                                  levels= rev(c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")))


housing$index <- seq_along(housing$sale.prc) 

#Importing Spacial Data

#Loading Neighborhoods 
municipality.load <- st_read("./Municipal_Boundary/Municipal_Boundary.shp")
municipality <- st_as_sf(municipality.load, coords = c("geometry"), crs = 4326)

#Joined datasets 
#housing_sf <- st_as_sf(housing, coords = c("long", "lat"), crs = 4326)
#joined <- st_join(housing_sf, municipality, join = st_intersects)



#Loading Trolley Routes
# trolley.load <- st_read("./Miami_Trolley_Routes.geojson") %>%
#   mutate(longitude = sf::st_coordinates(.)[,1],
#          latitude = sf::st_coordinates(.)[,2])

# trolley.cent <- trolley.load %>%
#   st_centroid() %>%
#   mutate(longitude = sf::st_coordinates(.)[,1],
#          latitude = sf::st_coordinates(.)[,2]) %>%
#   st_set_geometry(NULL)

# # Join centroid columns
# trolley.load <- ntrolley.load %>%
#   left_join(trolley.cent)


#Creating  Icons
#homeIcon <- makeAwesomeIcon(icon = "home", iconColor = 'white', library = "fa", markerColor = "darkblue")

homeIcon <- awesomeIconList(
  "Less than $100,000" = makeAwesomeIcon(icon = "home", library = "fa", markerColor = "red"),
  "$100,000 - $250,000" = makeAwesomeIcon(icon = "home", library = "fa", markerColor = "blue"),
  "$250,001 - $500,000" = makeAwesomeIcon(icon = "home", library = "fa", markerColor = "green"),
  "$500,001 - $1,000,000" = makeAwesomeIcon(icon = "home", library = "fa", markerColor = "orange"),
  "More than $1,000,000" = makeAwesomeIcon(icon = "home", library = "fa", markerColor = "yellow")
)


# Define UI for application that plots features -----------
ui <- navbarPage("Miami Housing Market 2016",
                 theme = shinytheme("spacelab"),
                 # Sidebar layout with a input and output definitions --------------
                 sidebarLayout(
                   # Inputs: Select variables to plot ------------------------------
                   sidebarPanel(
                    
                     
                     # # Select Miami Neighborhood
                     #  selectInput(inputId = "municipality",
                     #               label = "Select the municipality of the property for all graphs, map and table:",
                     #               choices = unique(sort(municipality.load$NAME)),
                     #               selected = "CORAL GABLES",
                     #               multiple = TRUE),
                     
                     # Set Age of the property ------------------------------------
                     sliderInput(inputId = "property.age",
                                 label = "Select age of the property for all graphs, map and table:",
                                 min = 0,
                                 max = 96,
                                 value = c(0, 10)),
                     
                     # Set Age of the property ------------------------------------
                     sliderInput(inputId = "property.ocean.dist",
                                 label = "Select properties ocean distance (ft) for all graphs, map and table:",
                                 min = 0, max = 67000,
                                 value = c(0, 5280)),
                     
                     
                     # Set Price Range of the property -----------------------------
                     checkboxGroupInput(inputId = "property.price",
                                        label = "Select price of the property for all graphs, map and table:",
                                        choices = c("Less than $100,000",
                                                    "$100,000 - $250,000",
                                                    "$250,001 - $500,000",
                                                    "$500,001 - $1,000,000",
                                                    "More than $1,000,000"),
                                        selected = c("Less than $100,000", "$100,000 - $250,000", "$250,001 - $500,000", "$500,001 - $1,000,000", "More than $1,000,000")),
                     
                     # Horizontal line for visual separation -----------------------
                     hr(),
                     
                     
                     # Show data table ---------------------------------------------
                     checkboxInput(inputId = "show_data",
                                   label = "Show data table",
                                   value = TRUE),
                     
                     
                     # Add Download Button
                     downloadButton("download.data", "Download"),
                     h6("Press the download button to save the dataset."),
                     
                   ),
                   
                   # Output --------------------------------------------------------
                   mainPanel(
                     # Tabs to separate each graph
                     tabsetPanel(
                       #Map Tab ----------------------------------------------------
                       tabPanel("Map", shinyjs::useShinyjs(),
                                #Style the background and change the page
                                tags$style(type = "text/css", ".leaflet {height: calc(100vh - 90px) !important;}
                                        body {background-color: white}"),
                                # Map Output
                                leafletOutput(outputId = "map")),
                       
                       
                       
                       # Plots Tab -------------------------------------------------
                       tabPanel("Plots",
                                h4("Market Analysis"),
                                plotlyOutput(outputId = "scatterplot"),
                                # Select variable for y-axis ----------------------------------
                                selectInput(inputId = "y",
                                            label = "Plots - Market Analysis Y-axis:",
                                            choices = c("Sale price" = "sale.prc",
                                                        "Land area (sqft)" = "lnd.sqft",
                                                        "Floor area (sqft)" = "tot.lvg.area",
                                                        "Value of special features" = "spec.feat.val",
                                                        "Distance to the ocean" = "ocean.dist",
                                                        "Distance to nearest water body" = "water.dist",
                                                        "Distance to business dist." = "cntr.dist",
                                                        "Distance to the highway" = "hw.dist"),
                                            selected = "sale.prc"),
                                
                                # Select variable for x-axis ----------------------------------
                                selectInput(inputId = "x",
                                            label = "Plots - Market Analysis X-axis:",
                                            choices = c("Sale price" = "sale.prc",
                                                        "Land area (sqft)" = "lnd.sqft",
                                                        "Floor area (sqft)" = "tot.lvg.area",
                                                        "Value of special features" = "spec.feat.val",
                                                        "Distance to the ocean" = "ocean.dist",
                                                        "Distance to nearest water body" = "water.dist",
                                                        "Distance to business dist." = "cntr.dist",
                                                        "Distance to the highway" = "hw.dist"),
                                            selected = "lnd.sqft"),
                                
                                #Bar Chart
                                hr(),
                                h4("Month Sold Distribution"),
                                plotlyOutput(outputId = "bar.chart"),
                                
                                #Pier Chart
                                hr(),
                                h4("Sale Price Distribution"),
                                plotlyOutput(outputId = "pie.chart")
                                ),
                     
                     
                     # Data table Tab ---------------------------------------------
                     tabPanel("Properties Table",
                              fluidPage(
                                wellPanel(DT::dataTableOutput(outputId = "table"))
                                
                              ))))))




# Define server function required to create the scatter plot ---------
server <- function(input, output) {

  # Data subset with reactive function for graphs 
  housing.subset <- reactive({
    req(input$property.age, input$property.price, input$property.ocean.dist)
    filter(housing, price.range %in% input$property.price & age >= input$property.age[1] & age <= input$property.age[2] & 
             ocean.dist >= input$property.ocean.dist[1] & ocean.dist <= input$property.ocean.dist[2])
  })
  
  # Create Map --------------------------------------------------------
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = "Google", group = "Google") %>%
      addProviderTiles(provider = providers$Wikimedia, group = "Wiki") %>%
      setView(-80.191788, 25.761681, 12) %>%
      addLayersControl(baseGroups = c("Google", "Wiki"))
  })


  # # PROPERTY LAYER - Adding Properties layers to map using lat and long  
  # observe({
  #   HouseInf <- housing.subset()
  #   layer1<- leafletProxy("map", data = HouseInf) %>%
  #     clearMarkers() %>%
  #     addAwesomeMarkers(data = HouseInf, ~long, ~lat, popup = paste0("Sale Price ($):", formatC(HouseInf$sale.prc, digits = 2, format = "d", big.mark = ","),
  #                                                                   "<br> Floor Area (sqft):", formatC(HouseInf$tot.lvg.area, digits = 0, format = "d",big.mark = ",")))
  # })
  
  
  # CLUSTERING LAYER
  observe({
    HouseInf <- housing.subset()
     leafletProxy("map", data =  HouseInf) %>%
      clearGroup(group = "HouseInf") %>%
      clearMarkerClusters() %>%
      addAwesomeMarkers(icon = ~homeIcon[price.range], 
                        clusterOptions = markerClusterOptions(), 
                        popup = paste0("Sale Price ($):", formatC(HouseInf$sale.prc, digits = 2, format = "d", big.mark = ","),
                                       "<br> Floor Area (sqft):", formatC(HouseInf$tot.lvg.area, digits = 0, format = "d",big.mark = ",")), 
                        group = "HouseInf", 
                        layerId = ~ index)
  })
  
  # # Borough Filter
  # priceInputs <- reactive({
  #   price <- subset(housing.subset, price.range == input$property.price)
  #   
  #   return(price)
  # })
  # 
  # 
  # observe({
  #   price <- priceInputs()
  #   
  #   leafletProxy("map", data = price) %>%
  #     clearGroup(group = "price") %>%
  #     addPolygons(popup = ~paste0("<b>", price.range, "</b>"), group = "price", layerId = ~sale.prc, fill = FALSE, color = "green") %>%
  #     setView(lng = price$long, lat = price$lat, zoom = 9)
  # })
  
  
  # # Add trolley routes to the map
  # observe({
  #   HouseInf <- housing.subset()
  #   trolley <- st_as_sf(trolley.load$geometry)
  #   #pal <- colorNumeric(palette = "Reds", domain = trolley.load$NAME)
  #   leafletProxy("map", data = trolley) %>%
  #       clearShapes() %>%
  #       addPolylines()
  #      })

  
  
  # # MIAMI MUNICIPALITIES - Adding Miami Municipalities to Map
  # observe({
  #   HouseInf <- housing.subset()
  #   leafletProxy("map", data = municipality.load) %>%
  #     clearGroup(group = "HouseInf") %>%
  #     addPolygons(popup = ~paste0("<b>", NAME, "</b>"), group = "Municipality", layerId = ~MUNICID, fill = FALSE, color = "gray")
  # })
  # 
  
  # Create Bar Chart -------------------------------------------------
  output$bar.chart <- renderPlotly({
    ggplotly(
      ggplot(data = housing.subset(), aes(x = month.sold.name)) +
        geom_bar(color = 'lightblue', fill = 'lightblue') +
        ggtitle("Number of properties sold per month in 2016") +
        xlab("Month of Sale") +
        ylab("Property Count") +
        theme_classic() +
        coord_flip() +
        geom_text(stat='count', aes(label=..count..), position = position_stack(vjust= 1.03)) + 
        theme(axis.title = element_text(color = "black", size = 15, face = "bold"),
              axis.title.y = element_text(face = "bold"))
    )
  })
  
  
  # Create scatter plot ----------------------------------------------
  output$scatterplot <- renderPlotly({
    ggplotly(
      ggplot(data = housing.subset(), aes_string(x = input$x, y = input$y)) +
        geom_point(color = "steelblue") +
        scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
        scale_x_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
        theme(axis.title = element_text(color = "black", size = 15, face = "bold"),
              axis.title.y = element_text(face = "bold")) + 
        labs(x = toTitleCase(str_replace_all(input$x, "\\.", " ")),
             y = toTitleCase(str_replace_all(input$y, "\\.", " "))
        ))  
  }
  )
  
  
  # # Create Pie Chart-------------------------------------------------
  
  output$pie.chart <- renderPlotly({
    
    # Plotting the pie chart using plot_ly() function
    pie <- plot_ly(housing.subset(), values =  ~sale.prc, labels = ~price.range,
                   type = "pie",
                   textposition = "outside", 
                   hovertemplate = "<b>%{label}</b><br>Percent of total: %{percent}<extra></extra>")
    
    return(pie)
    
  })
  
  
  # Print data table------------------------------------------------------
  output$table <- DT::renderDataTable(
    if(input$show_data){
      DT::datatable(data = housing.subset()[3:14], 
                    options = list(pageLength = 20, scrollX = TRUE), # add scrollX option
                    rownames = FALSE,
                    colnames = c('parcel no' = 'parcel.no', 'sale price' = 'sale.prc', 
                                 'land area' = 'lnd.sqft', 'floor area' = 'tot.lvg.area', 'special features value' = 'spec.feat.val', 
                                 'rail dist' = 'rail.dist', 'ocean dist' = 'ocean.dist', 'water dist' = 'water.dist', 'business center dist' = 'cntr.dist',
                                 'sub-center dist' = 'subcntr.di', 'highway dist' = 'hw.dist', 'property age' = 'age'))  %>% 
        formatCurrency('sale price', "$") %>% 
        formatCurrency(c('land area', 'floor area','special features value','rail dist','ocean dist','water dist', 
                         'business center dist', 'sub-center dist', 'highway dist'), "")%>% 
        formatRound(c('land area', 'floor area','special features value','rail dist','ocean dist','water dist', 
                      'business center dist', 'sub-center dist', 'highway dist'), 1)
    }
  )
  
  # Download data function------------------------------------------------
  output$download.data <- downloadHandler(
    filename = function() {
      paste("housing.miami", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(housing.subset(), file)
    }
  )
} 

# Run the application --------------------------------------------------
shinyApp(ui = ui, server = server)



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



#Importing Spacial Data

#city.load <- st_read("./City_Boundary/City_Boundary.shp")
#plot(cds.load)
#neighb.load <- st_read("./Miami_Neighborhoods/Miami_Neighborhoods_Shapefile.shp")
#trolley.load <- st_read("./Miami_Trolley_Routes/Miami_Trolley_Routes.shp")


# Define UI for application that plots features -----------
ui <- fluidPage(
  
  # Application title -----------------------------------------------
  titlePanel(title = "Miami Housing Market 2016"),
  
  
  # Horizontal line for visual separation -----------------------
  hr(),
  
  
  # Sidebar layout with a input and output definitions --------------
  sidebarLayout(
    # Inputs: Select variables to plot ------------------------------
    sidebarPanel(
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
               # Style the background and change the page
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
                           selected = "lnd.sqft")
      ), #tab for scatter plot
               
               hr(), 
               h4("Month Sold Distribution"), 
               plotlyOutput(outputId = "bar.chart"), 
               hr(), 
               h4("Sale Price Distribution"), 
               plotlyOutput(outputId = "pie.chart")),
      
      
      # Data table Tab ---------------------------------------------
      tabPanel("Properties Table", 
               fluidPage(
                 wellPanel(DT::dataTableOutput(outputId = "table"))
      
    )
  )
  )
  )
  )
  


# Define server function required to create the scatter plot ---------
server <- function(input, output) {
  
  housing.subset <- reactive({
    req(input$property.age, input$property.price, input$property.ocean.dist)
    filter(housing, price.range %in% input$property.price & age >= input$property.age[1] & age <= input$property.age[2] & 
             ocean.dist >= input$property.ocean.dist[1] & ocean.dist <= input$property.ocean.dist[2])
  })
  

  # Create Map --------------------------------------------------------
  #output$map <- renderLeaflet({
  #  leaflet() %>%
  #    addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = "Google", group = "Google") %>%
  #    addProviderTiles(provider = providers$Wikimedia, group = "Wiki") %>%
  #    setView(-74.0060, 40.7128, 3) %>%
  #    addLayersControl(baseGroups = c("Google", "Wiki"))
  #})
  
  
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
      DT::datatable(data = housing.subset()[0:14], 
                    options = list(pageLength = 20), 
                    rownames = FALSE,
                    colnames = c('latitude' = 'lat', 'longitude' = 'long', 'parcel no' = 'parcel.no', 'sale price' = 'sale.prc', 
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


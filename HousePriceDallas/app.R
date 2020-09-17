# package loading
library(dplyr)
library(ggplot2)
library(shiny)
library(leaflet)
library(data.table)
library(ggthemes)
library(scales)
theme_set(theme_minimal())

# data loading
df = read.csv("Housing_2020.csv")

# data proposing
column = c(
    "PROPERTY.TYPE", 
    "ZIP.OR.POSTAL.CODE",
    "PRICE",  
    "BEDS",  
    "BATHS",           
    "SQUARE.FEET",                
    "LOT.SIZE",                 
    "YEAR.BUILT",   
    "LATITUDE",                   
    "LONGITUDE")
df = df[, column]
df = na.omit(df)
df = df %>% arrange(ZIP.OR.POSTAL.CODE)

# ui.r
ui = fluidPage(
    
    titlePanel(title = "Housing Price from the Dallas Area"),
    h5("This is an interactive webpage to explore the housing price from the Dallas area."),
    h5("1. A distribution of home price shows in a zip code."),
    h5("2. A table shows the median home price by zip code and property type, especially in single family residential and townhouse."),
    h5("3. A boxplot shows for a combination of bed, bath, square feet, lot size, and year built."),
    h5("4. A map show the location of the home with its details."),
    
    tabsetPanel(
        
        # hist
        tabPanel(title = "Histogram",
                 sidebarLayout(
                     sidebarPanel(selectInput(inputId = "dropdown.hist",
                                              label = "Select of Zip Code:",
                                              choices = c(unique(df$ZIP.OR.POSTAL.CODE)),
                                              selected = "75082",
                                              multiple = T)),
                     mainPanel(plotOutput(outputId = "hist")))),
        
        # table
        tabPanel(title = "Table",
                 dataTableOutput(outputId = "table")),
        
        # boxplot
        tabPanel(title = "Boxplot",
                 sidebarLayout(
                     sidebarPanel(
                         sliderInput(inputId = "slider.bed",
                                     label = "Select of Bed Number:",
                                     min = min(df$BEDS),
                                     max = max(df$BEDS),
                                     value = median(df$BEDS),
                                     step = 1),
                         sliderInput(inputId = "slider.bath",
                                     label = "Select of Bath Number:",
                                     min = min(df$BATHS),
                                     max = max(df$BATHS),
                                     value = median(df$BATHS),
                                     step = 0.5),
                         sliderInput(inputId = "slider.sqft",
                                     label = "Select of Square Feet:",
                                     min = min(df$SQUARE.FEET),
                                     max = max(df$SQUARE.FEET),
                                     value = c(quantile(df$SQUARE.FEET, 0.25, names = F),
                                               quantile(df$SQUARE.FEET, 0.75, names = F)),
                                     step = 200),
                         sliderInput(inputId = "slider.lot",
                                     label = "Select of Lot Size:",
                                     min = min(df$LOT.SIZE),
                                     max = max(df$LOT.SIZE),
                                     value = c(quantile(df$LOT.SIZE, 0.25, names = F),
                                               quantile(df$LOT.SIZE, 0.75, names = F)),
                                     step = 50000),
                         sliderInput(inputId = "slider.year",
                                     label = "Select of Year Built:",
                                     min = min(df$YEAR.BUILT),
                                     max = max(df$YEAR.BUILT),
                                     value = c(quantile(df$YEAR.BUILT, 0.25, names = F),
                                               quantile(df$YEAR.BUILT, 0.75, names = F)),
                                     step = 1)),
                     mainPanel(plotOutput(outputId = "boxplot")))),
        
        # map
        tabPanel(title = "Map",
                 sidebarLayout(
                     sidebarPanel(selectInput(inputId = "dropdown.map",
                                              label = "Select of Zip Code:",
                                              choices = c(unique(df$ZIP.OR.POSTAL.CODE)),
                                              selected = "75082",
                                              multiple = T)),
                     mainPanel(leafletOutput(outputId = "map"))))
    )
)

# server.r
server = function(input, output) {
    
    # hist
    output$hist = renderPlot({
        df %>% 
            filter(ZIP.OR.POSTAL.CODE == input$dropdown.hist) %>% 
            select(PRICE) %>% 
            ggplot(aes(x = PRICE)) +
            geom_histogram() +
            labs(title = "Housing Price Distribution", x = "Price", y = "Count") +
            theme_minimal() +
            scale_x_continuous(labels = comma) +
            theme(plot.title = element_text(size = 22, face = "bold")) +
            theme(axis.text = element_text(size = 14), axis.title = element_text(size = 14))
    })
    
    # table
    output$table = renderDataTable({
        df
    })
    
    # boxplot
    output$boxplot = renderPlot({
        temp = df %>%
            filter(BEDS == input$slider.bed & BATHS == input$slider.bath) %>%
            filter(SQUARE.FEET >= input$slider.sqft[1] & SQUARE.FEET <= input$slider.sqft[2]) %>%
            filter(LOT.SIZE >= input$slider.lot[1] & LOT.SIZE <= input$slider.lot[2]) %>%
            filter(YEAR.BUILT >= input$slider.year[1] & YEAR.BUILT <= input$slider.year[2])
        validate(need((nrow(temp) != 0), "There is not a house in a such combination! Try it again."))
        temp %>% 
            ggplot(aes(x = "Price", y = PRICE)) +
            geom_boxplot() +
            labs(title = "Combination of Housing Price Boxplot", x = "Housing Price in Dallas", y = "") +
            theme_minimal() +
            scale_y_continuous(labels = comma) +
            theme(plot.title = element_text(size = 22, face = "bold")) +
            theme(axis.text = element_text(size = 14), axis.title = element_text(size = 14))
    })
    
    # map
    output$map = renderLeaflet({
        temp = df %>% filter(ZIP.OR.POSTAL.CODE == input$dropdown.map)
        temp %>% 
            leaflet() %>% 
            addTiles() %>% 
            addMarkers(lng = temp$LONGITUDE,
                       lat = temp$LATITUDE,
                       popup = paste("Property Type:", temp$PROPERTY.TYPE, "<br>",
                                     "Bed Number:", temp$BEDS, "<br>",
                                     "Bath Number:", temp$BATHS, "<br>",
                                     "Lot Size:", temp$LOT.SIZE, "<br>",
                                     "Square Feet:", temp$SQUARE.FEET, "<br>",
                                     "Year Built:", temp$YEAR.BUILT, "<br>",
                                     "Price:", temp$PRICE))
    })
}

# runapp 
shinyApp(ui = ui, server = server)

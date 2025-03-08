################## 
library(fda)
library(tidyverse)
library(sf)
library(tmap)
library(tmaptools)
library(leaflet)
library(randomForest)

load("rf_model.RData") # model results


# Data up to and including 2018-> 2019 is not clean
swedata2 <- read.csv("shinySWEdata.csv")

swedata2 <- swedata2 %>%  # Pick the site of interest
  arrange(site, year, dowy)

swedata2$region <- case_when(
  swedata2$Latitude >= 39 ~ "Northern Sierra",
  swedata2$Latitude < 39 & swedata2$Latitude >= 37 ~ "Central Sierra",
  swedata2$Latitude < 37 ~ "Southern Sierra"
)

data_filtered <- swedata2# %>% filter(  year <2019) 

pilllocinfo <- data_filtered %>% dplyr::select(site, Longitude, Latitude, Elevation, region)
pillloc <- distinct(pilllocinfo)



max_swe_data <- data_filtered %>%
  group_by(site, year) %>%
  summarize(max_swe = max(swe, na.rm = TRUE))%>% ungroup()


max_swe_data2 <- max_swe_data %>% left_join(pillloc, by = "site")

#print(str(max_swe_data2))

library(shiny)

addResourcePath("res", "www")


ui <- fluidPage(
  
 
  tags$head(
    tags$style(HTML("
        .nav-tabs > li > a {
        color: red !important;
      }
      .nav-tabs > li.active > a {
        color: green !important;  /* Change the color for the active tab */
      }
      body {
        background-image: url('res/a.png');
        background-size: cover;
        background-repeat: no-repeat;
        background-position: center center;
      }
    "))
  ),
  
  titlePanel(tags$span("Snowpack data and site map", style = "color: white;")),
  
  sidebarLayout(
    sidebarPanel(
      # Add the action buttons and output text 
      selectInput("region", "Select Region (1969-2018):", 
                  choices = c("Northern Sierra", "Central Sierra", "Southern Sierra"),
                  selected = NULL, multiple = FALSE),
      selectInput("site", "Select Site:", 
                  choices = unique(swedata2$site),
                  selected = NULL, multiple = FALSE),
      selectInput("year", "Select Year (1969-2018):", 
                  choices = unique(swedata2$year),
                  selected = NULL, multiple = FALSE),
      actionButton("go", "Generate Plot and Map"),
      actionButton("predict", "Predict Max SWE"),
      textOutput("predictedSWE") #, width = 5
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Overview",
                 fluidRow(
                   column(12, # overview of app
                          div(style = "background-color: darkblue; color: white; padding: 15px; border-radius: 5px;",
                              h3("App Overview", style = "font-size: 32px;"),
                              p("When one selects a region, site, and year, the app provides a regional summary 
           by computing quantiles (5%, 25%, 75%, 95%) across the days-of-water-year for all data points 
           in the selected region (filtered for years before WY 2019). These quantiles are visualized as 
           shaded ribbons in the plot, summarizing the general SWE behavior for the region. It also plots 
           a site-specific SWE profile for a chosen WY, allowing one to directly compare the site's 
           SWE profile against the regional distribution. This design enables one to see how the specific 
           site’s SWE profile (for that WY) compares with the overall regional pattern.  ", 
                                style = "font-size: 18px;")
                          )
                   )
                 )
        ),
        tabPanel("Plot", plotOutput("swePlot")),
        tabPanel("Map", leafletOutput("siteMap")),
        tabPanel("Histogram", plotOutput("sweHistogram")),  # tabPanel for the histogram
        tabPanel("Variable Importance", plotOutput("varImportance"))  # tabPanel for variable importance
      )#,   width = 3
    )
  )
)



server <- function(input, output) {
  
  observeEvent(input$go, {
    # Data filtering based on user input
    data_filtered <- swedata2 %>% 
      filter( region == input$region   & year <2019)
    
    # Plot
    output$swePlot <- renderPlot({
      # Generate the ggplot object based on data_filtered]
      quantiles_by_dowy <- data_filtered %>%
        group_by(dowy) %>%
        summarize(q05 = quantile(swe, probs = 0.05),
                  q25 = quantile(swe, probs = 0.25),
                  q75 = quantile(swe, probs = 0.75),
                  q95 = quantile(swe, probs = 0.95),
        )
      
      tidyquantiles_by_dowy <- quantiles_by_dowy %>%
        gather(key = "quantile", value = "sweqt", q05, q25, q75, q95)
      
      p<- ggplot(quantiles_by_dowy, aes(x = dowy)) +
        geom_ribbon(aes(ymin = q75, ymax = q95), fill = "blue", alpha=.25) +  # Shading the area between q25 and q75
        geom_ribbon(aes(ymin = q25, ymax = q75), fill = "grey80", alpha=.25) +  # Shading the area between q25 and q75
        geom_ribbon(aes(ymin = q05, ymax = q25), fill = "red", alpha=.25) +  # Shading the area between q25 and q75
        geom_line(aes(y = q05, color = "q05")) +  # Line for q25
        geom_line(aes(y = q25, color = "q25")) +  # Line for q25
        geom_line(aes(y = q75, color = "q75")) +  # Line for q75
        geom_line(aes(y = q95, color = "q95")) +  # Line for q75
        labs(color = "Quantile") +
        theme_minimal()
      
      data_specific_year_site <- swedata2 %>% 
        filter(year == input$year, site == input$site)
      
      
      p <- p + geom_line(data = data_specific_year_site, aes(y = swe), color = "black", size = 2.5)
      return(p)
      # Overlay the SWE measurements for the specific year and site with a thicker line
      # Return the ggplot object
    })
    
    # Map
    output$siteMap <- renderLeaflet({
      
      data_filtered <- swedata2 %>% 
        filter( region == input$region   & year <2019  )
      
      pilllocinfo <- data_filtered %>% dplyr::select(site, Longitude, Latitude, Elevation, region)
      pillloc <- distinct(pilllocinfo)
      swedata2_sf <- st_as_sf(pillloc, coords = c("Longitude", "Latitude"), crs = 4326)
      swedata2_sf$popup_content <- paste("Site:", swedata2_sf$site, "<br>", "Elevation:", swedata2_sf$Elevation)
      
      m <- leaflet(data = swedata2_sf) %>%
        addProviderTiles(providers$USGS.USTopo) %>%
        addCircleMarkers(popup = ~popup_content, color = "magenta", radius = 2)  # All sites
      
      # If a specific site is selected, filter the original data based on the selected site
      if (!is.null(input$site)) {
        data_specific_site <- swedata2 %>% 
          filter(site == input$site)
        
        data_specific_site_sf <- st_as_sf(data_specific_site, coords = c("Longitude", "Latitude"), crs = 4326)
        data_specific_site_sf$popup_content <- paste("Site:", data_specific_site_sf$site, "<br>", "Elevation:", data_specific_site_sf$Elevation)
        
        m <- m %>% addCircleMarkers(data = data_specific_site_sf, popup = ~popup_content, color = "blue", radius = 3)
      }
      
      return(m) 
    })
    
    # Histogram
    output$sweHistogram <- renderPlot({
      # Filter data based on the selected region
      data_region <- swedata2 %>% 
        filter(region == input$region, year <2019)
      pilllocinfo <- data_region %>% dplyr::select(site, Longitude, Latitude, Elevation, region)
      pillloc <- distinct(pilllocinfo)
      
      # Group by site and year, then get the max SWE for each site-year combo
      max_swe_data <- data_region %>%
        group_by(site, year) %>%
        summarize(max_swe = max(swe, na.rm = TRUE))
      max_swe_data2 <- max_swe_data %>% left_join(pillloc, by = "site")
      
      # Create the histogram
      ggplot(max_swe_data2, aes(x = max_swe)) +
        geom_histogram(  fill = "blue", color = "black", alpha = 0.7) +
        labs(title = "Histogram of Max SWE Measurements for the region",
             x = "Max SWE",
             y = "Count") +
        theme_minimal()
    })
    
    
  })
  observeEvent(input$predict, {
    aa=rf_model$terms
    tmpa =names(attr(aa,"dataClasses"))#[-1]

    max_swe_data2 <- mutate(max_swe_data2, region=as.factor(region))
    
    data_selected <- max_swe_data2 %>% 
      dplyr::filter(site == input$site, year == input$year) %>% dplyr::select(tmpa)
     
        # Predict max_swe for the selected data
    predicted_swe <- predict(rf_model, newdata = data_selected)
    
    # Display the prediction
    output$predictedSWE <- renderText({
      paste("Predicted max SWE for", input$site, " site in", input$year, ":", round(predicted_swe, 2))
    })
  })
  output$varImportance <- renderPlot({
    varImpPlot(rf_model, main="Variable Importance for SWE Random Forest Model")
    
  })
}


shinyApp(ui = ui, server = server)




library(shiny)
library(DT)
library(dplyr)
library(tidyr)
library(sf)
library(leaflet)
library(RColorBrewer)
library(leaflet.extras)
library(plotly)

ui <- fluidPage(
  # Add custom CSS
  tags$head(
    tags$style(HTML("
      /* Overall app styling */
      .container-fluid {
        padding: 20px;
        max-width: 1400px;
        margin: 0 auto;
      }
      
      /* Title styling */
      .title-panel {
        background-color: #f8f9fa;
        padding: 15px;
        border-radius: 8px;
        margin-bottom: 20px;
        border-left: 5px solid #007bff;
      }
      
      /* Sidebar styling */
      .well {
        background-color: #ffffff;
        border: 1px solid #e9ecef;
        border-radius: 8px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.05);
      }
      
      /* Tab styling */
      .nav-tabs > li > a {
        color: #495057;
        border-radius: 4px 4px 0 0;
      }
      
      .nav-tabs > li.active > a {
        background-color: #f8f9fa;
        border-bottom: 2px solid #007bff;
      }
      
      /* Stats box styling */
      .stats-box {
        background: linear-gradient(to right, #ffffff, #f8f9fa);
        border: 1px solid #e9ecef;
        border-radius: 8px;
        padding: 15px;
        margin: 10px 0;
        box-shadow: 0 2px 4px rgba(0,0,0,0.05);
      }
      
      /* Control inputs styling */
      .form-control {
        border-radius: 6px;
        border: 1px solid #ced4da;
      }
      
      /* Plot container styling */
      .plot-container {
        background-color: #ffffff;
        border: 1px solid #e9ecef;
        border-radius: 8px;
        padding: 15px;
        margin: 10px 0;
        box-shadow: 0 2px 4px rgba(0,0,0,0.05);
      }
    "))
  ),
  
  # Wrap title in styled div
  div(class = "title-panel",
      titlePanel("County Economic Report")
  ),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "year", "Select Year:",
                  choices = c(2019, 2020, 2021, 2022, 2023),
                  selected = 2023
      ),
      uiOutput("regionSelect"),
      
      # County selector for single county analysis
      conditionalPanel(
        condition = "input.tabset === 'County Analysis'",
        uiOutput("countySelect"),
        selectInput("countyIndicator", 
                    "Select Indicator:",
                    choices = c("GDP Contribution (%)" = "GDP Contribution (%)",
                                "Gross County Product (Millions)" = "Gross County Product (Millions)",
                                "Population" = "Population",
                                "Population Density" = "Population Density",
                                "GCP per Capita" = "GCP per Capita"),
                    selected = "GDP Contribution (%)")
      ),
      
      # Map controls only show on Map tab
      conditionalPanel(
        condition = "input.tabset === 'Map'",
        selectInput("mapIndicator", "Select Indicator for Map:",
                    choices = c("GDP Contribution (%)" = "gdpcont",
                                "Gross County Product (Millions)" = "gcp",
                                "Population" = "pop",
                                "Population Density" = "pop.density",
                                "GCP per Capita" = "gcp.pc"),
                    selected = "gdpcont")
      ),
      
      # Region stats always visible
      tags$div(class = "stats-box",
               uiOutput("regionStats")
      ),
      
      # Graph controls only show on Graphs tab
      conditionalPanel(
        condition = "input.tabset === 'Graphs'",
        checkboxGroupInput("graphIndicators", 
                           "Select Indicators for Comparison:",
                           choices = c("GDP Contribution (%)" = "GDP Contribution (%)",
                                       "Gross County Product (Millions)" = "Gross County Product (Millions)",
                                       "Population" = "Population",
                                       "Population Density" = "Population Density",
                                       "GCP per Capita" = "GCP per Capita"),
                           selected = c("GDP Contribution (%)", "GCP per Capita")),
        uiOutput("dynamicSlider")
      )
    ),
    
    mainPanel(
      tabsetPanel(id = "tabset",
                  tabPanel("About", 
                           value = "About",
                           fluidRow(
                             column(12,
                                    div(
                                      style = "padding: 20px;",
                                      h3("About This Application"),
                                      p("This interactive dashboard provides comprehensive economic analysis of Kenyan counties from 2019 to 2023. It offers various ways to explore and analyze county-level economic data:"),
                                      
                                      h4("Key Features:"),
                                      tags$ul(
                                        tags$li(strong("Interactive Map:"), 
                                                "Visualize county-level indicators through a color-coded map of Kenya. Hover over counties to see specific values."),
                                        tags$li(strong("Data Table:"), 
                                                "Access detailed county data in a searchable and downloadable table format."),
                                        tags$li(strong("Comparative Graphs:"), 
                                                "Compare multiple indicators across counties using bar charts and correlation plots."),
                                        tags$li(strong("County Analysis:"), 
                                                "Deep dive into individual county performance with historical trends and year-on-year changes.")
                                      ),
                                      
                                      h4("Available Indicators:"),
                                      tags$ul(
                                        tags$li(strong("GDP Contribution (%):"), 
                                                "County's percentage contribution to national GDP"),
                                        tags$li(strong("Gross County Product (Millions):"), 
                                                "Total economic output of the county in millions of KES"),
                                        tags$li(strong("Population:"), 
                                                "Total number of residents in the county"),
                                        tags$li(strong("Population Density:"), 
                                                "Number of residents per square kilometer"),
                                        tags$li(strong("GCP per Capita:"), 
                                                "Gross County Product divided by population")
                                      ),
                                      
                                      h4("How to Use:"),
                                      tags$ol(
                                        tags$li("Select a year of interest (2019-2023)"),
                                        tags$li("Choose a specific region or view all counties"),
                                        tags$li("Navigate between different tabs to explore various aspects of the data"),
                                        tags$li("Use the interactive features to filter, sort, and analyze the data")
                                      ),
                                      
                                      hr(),
                                      
                                      p(style = "font-style: italic;",
                                        "This dashboard is meant to be a simple showcase of the power of R Shiny dashboard in summarising data interactively.")
                                    )
                             )
                           )
                  ),
                  tabPanel("Map", 
                           value = "Map",
                           div(class = "plot-container",
                               leafletOutput("countyMap", height = "600px")
                           ),
                           uiOutput("legendTitle")
                  ),
                  tabPanel("Data Table", 
                           value = "Table",
                           DTOutput("countyTable")),
                  tabPanel("Graphs", 
                           value = "Graphs",
                           fluidRow(
                             column(12, 
                                    div(class = "plot-container",
                                        plotlyOutput("barChart", height = "400px")
                                    ),
                                    br(),
                                    div(class = "plot-container",
                                        plotlyOutput("scatterPlot", height = "400px")
                                    )
                             )
                           )
                  ),
                  tabPanel("County Analysis",
                           value = "County Analysis",
                           fluidRow(
                             column(12,
                                    div(class = "plot-container",
                                        h4("Historical Trend"),
                                        plotlyOutput("trendPlot", height = "300px")
                                    ),
                                    br(),
                                    div(class = "plot-container",
                                        h4("Year-on-Year Changes"),
                                        plotlyOutput("yoyPlot", height = "300px")
                                    ),
                                    br(),
                                    div(class = "stats-box",
                                        h4("County Statistics"),
                                        verbatimTextOutput("countyStats")
                                    )
                             )
                           )
                  )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Read and store the complete data with geometry
  kenya_sf_data <- reactive({
    req(input$year)
    
    # Read the original data with geometry
    df <- readRDS("C:/Users/HomePC/Desktop/Repository/Darubini_Final/Darubini_Redone/Kenya_Data.rds")
    
    # Create year-specific column names
    selected_cols <- c(
      "Region", "COUNTY",
      "land.kmsqr",
      paste0("pop.", input$year),
      paste0("pop.density.", input$year),
      paste0("gcp.", input$year),
      paste0("gcp.pc.", input$year),
      paste0("gdpcont.", input$year)
    )
    
    # Select columns while keeping geometry
    df <- df %>%
      select(all_of(selected_cols), geometry)
    
    # Rename columns
    names(df) <- c(
      "Region", "County",
      "Land Area (km²)",
      "Population",
      "Population Density",
      "Gross County Product (Millions)",
      "GCP per Capita",
      "GDP Contribution (%)",
      "geometry"
    )
    
    return(df)
  })
  
  # Table data (without geometry)
  kenya_data <- reactive({
    st_drop_geometry(kenya_sf_data())
  })
  
  # Region selection UI
  output$regionSelect <- renderUI({
    req(kenya_data())
    regions <- sort(unique(kenya_data()$Region))
    selectInput("region", "Select Region:",
                choices = c("All", regions),
                selected = "All")
  })
  
  # Filtered data for table
  filtered_data <- reactive({
    req(kenya_data(), input$region)
    
    if (input$region == "All") {
      return(kenya_data())
    } else {
      return(kenya_data() %>% filter(Region == input$region))
    }
  })
  
  # Add region statistics
  output$regionStats <- renderUI({
    req(kenya_data(), input$region)
    
    if (input$region == "All") {
      stats_data <- kenya_data()
    } else {
      stats_data <- kenya_data() %>% filter(Region == input$region)
    }
    
    # Calculate regional statistics
    total_pop <- sum(stats_data$Population)
    total_gcp <- sum(stats_data$`Gross County Product (Millions)`)
    avg_gcp_pc <- mean(stats_data$`GCP per Capita`)
    total_gdp_cont <- sum(stats_data$`GDP Contribution (%)`)
    
    # Create HTML for statistics box
    div(
      style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; margin-top: 15px;",
      h4(style = "margin-top: 0;", 
         if(input$region == "All") "Kenya Statistics:" else paste0(input$region, " Region Statistics:")),
      tags$ul(
        style = "list-style-type: none; padding-left: 0;",
        tags$li(sprintf("Total Population: %s", format(total_pop, big.mark = ","))),
        tags$li(sprintf("Total GCP: %.2f M", total_gcp)),
        tags$li(sprintf("Average GCP per Capita: %.2f", avg_gcp_pc)),
        tags$li(sprintf("GDP Contribution: %.2f%%", total_gdp_cont))
      )
    )
  })
  
  # Modify the map output
  output$countyMap <- renderLeaflet({
    req(kenya_sf_data(), input$mapIndicator)
    
    # Get the data for the selected indicator
    map_data <- kenya_sf_data()
    
    # Determine which column to use based on selected indicator
    value_col <- switch(input$mapIndicator,
                        "gdpcont" = "GDP Contribution (%)",
                        "gcp" = "Gross County Product (Millions)",
                        "pop" = "Population",
                        "pop.density" = "Population Density",
                        "gcp.pc" = "GCP per Capita")
    
    # Create color palette
    pal <- colorNumeric(
      palette = "YlOrRd",
      domain = map_data[[value_col]]
    )
    
    # Create the map
    leaflet(map_data) %>%
      addTiles() %>%  # Basic OpenStreetMap tiles
      addPolygons(
        fillColor = ~pal(get(value_col)),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = ~sprintf(
          "%s: %s",
          County,
          formatC(get(value_col), format = "f", digits = 2)
        )
      ) %>%
      addLegend(
        pal = pal,
        values = ~get(value_col),
        opacity = 0.7,
        title = NULL,
        position = "bottomright"
      )
  })
  
  # Create legend title
  output$legendTitle <- renderUI({
    req(input$mapIndicator)
    title <- switch(input$mapIndicator,
                    "gdpcont" = "GDP Contribution (%)",
                    "gcp" = "Gross County Product (Millions)",
                    "pop" = "Population",
                    "pop.density" = "Population Density",
                    "gcp.pc" = "GCP per Capita")
    h4(style = "text-align: center;", title)
  })
  
  # Render the data table
  output$countyTable <- renderDT({
    datatable(filtered_data(),
              options = list(
                pageLength = 10,
                scrollX = TRUE,
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
              ),
              extensions = 'Buttons',
              rownames = FALSE) %>%
      formatRound(columns = c("GDP Contribution (%)", 
                              "Gross County Product (Millions)",
                              "Population Density",
                              "GCP per Capita"), 
                  digits = 2)
  })
  
  # Dynamic slider that updates based on region selection
  output$dynamicSlider <- renderUI({
    req(filtered_data())
    
    # Get number of counties in current selection
    n_counties <- nrow(filtered_data())
    
    # Create slider with dynamic max value
    sliderInput("topN", 
                "Number of Counties to Display:",
                min = min(5, n_counties), 
                max = n_counties, 
                value = min(10, n_counties), 
                step = 1)
  })
  
  # Modify the bar chart to handle the dynamic nature of topN
  output$barChart <- renderPlotly({
    req(filtered_data(), input$graphIndicators, input$topN)
    
    # Get the data
    plot_data <- filtered_data()
    
    # Ensure topN doesn't exceed available counties
    actual_n <- min(input$topN, nrow(plot_data))
    
    # Create a bar chart for each selected indicator
    plot_list <- lapply(input$graphIndicators, function(indicator) {
      # Sort and get top N counties
      top_data <- plot_data[order(plot_data[[indicator]], decreasing = TRUE), ][1:actual_n, ]
      
      plot_ly(data = top_data,
              x = ~County,
              y = as.formula(paste0("~`", indicator, "`")),
              type = 'bar',
              marker = list(color = '#007bff'),
              name = indicator) %>%
        layout(
          title = list(
            text = paste("Top", actual_n, "Counties by", indicator),
            font = list(size = 16)
          ),
          xaxis = list(
            title = "County",
            tickangle = 45,
            gridcolor = '#f5f5f5'
          ),
          yaxis = list(
            title = indicator,
            gridcolor = '#f5f5f5'
          ),
          paper_bgcolor = 'rgba(0,0,0,0)',
          plot_bgcolor = 'rgba(0,0,0,0)',
          showlegend = TRUE
        )
    })
    
    # Combine all plots
    subplot(plot_list, nrows = length(plot_list), 
            shareX = TRUE, titleY = TRUE) %>%
      layout(height = actual_n * 50 + 200)  # Adjust height based on number of counties
  })
  
  # Scatter plot for correlation between two indicators
  output$scatterPlot <- renderPlotly({
    req(filtered_data(), input$graphIndicators)
    
    # Need at least 2 indicators selected
    if(length(input$graphIndicators) < 2) {
      return(NULL)
    }
    
    # Get the first two selected indicators
    indicator1 <- input$graphIndicators[1]
    indicator2 <- input$graphIndicators[2]
    
    plot_ly(data = filtered_data(),
            x = as.formula(paste0("~`", indicator1, "`")),
            y = as.formula(paste0("~`", indicator2, "`")),
            type = 'scatter',
            mode = 'markers+text',
            text = ~County,
            textposition = 'top',
            hoverinfo = 'text+x+y') %>%
      layout(title = paste("Correlation between", indicator1, "and", indicator2),
             xaxis = list(title = indicator1),
             yaxis = list(title = indicator2))
  })
  
  # County selection UI
  output$countySelect <- renderUI({
    req(kenya_data(), input$region)
    
    counties <- if (input$region == "All") {
      sort(unique(kenya_data()$County))
    } else {
      sort(kenya_data()$County[kenya_data()$Region == input$region])
    }
    
    selectInput("county", "Select County:",
                choices = counties,
                selected = counties[1])
  })
  
  # Function to get historical data for a county
  get_historical_data <- reactive({
    req(input$county)
    
    # Read the complete historical data
    df <- readRDS("C:/Users/HomePC/Desktop/Repository/Darubini_Final/Darubini_Redone/Kenya_Data.rds")
    
    # Filter for selected county
    county_data <- df[df$COUNTY == input$county, ]
    
    # Create mapping for column prefixes
    indicator_prefixes <- list(
      "GDP Contribution (%)" = "gdpcont",
      "Gross County Product (Millions)" = "gcp",
      "Population" = "pop",
      "Population Density" = "pop.density",
      "GCP per Capita" = "gcp.pc"
    )
    
    years <- 2019:2023
    
    # Initialize empty data frame
    historical <- data.frame(Year = years)
    
    # Add columns for each indicator
    for (ind_name in names(indicator_prefixes)) {
      prefix <- indicator_prefixes[[ind_name]]
      historical[[ind_name]] <- as.numeric(sapply(years, function(y) {
        col_name <- paste0(prefix, ".", y)
        county_data[[col_name]]
      }))
    }
    
    return(historical)
  })
  
  # Historical trend plot
  output$trendPlot <- renderPlotly({
    req(get_historical_data(), input$countyIndicator)
    
    hist_data <- get_historical_data()
    
    plot_ly(hist_data, 
            x = ~Year, 
            y = as.formula(paste0("~`", input$countyIndicator, "`")),
            type = 'scatter', 
            mode = 'lines+markers') %>%
      layout(title = paste("Historical Trend of", input$countyIndicator),
             yaxis = list(title = input$countyIndicator),
             showlegend = FALSE)
  })
  
  # Year-on-Year changes plot
  output$yoyPlot <- renderPlotly({
    req(get_historical_data(), input$countyIndicator)
    
    hist_data <- get_historical_data()
    
    # Ensure we're working with numeric data
    values <- as.numeric(hist_data[[input$countyIndicator]])
    
    # Calculate YoY changes
    yoy_data <- data.frame(
      Year = hist_data$Year[-1],
      Change = diff(values) / abs(values[-length(values)]) * 100
    )
    
    plot_ly(yoy_data, 
            x = ~Year, 
            y = ~Change,
            type = 'bar') %>%
      layout(title = paste("Year-on-Year % Change in", input$countyIndicator),
             yaxis = list(title = "Percentage Change (%)"),
             showlegend = FALSE)
  })
  
  # County statistics
  output$countyStats <- renderText({
    req(get_historical_data(), input$county, input$year)
    
    hist_data <- get_historical_data()
    selected_year <- as.numeric(input$year)
    prev_year <- selected_year - 1
    
    # Check if previous year exists in the data
    has_prev_year <- prev_year >= min(hist_data$Year)
    
    # Get current and previous year values for all indicators
    stats_text <- paste0(
      "Statistics for ", input$county, " County\n",
      "Selected Year: ", selected_year, "\n",
      if(has_prev_year) paste0("Compared to: ", prev_year, "\n") else "",
      "\nCurrent Values:\n"
    )
    
    # Add values for all indicators
    indicators <- names(hist_data)[-1]  # exclude Year column
    for (ind in indicators) {
      current_val <- as.numeric(hist_data[hist_data$Year == selected_year, ind])
      
      if(has_prev_year) {
        prev_val <- as.numeric(hist_data[hist_data$Year == prev_year, ind])
        pct_change <- (current_val - prev_val) / abs(prev_val) * 100
        
        stats_text <- paste0(
          stats_text,
          ind, ": ", format(current_val, big.mark = ",", digits = 2),
          " (", sprintf("%+.1f", pct_change), "% from previous year)\n"
        )
      } else {
        stats_text <- paste0(
          stats_text,
          ind, ": ", format(current_val, big.mark = ",", digits = 2),
          " (no previous year data)\n"
        )
      }
    }
    
    return(stats_text)
  })
}

shinyApp(ui = ui, server = server)
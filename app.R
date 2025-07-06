# Load required libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(leaflet)

# Load and clean data
exposure_data <- read_csv("indonesia_riverine_floods_people_exposed_rcp4.5_and_8p5_historical.csv")
colnames(exposure_data) <- make.names(colnames(exposure_data))  # makes safe column names

# Extract available RCP columns
rcp_columns <- grep("^rcp", colnames(exposure_data), value = TRUE)

# UI
ui <- fluidPage(
  
  # === Custom WRI Theme ===
  tags$head(
    tags$link(href = "https://fonts.googleapis.com/css2?family=Lato&display=swap", rel = "stylesheet"),
    tags$style(HTML("
      body {
        font-family: 'Lato', sans-serif;
        background-color: #F4F4F4;
        color: #2C2C2C;
      }
      h1, h2, h3, h4 {
        color: #003057;
      }
      .well {
        background-color: #FFFFFF;
        border-left: 5px solid #FDB913;
      }
      .selectize-input {
        border: 1px solid #003057;
      }
      .leaflet-container {
        background: #ffffff;
        border: 1px solid #ccc;
        border-radius: 6px;
        box-shadow: 0 0 5px rgba(0,0,0,0.1);
      }
    "))
  ),
  
  # === Title ===
  titlePanel(div(" Climate Risk Dashboard: Flood Exposure in Indonesia", 
                 style = "color:#003057; font-weight:700; font-size: 24px;")),
  
  # === Layout ===
  sidebarLayout(
    sidebarPanel(
      h4("Filters", style = "color:#003057"),
      selectInput("district", "Select District:",
                  choices = sort(unique(exposure_data$District_Name)),
                  selected = unique(exposure_data$District_Name)[1]),
      selectInput("scenario", "Select RCP Scenario:",
                  choices = rcp_columns,
                  selected = rcp_columns[1])
    ),
    
    mainPanel(
      div(style = "background-color: #ffffff; padding: 20px; border-radius: 8px; box-shadow: 0 0 5px rgba(0,0,0,0.05);",
          plotOutput("exposurePlot"),
          verbatimTextOutput("summaryText")
      ),
      hr(),
      
      h4("📘 Scenario Legend"),
      HTML("
        <ul>
          <li><b>What the Numbers Represent:</b> Each value reflects the estimated <b>number of people exposed</b> to riverine flooding in the selected district and scenario.</li>
          <li><b>Historical (1980):</b> Observed exposure based on past flood events and population data.</li>
          <li><b>RCP 4.5:</b> Stabilization scenario (emissions peak ~2040 then decline).</li>
          <li><b>RCP 8.5:</b> High-emission 'business-as-usual' scenario with continued fossil fuel use.</li>
          <li><b>Climate Models:</b>
            <ul>
              <li><b>ESM1M:</b> Canadian Earth System Model</li>
              <li><b>GEM1M:</b> Canadian Global Environmental Multiscale Model</li>
              <li><b>CM5A_LR:</b> IPSL Low-Resolution model (France)</li>
              <li><b>MIROC_ESM:</b> Japanese Earth System Model</li>
            </ul>
          </li>
        </ul>
      "),
      hr(),
      
      h4("🗺️ Interactive Flood Risk Map (Demo)"),
      p("This demo map highlights flood-prone cities. Future versions can show floodplains or district exposure polygons."),
      leafletOutput("floodMap", height = 500)
    )
  )
)

# Server
server <- function(input, output) {
  
  filtered_data <- reactive({
    exposure_data %>% filter(District_Name == input$district)
  })
  
  output$exposurePlot <- renderPlot({
    df <- filtered_data()
    if (nrow(df) == 0) return(NULL)
    
    selected_label <- input$scenario
    
    df <- df %>%
      select(
        historical = historical_1980,
        selected = !!sym(input$scenario)
      ) %>%
      pivot_longer(cols = everything(), names_to = "Scenario", values_to = "Exposed") %>%
      mutate(Scenario = case_when(
        Scenario == "historical" ~ "Historical (1980)",
        Scenario == "selected" ~ selected_label
      ))
    
    # Custom color palette
    custom_colors <- c(
      "Historical (1980)" = "#FDB913",
      selected_label = "#003057"
    )
    
    ggplot(df, aes(x = Scenario, y = Exposed, fill = Scenario)) +
      geom_bar(stat = "identity", width = 0.6) +
      scale_fill_manual(values = custom_colors) +
      labs(title = paste("Flood Exposure in", input$district),
           x = "Scenario", y = "People Exposed") +
      theme_minimal(base_family = "Lato") +
      theme(plot.title = element_text(face = "bold", size = 16))
  })
  
  output$summaryText <- renderText({
    df <- filtered_data()
    hist <- sum(df$historical_1980, na.rm = TRUE)
    rcp <- sum(df[[input$scenario]], na.rm = TRUE)
    
    paste0("In ", input$district, ":\n",
           "- Historical (1980): ", format(hist, big.mark = ","), " people\n",
           "- ", input$scenario, ": ", format(rcp, big.mark = ","), " people")
  })
  
  output$floodMap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = 120.0, lat = -2.5, zoom = 5) %>%
      addCircleMarkers(
        lng = c(106.8456, 112.7508, 98.6793),
        lat = c(-6.2088, -7.2575, 3.5952),
        label = c("Jakarta", "Surabaya", "Medan"),
        radius = 8,
        color = "#2c7fb8",
        fillOpacity = 0.7,
        popup = c(
          "Jakarta: One of the most flood-prone cities.",
          "Surabaya: Coastal urban exposure.",
          "Medan: Inland river flood risk."
        )
      )
  })
}

# Launch the app
shinyApp(ui = ui, server = server)

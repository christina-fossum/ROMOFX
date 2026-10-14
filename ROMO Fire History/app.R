library(shiny)
library(dplyr)
library(leaflet)
library(sf)
library(plotly)
library(DT)
library(lubridate)

# ---- Load your CSV ----

df <- read.csv("InFORM_FireOccurrence_Public_5898138240344715602.csv", stringsAsFactors = FALSE)
# filter just useful columns
df <- df %>% select(c(Calculated.Acres, Containment.Date.Time, Control.Date.Time, Incident.Size, Fire.Discovery.Date.Time, FireOutDateTime, Incident.Name, Initial.Latitude,
                      Initial.Longitude, Initial.Response.Date.Time, UniqueFireIdentifier, Initial.Response.Agencies, Fire.Cause, Fire.Cause.General, Fire.Cause.Specific,
                      AcresNPS, Calendar.Year, x, y))
df <- df %>% select(c(4:5, 7, 11, 13:15, 17:19))
df <- df %>% mutate(Fire.Cause = case_when(Fire.Cause == 1 ~ "Natural", Fire.Cause == 2 ~ "Treatment", Fire.Cause == 3 ~ "Human"))
df <- df %>% select(-c(6:7))
df <- df %>% filter(Fire.Cause %in% c("Natural", "Human"))

# Convert DateTime
df$DateTime <- as.POSIXct(df$Fire.Discovery.Date.Time, format = "%m/%d/%Y %I:%M:%S %p", tz = "UTC")

# Convert coordinates to lat/lon using sf
df_sf <- st_as_sf(df, coords = c("x", "y"), crs = 3857) %>% 
  st_transform(crs = 4326) # WGS84
coords <- st_coordinates(df_sf)
df$lon <- coords[,1]
df$lat <- coords[,2]

# Add convenient columns
df <- df %>% mutate(
  year = format(DateTime, "%Y"),
  cause = Fire.Cause,
  size = Incident.Size
)

# ---- UI ----
ui <- fluidPage(
  titlePanel("🔥 Rocky Mountain National Park Fire History Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("dates", "Select Date Range",
                     start = min(as.Date(df$DateTime)), end = max(as.Date(df$DateTime))),
      selectInput("cause", "Fire Cause",
                  choices = unique(df$cause), selected = unique(df$cause),
                  multiple = TRUE),
      sliderInput("size", "Fire Size (Acres)",
                  min = min(df$size), max = max(df$size),
                  value = c(min(df$size), max(df$size)))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Map", leafletOutput("map", height = 600)),
        tabPanel("Fires per Year", plotlyOutput("trend")),
        tabPanel("Acres Burned", plotlyOutput("acres_trend")),
        tabPanel("Data", DTOutput("table"))
      )
    )
  )
)

# ---- Server ----
server <- function(input, output) {
  
  filtered <- reactive({
    start <- as.POSIXct(input$dates[1])
    end <- as.POSIXct(input$dates[2]) + hours(23) + minutes(59) + seconds(59)
    
    df %>% filter(
      DateTime >= start,
      DateTime <= end,
      cause %in% input$cause,
      size >= input$size[1],
      size <= input$size[2]
    )
  })
  
  # ---- Map ----
  output$map <- renderLeaflet({
    leaflet(filtered()) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~lon,
        lat = ~lat,
        radius = ~log(size+1)*3,
        color = ~ifelse(cause=="Human","red","blue"),
        popup = ~paste0("<b>", Incident.Name, "</b><br>",
                        "Cause: ", cause, "<br>",
                        "Size: ", size, " acres<br>",
                        "Date: ", DateTime)
      )
  })
  
  # ---- Fires per Year by Cause ----
  output$trend <- renderPlotly({
    filtered() %>%
      group_by(cause) %>%
      count(year) %>% 
      plot_ly(
        x = ~year,
        y = ~n,
        color = ~cause,
        colors = c("Human" = "red", "Natural" = "blue"),
        type = "bar"
      ) %>%
      layout(
        title = "Fires per Year by Cause",
        xaxis = list(title = "Year"),
        yaxis = list(title = "Count"),
        barmode = "stack"
      )
  })
  
  # ---- Acres Burned per Year by Cause ----
  output$acres_trend <- renderPlotly({
    filtered() %>%
      group_by(year, cause) %>%
      summarise(total_acres = sum(size, na.rm = TRUE), .groups = "drop") %>%
      plot_ly(
        x = ~year,
        y = ~total_acres,
        color = ~cause,
        colors = c("Human" = "red", "Natural" = "blue"),
        type = "bar"
      ) %>%
      layout(
        title = "Total Acres Burned per Year by Cause",
        xaxis = list(title = "Year"),
        yaxis = list(title = "Total Acres"),
        barmode = "stack"
      )
  })
  
  # ---- Data Table ----
  output$table <- renderDT({
    datatable(filtered())
  })
}

# ---- Run App ----
shinyApp(ui, server)

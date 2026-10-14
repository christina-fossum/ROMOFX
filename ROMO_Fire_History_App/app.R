library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(sf)





# ---- Load data ----
df <- read.csv("InFORM_FireOccurrence_Public_5898138240344715602.csv",
               stringsAsFactors = FALSE)

nps <- st_read("nps shape/NPSboundary.shp") %>%
  filter(UNIT_CODE == "ROMO")


# ---- KEEP ONLY WHAT WE NEED (NO INDEX SELECTING) ----
df <- df %>% select(Calculated.Acres, Fire.Discovery.Date.Time, Incident.Name,UniqueFireIdentifier, Fire.Cause, Initial.Latitude, Initial.Longitude)

# ---- CLEAN DATA ----
df <- df %>%  mutate(Fire.Cause = case_when(Fire.Cause == 1 ~ "Natural", Fire.Cause == 3 ~ "Human", TRUE ~ NA_character_)) %>% filter(Fire.Cause %in% c("Natural", "Human"))

df$Initial.Latitude <- as.numeric(df$Initial.Latitude)
df$Initial.Longitude <- as.numeric(df$Initial.Longitude)

# ---- SAFE DATETIME PARSE ----
df$DateTime <- as.POSIXct(df$Fire.Discovery.Date.Time, format = "%m/%d/%Y %I:%M:%S %p", tz = "UTC")

df <- df %>% filter(!is.na(DateTime))

df$Year <- as.numeric(format(df$DateTime, "%Y"))

# ---- CONVERT FIRES TO SF ---- #
fires_sf <- st_as_sf(df, coords = c("Initial.Longitude","Initial.Latitude"), crs = 4269)
fires_sf <- st_filter(fires_sf, nps)


# ================= UI =================
ui <- fluidPage(
  
  titlePanel("Rocky Mountain National Park Fire History Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        "years",
        "Year Range:",
        min = min(df$Year),
        max = max(df$Year),
        value = c(min(df$Year), max(df$Year)),
        sep = ""
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Map", plotOutput("map")),
        tabPanel("Fires per Year", plotOutput("trend")),
        tabPanel("Ignition Types", plotOutput("ignitionBar")),
        tabPanel("Data", DTOutput("table"))
        
      )
    )
  )
)

# ================= SERVER =================
server <- function(input, output, session) {
  
  filtered <- reactive({
    df %>% 
      filter(Year >= input$years[1],
             Year <= input$years[2])
  })
  
  output$map <- renderPlot({
    
    req(input$years)
    
    fires_filtered <- fires_sf %>%
      dplyr::filter(Year >= input$years[1],
                    Year <= input$years[2])
    
    ggplot() +geom_sf(data = nps, fill = "#f5f3e7", color = "#333333") +
      geom_sf(data = fires_filtered,aes(color = Fire.Cause, size = Calculated.Acres),alpha = 0.7) +
      scale_color_manual(values = c("Human" = "darkred", "Natural" = "steelblue")) +
      scale_size_continuous(range = c(1, 4)) +theme_minimal() +
      labs(title = paste("Fire Locations:", input$years[1], "-", input$years[2]),color = "Ignition Type", size = "Acres")
  })
  
  output$trend <- renderPlot({
    
    df %>%
      group_by(Year, Fire.Cause) %>%
      summarise(n = n(), .groups = "drop") %>%
      
      ggplot(aes(x = Year, y = n, color = Fire.Cause,linetype = Fire.Cause)) +
      geom_line(linewidth = 1.3) + geom_point(size = 2) +
      scale_color_manual(values = c("Human" = "darkred","Natural" = "steelblue")) +
      scale_linetype_manual(values = c("Human" = "solid","Natural" = "dashed")) +
      theme_minimal() +labs(x = "Year",y = "Number of Fires",color = "Ignition Type",linetype = "Ignition Type")
  })
  
  output$ignitionBar <- renderPlot({
    df %>%
      group_by(Year, Fire.Cause) %>%
      summarise(n = n(), .groups = "drop") %>%
      
      ggplot(aes(Year, n, fill = Fire.Cause)) +geom_col() + theme_minimal()
  })
  
  output$table <- renderDT({
    filtered()
  })
  
}
shinyApp(ui, server)
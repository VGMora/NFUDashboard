#load packages
library(shiny)
library(bslib)
library(dplyr)
library(lubridate)
library(ggplot2)
library(readxl)
library(tidyverse)
library(patchwork) 
library(viridis)
library(wql)
library(scales)
library(DT)

# 1. DEFINE ORDERS (Do this globally)
station_order <- c("340","341", "342", "343", "344", "345", "346", "347", "348", "349")
month_order <- c("Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug")

#load & clean data
SLS <- read_excel("LFS CATCHcpue.xlsx", sheet = "SLS") %>% mutate(Survey_Type = "SLS")

mm20 <- read_excel("LFS CATCHcpue.xlsx", sheet = "20mmall") %>% mutate(Survey_Type = "20-mm")

SLS <- SLS%>%
  mutate(
    WaterYear = as.integer(WaterYear),
    Station = as.character(Station),
    CPUE    = as.numeric(CPUE),
    Survey  = as.integer(Survey),
    Catch = as.integer(Catch)
  )

mm20 <- mm20%>%
  mutate(
    WaterYear = as.integer(WaterYear),
    Station = as.character(Station),
    CPUE    = as.numeric(CPUE),
    Survey  = as.integer(Survey),
    Catch = as.integer(Catch)
  )

combdata <- bind_rows(SLS, mm20) %>%
  filter(Oldregionupdate %in% c("Napa River", "NR")) %>%
  mutate(
    Date = as.Date(Date),
    WaterYear = as.integer(WaterYear),
    # Ensure Month is a factor in  order
    Month = month(Date, label = TRUE, abbr = TRUE),
    Month = factor(Month, levels = month_order),
    # Ensure Station is a factor in your specific order
    Station = factor(as.character(Station), levels = station_order),
    CPUE = as.numeric(CPUE),
    Survey_Label = paste(Survey_Type, Survey)
  ) %>%
  # Chronological order for Survey Labels
  mutate(Survey_Label = fct_reorder(Survey_Label, Date, .fun = min))

# --- 3. FORCE EMPTY ROWS (To show skipped stations) ---
# This ensures every Year-Month-Station combination exists in the data
combdata <- combdata %>%
  complete(WaterYear, Station)



#UI (user interface)
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  titlePanel("Napa River Station CPUE: SLS & 20mm"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("checkGroup", 
                         label = h3("Select Water Years"), 
                         choices = sort(unique(combdata$WaterYear), decreasing = TRUE), 
                         selected = max(combdata$WaterYear, na.rm = TRUE),
                         inline = TRUE)
    ),
    
    mainPanel(
      # The heatmap can get very long with 30 years, height is dynamic in server
      plotOutput("heatmap", height = "auto") 
    )
  )
)

###Server
server <- function(input, output) {
  
  filtered_data <- reactive({
    req(input$checkGroup)
    combdata %>% filter(WaterYear %in% as.numeric(input$checkGroup))
  })
  
  output$heatmap <- renderPlot({
    ggplot(filtered_data(), aes(x = Survey_Label, y = Station, fill = CPUE)) +
      geom_tile(color = "white", size = 0.3) +
      # This creates the "Month" groupings at the bottom
      # switch = "x" moves the labels to the bottom
      facet_grid(WaterYear ~ Month, scales = "free_x", space = "free_x", switch = "x") +
      scale_fill_viridis_c(
        option = "magma", 
        name = "CPUE", 
        na.value = "grey95", # Color for skipped stations/surveys
        trans = "pseudo_log"
      ) +
      theme_minimal() +
      theme(
        # STATION LABELS (Y-AXIS): Bold and Big
        axis.text.y = element_text(face = "bold", size = 14, color = "black"),
        # MONTH LABELS (Bottom Labels): Look like your image
        strip.placement = "outside",
        strip.text.x = element_text(face = "bold", size = 12),
        strip.background = element_rect(fill = "#f0f0f0", color = "white"),
        # General formatting
        panel.spacing = unit(0.1, "lines"),
        axis.text.x = element_blank(), # Hide Survey Labels if you only want Month labels
        axis.title = element_blank(),
        panel.grid = element_blank(),
        # Make the WaterYear labels on the right bold too
        strip.text.y = element_text(face = "bold", size = 12, angle = 0)
      )
  }, height = function() {
    # Adjusts plot height based on years selected: 300px per year
    max(500, length(input$checkGroup) * 250)
  })
}

#command to start the app
shinyApp(ui = ui, server = server)

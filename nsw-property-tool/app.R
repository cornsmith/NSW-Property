# Setup -------------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(tidyverse)

source("./src/main.R")
suburb <- sort(unique(LV$SUBURB_NAME))


# UI ----------------------------------------------------------------------
ui <- dashboardPage(
  
  dashboardHeader(title = "NSW Property Tool"),
  
  dashboardSidebar(
    selectInput(inputId = "suburb",
                label = "Suburb",
                choices = suburb,
                selected = "SYDNEY"
    ),
    htmlOutput("street_selector")
  ),
  
  dashboardBody(
    plotOutput("distPlot"),
    tableOutput("propertyTable")
  )
  
)


# Server ------------------------------------------------------------------
server <- function(input, output) {
  
  output$street_selector <- renderUI({
    selectInput(inputId = "street",
                label = "Street",
                choices = sort(unique(LV$STREET_NAME[LV$SUBURB_NAME == input$suburb]))
    )
  })
  
  output$distPlot <- renderPlot({
    LV %>%
      filter(SUBURB_NAME == input$suburb) %>%
      filter(STREET_NAME == input$street) %>% 
      select(LAND_VALUE_1) %>%
      ggplot(aes(LAND_VALUE_1)) +
      geom_density()
  })
  
  output$propertyTable <- renderTable(
    LV %>%
      filter(SUBURB_NAME == input$suburb) %>%
      filter(STREET_NAME == input$street) %>% 
      arrange(as.integer(str_replace(HOUSE_NUMBER, pattern = "[A-z]", replacement = ""))) %>% 
      select(HOUSE_NUMBER, STREET_NAME, ZONE_CODE, AREA, LAND_VALUE_1)
  )
}

# App ---------------------------------------------------------------------
shinyApp(ui = ui, server = server)

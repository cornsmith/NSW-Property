# Setup -------------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(tidyverse)

source("./src/main.R")
suburb <- sort(unique(LV$SUBURB_NAME))
shortlist_suburbs <- c("SYDNEY", "SURRY HILLS")

# UI ----------------------------------------------------------------------
header <- dashboardHeader(title = "NSW Property Tool")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Street Analysis", tabName = "street", icon = icon("dashboard")),
    menuItem("Recent Sales", tabName = "recent", icon = icon("th"))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "street",
            fluidRow(
              box(
                width = 2,
                selectInput(inputId = "suburb",
                            label = "Suburb",
                            choices = suburb,
                            selected = "SYDNEY"
                ),
                htmlOutput("street_selector")
              ),
              box(
                title = "Land Value", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 5,
                tableOutput("propertyTable")
              ),
              box(
                title = "Sold", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 5,
                tableOutput("soldTable")
              )
            ),
            plotOutput("distPlot"),
            box(
              title = "Unsold", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = 5,
              tableOutput("unsoldTable")
            )
    ),
    tabItem(tabName = "recent",
            tableOutput("recentSalesTable")
    )
  )
)

ui <- dashboardPage(header, sidebar, body, skin = "black")


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
      select(HOUSE_NUMBER, ZONE_CODE, AREA, LAND_VALUE_1) %>% 
      mutate(Value = LAND_VALUE_1 / AREA)
  )
  
  output$soldTable <- renderTable(
    PSI %>%
      filter(Suburb_Name == input$suburb) %>%
      filter(Street_Name == input$street) %>% 
      arrange(desc(Contract_Date)) %>% 
      select(House_Number, Zone_Code, Area, Purchase_Price, Contract_Date) %>% 
      mutate(Contract_Date = format(Contract_Date, '%Y-%m-%d'))
  )
  
  output$recentSalesTable <- renderTable(
    PSI %>%   
      filter(Suburb_Name %in% shortlist_suburbs) %>%
      filter(lubridate::year(Contract_Date) >= 2020) %>% 
      filter(Unit_Number == "") %>% 
      arrange(desc(Download_Datetime)) %>% 
      select(House_Number, Street_Name, Suburb_Name, Purchase_Price, Area, Contract_Date, Zone_Code) %>% 
      mutate(Contract_Date = format(Contract_Date, '%Y-%m-%d'))
  )
  
  output$unsoldTable <- renderTable(
    LV %>% 
      filter(SUBURB_NAME == input$suburb) %>%
      filter(STREET_NAME == input$street) %>% 
      anti_join(PSI, by = c("PROPERTY_ID" = "Property_Id")) %>% 
      anti_join(PSI, by = c("SUBURB_NAME" = "Suburb_Name", "STREET_NAME" = "Street_Name", "HOUSE_NUMBER" = "House_Number")) %>% 
      arrange(SUBURB_NAME, STREET_NAME, as.integer(str_replace(HOUSE_NUMBER, pattern = "[A-z]", replacement = ""))) %>% 
      select(HOUSE_NUMBER, ZONE_CODE, AREA, LAND_VALUE_1)
  )  
}

# App ---------------------------------------------------------------------
shinyApp(ui = ui, server = server)

# Load required packages
library(shiny)
library(dplyr)
library(ggplot2)
library(DT)

# 1) Load dataset ---------------------------------------------------------
clinic_data <- read.csv("data/clinic_data.csv", stringsAsFactors = FALSE)
clinic_data$visit_date <- as.Date(clinic_data$visit_date)

# 2) UI -------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Clinic Blood Pressure Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "clinic",
        label   = "Clinic:",
        choices = c("All", sort(unique(clinic_data$clinic))),
        selected = "All"
      ),
      
      checkboxGroupInput(
        inputId = "sex",
        label   = "Sex:",
        choices = unique(clinic_data$sex),
        selected = unique(clinic_data$sex)
      ),
      
      sliderInput(
        inputId = "age_range",
        label   = "Age Range:",
        min  = min(clinic_data$age),
        max  = max(clinic_data$age),
        value = c(min(clinic_data$age), max(clinic_data$age))
      ),
      
      checkboxGroupInput(
        inputId = "smoker",
        label   = "Smoker:",
        choices = unique(clinic_data$smoker),
        selected = unique(clinic_data$smoker)
      ),
      
      dateRangeInput(
        inputId = "date_range",
        label   = "Visit Date Range:",
        start = min(clinic_data$visit_date),
        end   = max(clinic_data$visit_date)
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Table",   DTOutput("table")),
        tabPanel("BP Plot", plotOutput("bp_plot")),
        tabPanel("Summary", verbatimTextOutput("summary"))
      )
    )
  )
)

# 3) SERVER ---------------------------------------------------------------
server <- function(input, output, session) {
  
  # Filtered dataset (reactive)
  filtered_data <- reactive({
    df <- clinic_data
    
    if (input$clinic != "All") {
      df <- df %>% filter(clinic == input$clinic)
    }
    
    df <- df %>%
      filter(
        sex %in% input$sex,
        smoker %in% input$smoker,
        age >= input$age_range[1],
        age <= input$age_range[2],
        visit_date >= input$date_range[1],
        visit_date <= input$date_range[2]
      )
    
    df
  })
  
  # Table Output
  output$table <- renderDT({
    datatable(filtered_data())
  })
  
  output$bp_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = sbp)) +
      geom_histogram(
        bins = 25,
        fill = "grey70",
        color = "black"   # outline for bars
      ) +
      labs(
        title = "Systolic Blood Pressure Histogram",
        x = "SBP (mmHg)",
        y = "Count"
      ) +
      theme_minimal(base_size = 16)
  })
  
  
  # Summary Statistics
  output$summary <- renderPrint({
    filtered_data() %>%
      summarise(
        n        = n(),
        mean_age = mean(age),
        mean_sbp = mean(sbp),
        mean_dbp = mean(dbp),
        mean_bmi = mean(bmi)
      )
  })
}

# 4) Run the Shiny App ----------------------------------------------------
shinyApp(ui = ui, server = server)


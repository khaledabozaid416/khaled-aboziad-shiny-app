# app.R
# Clinic Blood Pressure Dashboard with colored tabs, icons, KPIs, pies, boxplots, and summary cards

library(shiny)
library(dplyr)
library(ggplot2)

# 1) Load dataset ---------------------------------------------------------
clinic_data <- read.csv("data/clinic_data.csv", stringsAsFactors = FALSE)
clinic_data$visit_date <- as.Date(clinic_data$visit_date)

# 2) UI -------------------------------------------------------------------
ui <- fluidPage(
  
  # CSS for KPI cards and TAB colors
  tags$head(
    tags$style(HTML("
      /* ---------- KPI / SUMMARY CARDS ---------- */
      .kpi-card {
        color: white;
        border-radius: 12px;
        padding: 16px;
        box-shadow: 0 2px 6px rgba(0,0,0,0.15);
        margin-bottom: 15px;
        text-align: center;
      }

      .kpi-icon {
        font-size: 30px;
        margin-bottom: 6px;
        opacity: 0.95;
      }

      .kpi-title {
        font-size: 13px;
        opacity: 0.85;
        margin-bottom: 4px;
        text-transform: uppercase;
        letter-spacing: 0.5px;
      }

      .kpi-value {
        font-size: 24px;
        font-weight: bold;
      }

      .kpi-blue   { background-color: #1e88e5; }
      .kpi-green  { background-color: #43a047; }
      .kpi-orange { background-color: #fb8c00; }
      .kpi-red    { background-color: #e53935; }
      .kpi-purple { background-color: #8e24aa; }
      .kpi-grey   { background-color: #455a64; }

      /* ---------- TABS BASE STYLE ---------- */
      .nav-tabs > li > a {
        font-weight: 500;
        border-radius: 4px 4px 0 0;
        border: 1px solid #ddd;
        margin-right: 3px;
      }

      .nav-tabs > li > a:hover {
        opacity: 0.9;
      }

      .tab-content {
        border-top: 1px solid #ddd;
        padding-top: 10px;
      }

      /* ---------- OVERVIEW TAB ---------- */
      .nav-tabs > li > a[data-value='overview_tab'] {
        background-color: #e3f2fd;
        color: #1e88e5;
      }
      .nav-tabs > li.active > a[data-value='overview_tab'],
      .nav-tabs > li.active > a[data-value='overview_tab']:focus,
      .nav-tabs > li.active > a[data-value='overview_tab']:hover {
        background-color: #1e88e5;
        color: #ffffff;
      }

      /* ---------- BP DISTRIBUTION TAB ---------- */
      .nav-tabs > li > a[data-value='bp_tab'] {
        background-color: #f3e5f5;
        color: #8e24aa;
      }
      .nav-tabs > li.active > a[data-value='bp_tab'],
      .nav-tabs > li.active > a[data-value='bp_tab']:focus,
      .nav-tabs > li.active > a[data-value='bp_tab']:hover {
        background-color: #8e24aa;
        color: #ffffff;
      }

      /* ---------- AGE vs SBP TAB ---------- */
      .nav-tabs > li > a[data-value='age_sbp_tab'] {
        background-color: #e8f5e9;
        color: #2e7d32;
      }
      .nav-tabs > li.active > a[data-value='age_sbp_tab'],
      .nav-tabs > li.active > a[data-value='age_sbp_tab']:focus,
      .nav-tabs > li.active > a[data-value='age_sbp_tab']:hover {
        background-color: #2e7d32;
        color: #ffffff;
      }

      /* ---------- SMOKERS TAB ---------- */
      .nav-tabs > li > a[data-value='smokers_tab'] {
        background-color: #fff3e0;
        color: #ef6c00;
      }
      .nav-tabs > li.active > a[data-value='smokers_tab'],
      .nav-tabs > li.active > a[data-value='smokers_tab']:focus,
      .nav-tabs > li.active > a[data-value='smokers_tab']:hover {
        background-color: #ef6c00;
        color: #ffffff;
      }

      /* ---------- CLINIC & SEX TAB ---------- */
      .nav-tabs > li > a[data-value='clinic_sex_tab'] {
        background-color: #e0f7fa;
        color: #00838f;
      }
      .nav-tabs > li.active > a[data-value='clinic_sex_tab'],
      .nav-tabs > li.active > a[data-value='clinic_sex_tab']:focus,
      .nav-tabs > li.active > a[data-value='clinic_sex_tab']:hover {
        background-color: #00838f;
        color: #ffffff;
      }

      /* ---------- SUMMARY TAB ---------- */
      .nav-tabs > li > a[data-value='summary_tab'] {
        background-color: #f5f5f5;
        color: #424242;
      }
      .nav-tabs > li.active > a[data-value='summary_tab'],
      .nav-tabs > li.active > a[data-value='summary_tab']:focus,
      .nav-tabs > li.active > a[data-value='summary_tab']:hover {
        background-color: #424242;
        color: #ffffff;
      }
    "))
  ),
  
  titlePanel("Clinic Blood Pressure Dashboard"),
  
  sidebarLayout(
    
    # -------- Sidebar filters -------------------------------------------
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
    
    # -------- Main panel ------------------------------------------------
    mainPanel(
      tabsetPanel(
        id = "main_tabs",
        
        # ================= Overview Tab =================
        tabPanel(
          title = tagList(icon("tachometer"), "Overview"),
          value = "overview_tab",
          br(),
          
          fluidRow(
            column(
              3,
              div(class = "kpi-card kpi-blue",
                  div(class = "kpi-icon", icon("users")),
                  div(class = "kpi-title", "Total Patients"),
                  div(class = "kpi-value", textOutput("kpi_n"))
              )
            ),
            column(
              3,
              div(class = "kpi-card kpi-green",
                  div(class = "kpi-icon", icon("heartbeat")),
                  div(class = "kpi-title", "Mean SBP (mmHg)"),
                  div(class = "kpi-value", textOutput("kpi_mean_sbp"))
              )
            ),
            column(
              3,
              div(class = "kpi-card kpi-orange",
                  div(class = "kpi-icon", icon("heartbeat")),
                  div(class = "kpi-title", "Mean DBP (mmHg)"),
                  div(class = "kpi-value", textOutput("kpi_mean_dbp"))
              )
            ),
            column(
              3,
              div(class = "kpi-card kpi-red",
                  div(class = "kpi-icon", icon("smoking")),
                  div(class = "kpi-title", "Smokers (%)"),
                  div(class = "kpi-value", textOutput("kpi_smoker_pct"))
              )
            )
          ),
          
          br(),
          fluidRow(
            column(
              6,
              h4("SBP Distribution"),
              plotOutput("overview_sbp")
            ),
            column(
              6,
              h4("DBP Distribution"),
              plotOutput("overview_dbp")
            )
          )
        ),
        
        # ================= BP Distribution Tab =================
        tabPanel(
          title = tagList(icon("bar-chart"), "BP Distribution"),
          value = "bp_tab",
          br(),
          plotOutput("bp_plot")
        ),
        
        # ================= Age vs SBP Tab =====================
        tabPanel(
          title = tagList(icon("line-chart"), "Age vs SBP"),
          value = "age_sbp_tab",
          br(),
          plotOutput("age_sbp_plot")
        ),
        
        # ================= Smokers Tab ========================
        tabPanel(
          title = tagList(icon("smoking"), "Smokers"),
          value = "smokers_tab",
          br(),
          fluidRow(
            column(
              6,
              h4("Smokers vs Non-smokers"),
              plotOutput("smoker_pie_overall")
            ),
            column(
              6,
              h4("Male vs Female among Smokers"),
              plotOutput("smoker_pie_sex")
            )
          )
        ),
        
        # ================= Clinic & Sex Comparison Tab ========
        tabPanel(
          title = tagList(icon("columns"), "Clinic & Sex Comparison"),
          value = "clinic_sex_tab",
          br(),
          fluidRow(
            column(
              6,
              h4("SBP by Clinic"),
              plotOutput("sbp_box_clinic")
            ),
            column(
              6,
              h4("SBP by Sex"),
              plotOutput("sbp_box_sex")
            )
          )
        ),
        
        # ================= Summary Tab (cards) =================
        tabPanel(
          title = tagList(icon("info-circle"), "Summary"),
          value = "summary_tab",
          br(),
          fluidRow(
            column(
              4,
              div(class = "kpi-card kpi-blue",
                  div(class = "kpi-icon", icon("users")),
                  div(class = "kpi-title", "Total Patients"),
                  div(class = "kpi-value", textOutput("sum_n"))
              )
            ),
            column(
              4,
              div(class = "kpi-card kpi-green",
                  div(class = "kpi-icon", icon("user")),
                  div(class = "kpi-title", "Mean Age (years)"),
                  div(class = "kpi-value", textOutput("sum_mean_age"))
              )
            ),
            column(
              4,
              div(class = "kpi-card kpi-purple",
                  div(class = "kpi-icon", icon("heartbeat")),
                  div(class = "kpi-title", "Mean SBP (mmHg)"),
                  div(class = "kpi-value", textOutput("sum_mean_sbp"))
              )
            )
          ),
          fluidRow(
            column(
              4,
              div(class = "kpi-card kpi-orange",
                  div(class = "kpi-icon", icon("heartbeat")),
                  div(class = "kpi-title", "Mean DBP (mmHg)"),
                  div(class = "kpi-value", textOutput("sum_mean_dbp"))
              )
            ),
            column(
              4,
              div(class = "kpi-card kpi-grey",
                  div(class = "kpi-icon", icon("balance-scale")),
                  div(class = "kpi-title", "Mean BMI"),
                  div(class = "kpi-value", textOutput("sum_mean_bmi"))
              )
            ),
            column(
              4,
              div(class = "kpi-card kpi-red",
                  div(class = "kpi-icon", icon("smoking")),
                  div(class = "kpi-title", "Smokers (%)"),
                  div(class = "kpi-value", textOutput("sum_smoker_pct"))
              )
            )
          )
        )
      )
    )
  )
)

# 3) SERVER ---------------------------------------------------------------
server <- function(input, output, session) {
  
  # -------- Filtered dataset (reactive) ---------------------------------
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
  
  # -------- Summary reactive (for KPIs + summary cards) -----------------
  summary_reactive <- reactive({
    df <- filtered_data()
    if (nrow(df) == 0) return(NULL)
    
    df %>%
      summarise(
        n          = n(),
        mean_age   = mean(age, na.rm = TRUE),
        mean_sbp   = mean(sbp, na.rm = TRUE),
        mean_dbp   = mean(dbp, na.rm = TRUE),
        mean_bmi   = mean(bmi, na.rm = TRUE),
        smoker_pct = mean(smoker == "Yes", na.rm = TRUE) * 100
      )
  })
  
  # -------- KPI Outputs (Overview cards) --------------------------------
  output$kpi_n <- renderText({
    s <- summary_reactive()
    if (is.null(s)) return("0")
    s$n
  })
  output$kpi_mean_sbp <- renderText({
    s <- summary_reactive()
    if (is.null(s)) return("-")
    round(s$mean_sbp, 1)
  })
  output$kpi_mean_dbp <- renderText({
    s <- summary_reactive()
    if (is.null(s)) return("-")
    round(s$mean_dbp, 1)
  })
  output$kpi_smoker_pct <- renderText({
    s <- summary_reactive()
    if (is.null(s)) return("0%")
    paste0(round(s$smoker_pct, 1), "%")
  })
  
  # -------- Summary Tab card outputs ------------------------------------
  output$sum_n <- renderText({
    s <- summary_reactive()
    if (is.null(s)) return("0")
    s$n
  })
  output$sum_mean_age <- renderText({
    s <- summary_reactive()
    if (is.null(s)) return("-")
    round(s$mean_age, 1)
  })
  output$sum_mean_sbp <- renderText({
    s <- summary_reactive()
    if (is.null(s)) return("-")
    round(s$mean_sbp, 1)
  })
  output$sum_mean_dbp <- renderText({
    s <- summary_reactive()
    if (is.null(s)) return("-")
    round(s$mean_dbp, 1)
  })
  output$sum_mean_bmi <- renderText({
    s <- summary_reactive()
    if (is.null(s)) return("-")
    round(s$mean_bmi, 1)
  })
  output$sum_smoker_pct <- renderText({
    s <- summary_reactive()
    if (is.null(s)) return("0%")
    paste0(round(s$smoker_pct, 1), "%")
  })
  
  # -------- Overview Plots (SBP & DBP) ----------------------------------
  output$overview_sbp <- renderPlot({
    df <- filtered_data()
    if (nrow(df) == 0) return(NULL)
    
    ggplot(df, aes(x = sbp)) +
      geom_histogram(bins = 25, fill = "grey70", color = "black") +
      labs(x = "SBP (mmHg)", y = "Count") +
      theme_minimal(base_size = 14)
  })
  
  output$overview_dbp <- renderPlot({
    df <- filtered_data()
    if (nrow(df) == 0) return(NULL)
    
    ggplot(df, aes(x = dbp)) +
      geom_histogram(bins = 25, fill = "grey70", color = "black") +
      labs(x = "DBP (mmHg)", y = "Count") +
      theme_minimal(base_size = 14)
  })
  
  # -------- BP Distribution Plot ----------------------------------------
  output$bp_plot <- renderPlot({
    df <- filtered_data()
    if (nrow(df) == 0) return(NULL)
    
    ggplot(df, aes(x = sbp)) +
      geom_histogram(
        bins = 25,
        fill = "grey70",
        color = "black"
      ) +
      labs(
        title = "Systolic Blood Pressure Distribution",
        x = "SBP (mmHg)",
        y = "Count"
      ) +
      theme_minimal(base_size = 16)
  })
  
  # -------- Age vs SBP Scatter Plot -------------------------------------
  output$age_sbp_plot <- renderPlot({
    df <- filtered_data()
    if (nrow(df) == 0) return(NULL)
    
    ggplot(df, aes(x = age, y = sbp, color = smoker)) +
      geom_point(alpha = 0.6, size = 2) +
      labs(
        title = "Age vs Systolic Blood Pressure",
        x = "Age (years)",
        y = "SBP (mmHg)",
        color = "Smoker"
      ) +
      theme_minimal(base_size = 14)
  })
  
  # -------- Smokers Pie Chart: overall Yes vs No ------------------------
  output$smoker_pie_overall <- renderPlot({
    df <- filtered_data()
    if (nrow(df) == 0) return(NULL)
    
    df_pie <- df %>%
      count(smoker) %>%
      mutate(
        percent = round(n / sum(n) * 100, 1),
        label   = paste0(percent, "%")
      )
    
    ggplot(df_pie, aes(x = "", y = n, fill = smoker)) +
      geom_col(width = 1, color = "white") +
      geom_text(
        aes(label = label),
        position = position_stack(vjust = 0.5),
        color = "white", size = 6, fontface = "bold"
      ) +
      coord_polar(theta = "y") +
      labs(
        fill  = "Smoker",
        title = "Proportion of Smokers"
      ) +
      theme_void(base_size = 14) +
      theme(legend.position = "bottom") +
      scale_fill_manual(
        values = c("No" = "#4caf50", "Yes" = "#e53935")
      )
  })
  
  # -------- Smokers Pie Chart: Male vs Female among smokers -------------
  output$smoker_pie_sex <- renderPlot({
    df <- filtered_data()
    if (nrow(df) == 0) return(NULL)
    
    df_smokers <- df %>%
      filter(smoker == "Yes") %>%
      count(sex)
    
    if (nrow(df_smokers) == 0) return(NULL)
    
    df_smokers <- df_smokers %>%
      mutate(
        percent = round(n / sum(n) * 100, 1),
        label   = paste0(percent, "%")
      )
    
    ggplot(df_smokers, aes(x = "", y = n, fill = sex)) +
      geom_col(width = 1, color = "white") +
      geom_text(
        aes(label = label),
        position = position_stack(vjust = 0.5),
        color = "white", size = 6, fontface = "bold"
      ) +
      coord_polar(theta = "y") +
      labs(
        fill  = "Sex",
        title = "Sex Distribution among Smokers"
      ) +
      theme_void(base_size = 14) +
      theme(legend.position = "bottom") +
      scale_fill_manual(
        values = c("Male" = "#1e88e5", "Female" = "#8e24aa")
      )
  })
  
  # -------- Boxplot: SBP by Clinic --------------------------------------
  output$sbp_box_clinic <- renderPlot({
    df <- filtered_data()
    if (nrow(df) == 0) return(NULL)
    
    ggplot(df, aes(x = clinic, y = sbp, fill = clinic)) +
      geom_boxplot() +
      labs(
        title = "SBP by Clinic",
        x = "Clinic",
        y = "SBP (mmHg)"
      ) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none")
  })
  
  # -------- Boxplot: SBP by Sex -----------------------------------------
  output$sbp_box_sex <- renderPlot({
    df <- filtered_data()
    if (nrow(df) == 0) return(NULL)
    
    ggplot(df, aes(x = sex, y = sbp, fill = sex)) +
      geom_boxplot() +
      labs(
        title = "SBP by Sex",
        x = "Sex",
        y = "SBP (mmHg)"
      ) +
      theme_minimal(base_size = 14) +
      scale_fill_manual(
        values = c("Male" = "#1e88e5", "Female" = "#8e24aa")
      )
  })
}

# 4) Run the Shiny App ----------------------------------------------------
shinyApp(ui = ui, server = server)

ui <- fluidPage(
  # Custom Style
  tags$head(
    tags$style(HTML("
      body {
        background-color: #f5f7fa;
        font-family: 'Microsoft YaHei', 'Segoe UI', sans-serif;
      }
      .well {
        background-color: #ffffff;
        border: 1px solid #d1d9e6;
        border-radius: 10px;
        box-shadow: 0 2px 10px rgba(0,0,0,0.08);
      }
      .main-header {
        background: linear-gradient(135deg, #2c3e50 0%, #3498db 100%);
        color: white;
        padding: 25px;
        margin-bottom: 25px;
        border-radius: 10px;
        text-align: center;
      }
      .metric-card {
        background: white;
        border-radius: 8px;
        padding: 15px;
        margin: 10px 0;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        border-left: 4px solid #3498db;
      }
      .industry-badge {
        display: inline-block;
        padding: 4px 12px;
        margin: 2px;
        background: #e3f2fd;
        border-radius: 15px;
        font-size: 12px;
        color: #1976d2;
      }
    "))
  ),

  titlePanel(
    div(class = "main-header",
        h1("📊 Industry Financial Metrics Analysis Platform"),
        h4("Multidimensional Financial Analysis Based on the Osiris Dataset")
    )
  ),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      wellPanel(
        h4("🔧 Analysis Settings"),

        # Select Industry
        selectizeInput(
          "industry_select",
          label = "Select Industry:",
          choices = NULL,
          multiple = TRUE,
          options = list(placeholder = 'Select or enter an industry...')
        ),

        # Select Year Range
        sliderInput(
          "year_range",
          label = "Year Range:",
          min = 2017,
          max = 2021,
          value = c(2017, 2021),
          step = 1,
          sep = ""
        ),

        # Select Financial Metrics
        selectInput(
          "metric_select",
          label = "Financial Metrics:",
          choices = c(
            "Operating Revenue" = "operating_income",
            "Net Debt" = "net_debt",
            "Current Assets" = "current_assets",
            "Current Ratio" = "current_ratio",
            "Debt to Asset Ratio" = "debt_to_assets",
            "Profit to Assets Ratio" = "profitability_ratio"
          ),
          selected = "operating_income"
        ),

        # Select Chart Type
        radioButtons(
          "chart_type",
          label = "Chart Type",
          choices = c(
            "Trend Chart" = "line",
            "Bar Chart" = "bar",
            "Area Chart" = "area"
          ),
          selected = "line"
        ),

        # Analysis Dimensions
        checkboxGroupInput(
          "analysis_options",
          label = "Analysis Options:",
          choices = c(
            "Show Industry Average" = "show_avg",
            "Show Growth Rate" = "show_growth",
            "Show Trendline" = "show_trend"
          ),
          selected = "show_trend"
        ),

        # Action Button
        actionButton("analyze_btn", "Run Analysis",
                     icon = icon("chart-line"),
                     class = "btn-primary"),
        actionButton("reset_btn", "Reset",
                     icon = icon("refresh"))
      ),

      # Metric Description
      div(class = "metric-card",
          h5("📈 Metric Description:"),
          tags$ul(
            tags$li(strong("Operating Revenue:"), "Revenue from principal business activities"),
            tags$li(strong("Net Debt:"), "Total debt minus cash and equivalents"),
            tags$li(strong("Current Assets:"), "Assets realizable within one year"),
            tags$li(strong("Current Ratio:"), "Current Assets / Current Liabilities, and is used to measure short-term solvency.")
          )
      )
    ),

    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel(
          "📊 Trend Analysis",
          fluidRow(
            column(12,
                   plotOutput("trend_plot", height = "400px"),
                   uiOutput("trend_insights")
            )
          ),
          fluidRow(
            column(6, plotOutput("comparison_plot", height = "300px")),
            column(6, plotOutput("distribution_plot", height = "300px"))
          )
        ),

        tabPanel(
          "📋 Data Details",
          fluidRow(
            column(12,
                   h4("Filtered Data Preview"),
                   dataTableOutput("data_table")
            )
          )
        ),

        tabPanel(
          "🔍  Industry Comparison",
          fluidRow(
            column(6, plotOutput("radar_plot", height = "400px")),
            column(6, plotOutput("bubble_plot", height = "400px"))
          )
        ),

        tabPanel(
          "📖  Analysis Guide",
          div(class = "metric-card",
              h3("📚 Metric Interpretation Guide"),

              h4("💰 Operating Revenue:"),
              p("Reflects the market presence and operational strength of a company's core business. Consistent growth often indicates a solid market position."),

              h4("🏦 Net Debt:"),
              p("A negative value indicates a net cash position, while a positive value signifies net debt. Industry context should be consulted for analysis."),

              h4("💼 Current Assets:"),
              p("Measures a company's short-term financial flexibility. A level that is too high may indicate inefficient utilization of assets."),

              h4("⚖️ Current Ratio:"),
              p("• >2: Adequate Liquidity", tags$br(),
                "1.5-2: Moderate Liquidity", tags$br(),
                "<1.5: Potential short-term liquidity strain."),

              h4("📈 Key Analysis Points:"),
              tags$ul(
                tags$li("Focus on the annual trend of the metric."),
                tags$li("Compare performance across different industries."),
                tags$li("Consider multiple metrics collectively for a comprehensive assessment."),
                tags$li("Watch for outliers and extreme cases.")
              )
          )
        )
      )
    )
  )
)

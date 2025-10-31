server <- function(input, output, session) {

  # Update Industries Dynamically
  observe({
    industries <- unique(FinancialAnalysis::financial_data$industry_label)
    updateSelectizeInput(
      session,
      "industry_select",
      choices = industries,
      selected = industries[1:3],
      server = TRUE
    )
  })

  # Responsive Data Filtering
  filtered_data <- reactive({
    req(input$analyze_btn)

    data <- FinancialAnalysis::financial_data

    # Industry Filter
    if (!is.null(input$industry_select) && length(input$industry_select) > 0) {
      data <- data %>%
        filter(industry_label %in% input$industry_select)
    }

    # Year Filter
    data <- data %>%
      filter(year >= input$year_range[1], year <= input$year_range[2])

    return(data)
  }) %>% bindEvent(input$analyze_btn)

  # Main Trend Chart
  output$trend_plot <- renderPlot({
    data <- filtered_data()
    if (nrow(data) == 0) return(create_empty_plot("Please select an industry and then click 'Run Analysis'"))

    metric <- input$metric_select
    plot_title <- switch(metric,
                         "operating_income" = "Operating Revenue Trend Analysis",
                         "net_debt" = "Net Debt Trend Analysis",
                         "current_assets" = "Current Assets Trend Analysis",
                         "current_ratio" = "Current Ratio Trend Analysis",
                         "debt_to_assets" = "Debt to Asset Ratio Trend Analysis",
                         "profitability_ratio" = "Asset Profitability Ratio Trend Analysis"
    )

    p <- ggplot(data, aes(x = year, y = .data[[metric]],
                          color = industry_label, group = industry_label)) +
      labs(title = plot_title, x = "Year", y = get_metric_label(metric),
           color = "Industry") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom")

    # Add a Layer that Matches the Chart Type
    if (input$chart_type == "line") {
      p <- p + geom_line(size = 1.5) + geom_point(size = 3)
    } else if (input$chart_type == "bar") {
      p <- p + geom_col(position = "dodge", alpha = 0.8)
    } else {
      p <- p + geom_area(alpha = 0.4) + geom_line(size = 1)
    }

    # Add Trendline
    if ("show_trend" %in% input$analysis_options) {
      p <- p + geom_smooth(method = "lm", se = FALSE, linetype = "dashed", size = 0.8)
    }

    return(p)
  })

  # Industry Comparison Chart
  output$comparison_plot <- renderPlot({
    data <- filtered_data()
    if (nrow(data) == 0) return(NULL)

    latest_data <- data %>% filter(year == max(year))

    ggplot(latest_data, aes(x = reorder(industry_label, .data[[input$metric_select]]),
                            y = .data[[input$metric_select]], fill = industry_label)) +
      geom_col(alpha = 0.8) +
      coord_flip() +
      labs(title = "Latest Year Industry Comparison", x = "", y = get_metric_label(input$metric_select)) +
      theme_minimal() +
      theme(legend.position = "none") +
      scale_fill_brewer(palette = "Set3")
  })

  # Distribution Chart
  output$distribution_plot <- renderPlot({
    data <- filtered_data()
    if (nrow(data) == 0) return(NULL)

    ggplot(data, aes(x = .data[[input$metric_select]], fill = industry_label)) +
      geom_density(alpha = 0.6) +
      labs(title = "Metric Distribution Density", x = get_metric_label(input$metric_select), y = "Density") +
      theme_minimal() +
      scale_fill_brewer(palette = "Set2")
  })

  # Radar Chart (Industry Comparison)
  output$radar_plot <- renderPlot({
    data <- filtered_data()
    if (nrow(data) == 0) return(NULL)

    # Data Normalized for the Radar Chart
    radar_data <- data %>%
      group_by(industry_label) %>%
      summarise(
        operating_income = mean(operating_income, na.rm = TRUE),
        current_ratio = mean(current_ratio, na.rm = TRUE),
        profitability_ratio = mean(profitability_ratio, na.rm = TRUE)
      ) %>%
      mutate(across(where(is.numeric), scales::rescale))

    ggplot(radar_data, aes(x = industry_chinese, y = operating_income, fill = industry_label)) +
      geom_col() +
      labs(title = "Multi-Dimensional Industry Comparison", x = "Industry", y = "Standardized Score") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })

  # Bubble Chart
  output$bubble_plot <- renderPlot({
    data <- filtered_data()
    if (nrow(data) == 0) return(NULL)

    latest_data <- data %>% filter(year == max(year))

    ggplot(latest_data, aes(x = net_debt, y = operating_income,
                            size = current_assets, color = industry_label)) +
      geom_point(alpha = 0.7) +
      labs(title = "Debt-Income-Asset Relationship Chart",
           x = "Net Debt", y = "Operating Revenue",
           size = "Current Assets", color = "Industry") +
      theme_minimal() +
      scale_color_brewer(palette = "Set1")
  })

  # Trend Insights
  output$trend_insights <- renderUI({
    data <- filtered_data()
    if (nrow(data) == 0) return(NULL)

    metric <- input$metric_select
    latest_year <- max(data$year)
    oldest_year <- min(data$year)

    insights <- data %>%
      group_by(industry_label) %>%
      summarise(
        growth = (.data[[metric]][year == latest_year] - .data[[metric]][year == oldest_year]) /
          .data[[metric]][year == oldest_year] * 100
      )

    top_growth <- insights %>% arrange(desc(growth)) %>% head(1)
    top_industry <- top_growth$industry_chinese
    growth_rate <- round(top_growth$growth, 1)

    div(
      class = "metric-card",
      h4("💡 Key Insights:"),
      p(strong("Growth-Leading Industries:"), top_industry,
        "from", oldest_year, "-", latest_year,
        get_metric_label(metric), ifelse(growth_rate > 0, "Growth", "Decrease"),
        abs(growth_rate), "%"),
      p(strong("Analytical Recommendations:"), get_analysis_suggestion(metric, growth_rate))
    )
  })

  # Data Table
  output$data_table <- renderDataTable({
    data <- filtered_data() %>%
      select(year, industry_label, operating_income, net_debt,
             current_assets, current_ratio, debt_to_assets)

    datatable(
      data,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bfrtip'
      ),
      colnames = c(
        'Year', 'Industry', 'Operating Revenue', 'Net Debt',
        'Current Assets', 'Current Ratio', 'Debt to Asset Ratio'
      )
    ) %>%
      formatRound(columns = c(3:7), digits = 2)
  })

  # Reset Button
  observeEvent(input$reset_btn, {
    updateSelectizeInput(session, "industry_select", selected = character(0))
    updateSliderInput(session, "year_range", value = c(2017, 2021))
    updateSelectInput(session, "metric_select", selected = "operating_income")
    updateRadioButtons(session, "chart_type", selected = "line")
  })
}

# Helper Function
create_empty_plot <- function(message) {
  ggplot() +
    annotate("text", x = 1, y = 1, label = message, size = 6) +
    theme_void()
}

get_metric_label <- function(metric) {
  switch(metric,
         "operating_income" = "Operating Revenue",
         "net_debt" = "Net Debt",
         "current_assets" = "Current Assets",
         "current_ratio" = "Current Ratio",
         "debt_to_assets" = "Debt to Asset Ratio",
         "profitability_ratio" = "Asset Profitability Ratio"
  )
}

get_analysis_suggestion <- function(metric, growth_rate) {
  suggestions <- list(
    operating_income = ifelse(growth_rate > 0,
                              "With the industry performing well, watch the leading companies that stand to benefit the most.",
                              "The industry faces headwinds; investment caution is advised."),
    net_debt = ifelse(growth_rate > 0,
                      "Rising debt levels warrant caution regarding financial risks.",
                      "The financial condition has improved, and debt-servicing capacity has strengthened."),
    current_ratio = ifelse(growth_rate > 0,
                           "Liquidity has improved, and short-term risk has been reduced.",
                           "Liquidity is tightening; closely monitor cash flow.")
  )

  return(suggestions[[metric]] %||% "Consider this metric alongside others for a comprehensive view.")
}

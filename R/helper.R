#' Get Financial Metric Label
#'
#' @param metric Character, metric code name
#'
#' @return Character string with formatted metric name
#'
#' @noRd
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

#' Create Empty Plot
#'
#' Creates an empty plot with a prompt message when there is no data to display.
#'
#' @param message Character, message to display in the plot
#'
#' @return ggplot2 object
#'
#' @noRd
create_empty_plot <- function(message) {
  ggplot() +
    annotate("text", x = 1, y = 1, label = message, size = 6) +
    theme_void()
}


#' Generate Analysis Suggestions
#'
#' Generates corresponding analysis suggestions based on financial metrics and growth rates.
#'
#' @param metric Character, financial metric name
#' @param growth_rate Numeric, growth rate percentage
#'
#' @return Character, analysis suggestion text
#'
#' @noRd
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
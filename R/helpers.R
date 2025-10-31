#' Create empty plot with message
#' @noRd
create_empty_plot <- function(message) {
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 1, y = 1, label = message, size = 6) +
    ggplot2::theme_void()
}

#' Get metric label
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

#' Get analysis suggestion
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

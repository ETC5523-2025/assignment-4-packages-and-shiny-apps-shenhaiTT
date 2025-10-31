#' HongyuFinancialAnalysis: Interactive Industry Financial Analysis Platform
#'
#' Provides interactive industry financial analysis based on the Osiris dataset.
#' Includes features such as trend analysis, cross-industry comparison,
#' multidimensional visualization, and supports analysis of key financial metrics
#' including operating revenue, net debt, current assets, current ratio, etc.
#'
#' @section Key Features:
#' \itemize{
#'   \item Interactive financial metrics trend analysis
#'   \item Cross-industry financial performance comparison
#'   \item Multidimensional data visualization
#'   \item Intelligent insights and suggestion generation
#' }
#'
#' @section Datasets:
#' \describe{
#'   \item{\code{financial_data}}{Industry financial metrics dataset}
#'   \item{\code{industry_metadata}}{Industry metadata lookup table}
#' }
#'
#' @section Main Functions:
#' \code{\link{launch_app}} - Launch interactive analysis platform
#'
#' @references
#' Data source: Osiris Global Public Company Database
#'
#' @examples
#' \dontrun{
#' # Launch analysis application
#' launch_app()
#'
#' # View dataset
#' data(financial_data)
#' head(financial_data)
#' }
#'
#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage
# namespace imports. Modify with care!
## usethis namespace: start
#' @import shiny
#' @import ggplot2
#' @import dplyr
#' @importFrom DT datatable renderDT DTOutput formatRound
#' @importFrom tidyr gather spread
#' @importFrom scales rescale
## usethis namespace: end
NULL

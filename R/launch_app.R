#' Launch Industry Financial Analysis Platform
#'
#' Launch an interactive Shiny application for analyzing financial indicator trends and comparisons across various industries.
#' The application includes features such as trend analysis, industry benchmarking, and multidimensional visualization.
#'
#' @return  invisibly returns NULL. The function is called for its side effect
#'          of launching a Shiny application.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Launch Financial Analysis App
#' launch_app()
#' }
#'
#' @seealso
#' For detailed information on the dataset, please refer to \code{\link{financial_data}}
#'
#' @references
#' Data source: Osiris database
launch_app <- function() {
  appDir <- system.file("app", package = "FinancialAnalysis")
  if (appDir == "") {
    stop("Could not find app directory. Try re-installing 'FinancialAnalysis'.",
         call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}

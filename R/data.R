#' Industry Financial Metrics Dataset
#'
#' This dataset contains key financial metrics for multiple industries
#' during the 2017-2021 period, sourced from the Osiris database.
#'
#' @format A data frame with 45 rows and 9 columns:
#' \describe{
#' \item{year}{Integer, representing the year (2017-2021)}
#' \item{industry}{Character, industry code abbreviation}
#' \item{operating_income}{Numeric, average operating income}
#' \item{net_debt}{Numeric, average net debt}
#' \item{current_assets}{Numeric, average current assets}
#' \item{current_ratio}{Numeric, average current ratio}
#' \item{debt_to_assets}{Numeric, debt-to-assets ratio (calculated field)}
#' \item{profitability_ratio}{Numeric, profitability-to-assets ratio (calculated field)}
#' \item{industry_label}{Character, industry name}
#' }
#'
#' @source Osiris Global Public Company Database
#'
#' @examples
#' # Load the dataset
#' data(financial_data)
#'
#' # View data structure
#' str(financial_data)
#'
#' # View first few rows of data
#' head(financial_data)
#'
#' @keywords datasets
"financial_data"

#' Industry Metadata
#'
#' Contains a lookup table between industry codes and Chinese names,
#' along with detailed descriptions of each industry.
#'
#' @format A data frame with 9 rows and 3 columns:
#' \describe{
#' \item{code}{Character, industry code abbreviation}
#' \item{name_label}{Character, industry name}
#' \item{description}{Character, detailed industry description}
#' }
#'
#' @examples
#' # View industry metadata
#' data(industry_metadata)
#' print(industry_metadata)
#'
#' @keywords datasets
"industry_metadata"
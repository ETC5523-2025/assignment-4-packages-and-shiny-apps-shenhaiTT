# Creating a financial data analysis dataset for industries.
library(dplyr)
library(readr)

# Read raw data
financial_data <- read_csv("osiris_Bill_grouped.csv") %>%
  select(-1) %>%
  rename(
    year = year,
    industry = industry_macro_short,
    operating_income = avg_operating_income_ada,
    net_debt = avg_net_debt,
    current_assets = avg_total_current_assets,
    current_ratio = avg_current_ratio
  ) %>%
  mutate(
    industry = factor(industry),
    # Calculate some derived metrics
    debt_to_assets = net_debt / current_assets,
    profitability_ratio = operating_income / current_assets
  )

# Add industry labels
industry_labels <- c(
  "BAS" = "Basic Materials",
  "CSD" = "Consumer Services",
  "CST" = "Consumer Products",
  "ENE" = "Energy",
  "FIN" = "Financials",
  "HEA" = "Healthcare",
  "IND" = "Industrials",
  "REA" = "Real Estate",
  "TEC" = "Technology"
)

financial_data <- financial_data %>%
  mutate(industry_label = industry_labels[industry])

# Save to the package
usethis::use_data(financial_data, overwrite = TRUE)

# Create industry metadata
industry_metadata <- data.frame(
  code = names(industry_labels),
  name_label = industry_labels,
  description = c(
    "Basic Materials" = "Chemical, metal, building materials, and other raw material industries",
    "Consumer Services" = "Retail, tourism, catering, and other consumer services",
    "Consumer Products" = "Food, household appliances, daily necessities, and other consumer goods manufacturing",
    "Energy" = "Petroleum, natural gas, electricity, and other energy industries",
    "Financials" = "Banks, insurance, securities, and other financial institutions",
    "Healthcare" = "Pharmaceuticals, medical devices, and medical services",
    "Industrials" = "Machinery, electronics, transportation equipment manufacturing",
    "Real Estate" = "Real estate development, property management",
    "Technology" = "Software, hardware, IT services"
  )
)

usethis::use_data(industry_metadata, overwrite = TRUE)

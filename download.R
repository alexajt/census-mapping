library(dplyr)
library(purrr)
library(readr)
library(tidycensus)

# ACS 5-year vintages overlap; year is the ending year, not a single-year estimate.
# Monetary estimates are not adjusted here to a common inflation base.
years <- 2009:2024
education_variables <- sprintf("B15003_%03d", 1:25)
age_variables <- sprintf("B01001_%03d", c(1, 3:25, 27:49))
variables <- c(
  "B01003_001",
  "B19013_001",
  "B17001_001",
  "B17001_002",
  education_variables,
  "B25064_001",
  "B25077_001",
  "B25003_001",
  "B25003_002",
  age_variables
)

sum_estimates <- \(data, variables) {
  rowSums(data[paste0(variables, "E")], na.rm = TRUE)
}

summarise_acs <- \(data, year) {
  under_18 <- sum_estimates(data, paste0(
    "B01001_", sprintf("%03d", c(3:6, 27:30))
  ))
  age_65_plus <- sum_estimates(data, paste0(
    "B01001_", sprintf("%03d", c(20:25, 44:49))
  ))
  education_available <- year >= 2012

  data |>
    transmute(
      GEOID,
      county = NAME,
      year,
      population = B01003_001E,
      population_moe = B01003_001M,
      median_household_income = B19013_001E,
      median_household_income_moe = B19013_001M,
      below_poverty = B17001_002E,
      below_poverty_moe = B17001_002M,
      poverty_rate_percent = 100 * B17001_002E / B17001_001E,
      under_18 = under_18,
      under_18_percent = 100 * under_18 / B01003_001E,
      age_18_to_64 = B01003_001E - under_18 - age_65_plus,
      age_18_to_64_percent = 100 * age_18_to_64 / B01003_001E,
      age_65_plus = age_65_plus,
      age_65_plus_percent = 100 * age_65_plus / B01003_001E,
      high_school_or_higher_percent = if (education_available) {
        100 * sum_estimates(data, paste0(
          "B15003_", sprintf("%03d", 17:25)
        )) / B15003_001E
      } else {
        NA_real_
      },
      bachelors_or_higher_percent = if (education_available) {
        100 * sum_estimates(data, paste0(
          "B15003_", sprintf("%03d", 22:25)
        )) / B15003_001E
      } else {
        NA_real_
      },
      median_gross_rent = B25064_001E,
      median_gross_rent_moe = B25064_001M,
      median_home_value = B25077_001E,
      median_home_value_moe = B25077_001M,
      owner_occupied = B25003_002E,
      homeownership_rate_percent = 100 * B25003_002E / B25003_001E
    )
}

florida_counties <- map_dfr(years, \(year) {
  year_variables <- variables

  # Educational attainment is available starting with the 2012 ACS 5-year release.
  if (year < 2012) {
    year_variables <- setdiff(year_variables, education_variables)
  }

  get_geography <- \(geography) {
    get_acs(
      geography = geography,
      state = "FL",
      year = year,
      survey = "acs5",
      variables = year_variables,
      output = "wide"
    )
  }

  bind_rows(
    summarise_acs(get_geography("county"), year),
    summarise_acs(get_geography("state"), year)
  )
})

dir.create("data", showWarnings = FALSE)
write_csv(florida_counties, "data/florida_county_acs_trends.csv")

library(dplyr)
library(purrr)
library(readr)
library(sf)
library(tidycensus)
library(tigris)

options(tigris_use_cache = TRUE)
sf_use_s2(FALSE)

# ACS 5-year vintages (2010 to 2024) where migration (B07001) and nativity (B05002) are available.
years <- 2010:2024

employment_variables <- c("B23025_003", "B23025_005")

variables <- c(
  "B01003_001", # Total population
  # Migration in past 12 months (B07001)
  "B07001_001", # Migration universe
  "B07001_017", # Same house 1 year ago
  "B07001_033", # Moved within same county
  "B07001_049", # Moved from different county within same state
  "B07001_065", # Moved from different state
  "B07001_081", # Moved from abroad
  # Nativity & Foreign-Born (B05002)
  "B05002_001", # Nativity universe
  "B05002_013", # Foreign-born total
  "B05002_014", # Foreign-born: Naturalized U.S. citizen
  # Race & Hispanic/Latino Ethnicity (B03002)
  "B03002_001", # Race/ethnicity universe
  "B03002_003", # Non-Hispanic White alone
  "B03002_004", # Non-Hispanic Black alone
  "B03002_006", # Non-Hispanic Asian alone
  "B03002_012", # Hispanic or Latino
  # Economic conditions
  "B19013_001", # Median household income
  "B17001_001", # Poverty universe
  "B17001_002", # Income below poverty level
  "B25064_001", # Median gross rent
  "B25077_001", # Median home value
  employment_variables
)

summarise_acs <- \(data, year, level) {
  employment_available <- year >= 2011

  data |>
    transmute(
      GEOID,
      name = NAME,
      level,
      year,
      population = B01003_001E,
      # Migration indicators (%)
      same_house_percent = 100 * B07001_017E / B07001_001E,
      moved_within_county_percent = 100 * B07001_033E / B07001_001E,
      moved_from_other_county_percent = 100 * B07001_049E / B07001_001E,
      moved_from_other_state_percent = 100 * B07001_065E / B07001_001E,
      moved_from_abroad_percent = 100 * B07001_081E / B07001_001E,
      in_migration_percent = 100 * (B07001_049E + B07001_065E + B07001_081E) / B07001_001E,
      # Foreign-born indicators
      foreign_born = B05002_013E,
      foreign_born_percent = 100 * B05002_013E / B05002_001E,
      naturalized_citizen_percent = 100 * B05002_014E / B05002_001E,
      non_citizen_percent = 100 * (B05002_013E - B05002_014E) / B05002_001E,
      # Race & ethnicity indicators (%)
      hispanic_percent = 100 * B03002_012E / B03002_001E,
      white_non_hispanic_percent = 100 * B03002_003E / B03002_001E,
      black_non_hispanic_percent = 100 * B03002_004E / B03002_001E,
      asian_non_hispanic_percent = 100 * B03002_006E / B03002_001E,
      # Economic indicators
      median_household_income = B19013_001E,
      poverty_rate_percent = 100 * B17001_002E / B17001_001E,
      unemployment_rate_percent = if (employment_available) {
        100 * B23025_005E / B23025_003E
      } else {
        NA_real_
      },
      median_gross_rent = B25064_001E,
      median_home_value = B25077_001E
    )
}

fetch_year_vars <- \(year) {
  if (year < 2011) setdiff(variables, employment_variables) else variables
}

# 1. County folder: Sarasota County Census tracts (2010-2024) + all Florida Census tracts (2024)
sarasota_data <- map_dfr(years, \(year) {
  yr_vars <- fetch_year_vars(year)
  tracts <- get_acs(
    geography = "tract",
    state = "FL",
    county = "Sarasota",
    year = year,
    survey = "acs5",
    variables = yr_vars,
    output = "wide"
  )
  county_total <- get_acs(
    geography = "county",
    state = "FL",
    county = "Sarasota",
    year = year,
    survey = "acs5",
    variables = yr_vars,
    output = "wide"
  )
  bind_rows(
    summarise_acs(tracts, year, "tract"),
    summarise_acs(county_total, year, "county")
  )
})

dir.create("county", showWarnings = FALSE)
write_csv(sarasota_data, "county/sarasota_migration_acs.csv")

fl_tracts_2024 <- get_acs(
  geography = "tract",
  state = "FL",
  year = 2024,
  survey = "acs5",
  variables = variables,
  output = "wide"
) |>
  summarise_acs(2024L, "tract") |>
  mutate(county_geoid = substr(GEOID, 1, 5), .after = GEOID)

write_csv(fl_tracts_2024, "county/florida_tracts_2024.csv")

# 2. State folder: Florida counties + Florida state total (2010-2024)
florida_data <- map_dfr(years, \(year) {
  yr_vars <- fetch_year_vars(year)
  counties <- get_acs(
    geography = "county",
    state = "FL",
    year = year,
    survey = "acs5",
    variables = yr_vars,
    output = "wide"
  )
  state_total <- get_acs(
    geography = "state",
    state = "FL",
    year = year,
    survey = "acs5",
    variables = yr_vars,
    output = "wide"
  )
  bind_rows(
    summarise_acs(counties, year, "county"),
    summarise_acs(state_total, year, "state")
  )
})

dir.create("state", showWarnings = FALSE)
write_csv(florida_data, "state/florida_migration_acs.csv")

# 3. Country folder: U.S. states + United States national total (2010-2024)
us_data <- map_dfr(years, \(year) {
  yr_vars <- fetch_year_vars(year)
  states <- get_acs(
    geography = "state",
    year = year,
    survey = "acs5",
    variables = yr_vars,
    output = "wide"
  )
  us_total <- get_acs(
    geography = "us",
    year = year,
    survey = "acs5",
    variables = yr_vars,
    output = "wide"
  )
  bind_rows(
    summarise_acs(states, year, "state"),
    summarise_acs(us_total, year, "us")
  )
})

dir.create("country", showWarnings = FALSE)
write_csv(us_data, "country/us_migration_acs.csv")

# 4. Simplified GeoJSON boundaries for interactive county -> tract drilldown
fl_counties_sf <- counties(state = "FL", cb = TRUE, year = 2023) |>
  st_transform(4326) |>
  select(GEOID, county_name = NAME) |>
  st_simplify(dTolerance = 0.003, preserveTopology = TRUE)

unlink("state/florida_counties.geojson")
st_write(
  fl_counties_sf,
  "state/florida_counties.geojson",
  driver = "GeoJSON",
  layer_options = "COORDINATE_PRECISION=4",
  quiet = TRUE
)

fl_tracts_sf <- tracts(state = "FL", cb = TRUE, year = 2023) |>
  st_transform(4326) |>
  mutate(county_geoid = substr(GEOID, 1, 5)) |>
  select(GEOID, county_geoid, tract_name = NAME) |>
  st_simplify(dTolerance = 0.001, preserveTopology = TRUE)

unlink("county/florida_tracts.geojson")
st_write(
  fl_tracts_sf,
  "county/florida_tracts.geojson",
  driver = "GeoJSON",
  layer_options = "COORDINATE_PRECISION=4",
  quiet = TRUE
)

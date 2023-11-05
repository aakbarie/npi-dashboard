
# Geocoding to find county ------------------------------------------------
# function geocode_addresses that takes an address dataframe and returns it with county names

library(tidyverse)
library(tidygeocoder)
library(USAboundaries)

geocode_addresses <- function(address_df) {
    # Ensure the USAboundaries package is available
    if (!requireNamespace("USAboundaries", quietly = TRUE)) {
        stop("The USAboundaries package is required but is not installed.")
    }
    
    us_counties <- us_counties(state = "CA")
    
    # Geocode addresses
    result <- test_data %>%
        mutate(addr = paste0(address, ", ", zip_code)) %>%
        geocode(
            address = addr,
            method = 'census', full_results = TRUE,
            api_options = list(census_return_type = 'geographies')
        ) %>%
        select(
            name, address = input_address, county_fips, lat, long
        ) %>%
        left_join(
            us_counties %>%
                select(county_fips = countyfp, county = name),
            by = 'county_fips'
        )
    
    return(result)
}


# Unit test ---------------------------------------------------------------

library(testthat)

# Sample data to test
test_data <- tibble(
    name = c("John Doe", "Jane Smith"),
    address = c(
        "170 17TH ST", 
        "23625 HOLMAN HWY"
    ),
    zip_code = c("93950", "93940")
)

# Expected results
expected_results <- tibble(
    name = c("John Doe", "Jane Smith"),
    address = c(
        "170 17TH ST, 93950",
        "23625 HOLMAN HWY 93940"
    ),
    county_fips = c("001", "085"),
    lat = c(38.8977, 37.3318),
    long = c(-77.0365, -122.0312),
    county = c("Monterey", "Monterey")
)

# Unit test
test_that("geocode_addresses function works correctly", {
    result <- geocode_addresses(test_data)
    
    # Test if the result has the same number of rows as expected
    expect_equal(nrow(result), nrow(expected_results))
    
    # Test if the result has the expected county names
    expect_equal(result$county, expected_results$county)
    
    # Optionally, test for other expected columns and values
})

# Run the tests
test_that()


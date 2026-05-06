library(tidycensus)
library(tidyverse)

# 1. Set your API key (get one at https://api.census.gov/data/key_signup.html)
# census_api_key("YOUR_KEY_HERE", install = TRUE)

# 2. Download 2020 Census Tract Population for a state (e.g., California)
# Define the Denver Metro Counties (FIPS Codes)
# 001-Adams, 005-Arapahoe, 014-Broomfield, 019-Clear Creek, 
# 031-Denver, 035-Douglas, 039-Elbert, 047-Gilpin, 059-Jefferson, 093-Park
denver_counties <- c("001", "005", "014", "019", "031", "035", "039", "047", "059", "093")

# Download 2020 Population by Census Tract
denver_pop_tract <- get_decennial(
  geography = "tract",
  variables = "P1_001N", # Total Population
  state = "CO",
  county = denver_counties,
  year = 2020,
  output = "wide", # Puts variable values in columns
  geometry = TRUE) # Set TRUE to download sf geometry for mapping)

# View the result
print(denver_pop_tract)

# number of tracts?
tract_count <- nrow(denver_pop_tract)

# get density and areas
library(dplyr)
library(sf)

denver_pop_tract <- denver_pop_tract %>%
  mutate(
    area_km2 = as.numeric(st_area(geometry)) / 1e6,
    pop_density = P1_001N / area_km2)

# view
print(denver_pop_tract)



# Create five Hanberry classes
denver_pop_tract <- denver_pop_tract %>%
  mutate(
    landscape = case_when(
      pop_density < 250 ~ "Exurban",
      pop_density >= 250 & pop_density < 550 ~ "Suburban Low",
      pop_density >= 550 & pop_density < 800 ~ "Suburban High",
      pop_density >= 800 & pop_density < 1900 ~ "Urban Low",
      pop_density >= 1900 ~ "Urban High"
    ))
view(denver_pop_tract)
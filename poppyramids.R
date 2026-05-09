# POPULATION PYRAMIDS
# 1. for the overall urban population
# 2. one for the overall suburban population

# don't have age-sex variables yet
# B01001 = sex by age

denver_age_sex <- get_acs(
  geography = "tract",
  table = "B01001",
  state = "CO",
  county = denver_counties,
  year = 2020)

# join this data to our 'landscape' categories
# i.e., the Urban and Subruban (and Exurban) communities
denver_age_sex <- denver_age_sex %>%
  left_join(
    st_drop_geometry(
      denver_pop_tract %>%
        select(GEOID, landscape)),
    by = "GEOID")

# form the urban and suburban groups
# urban = urban low + urban high
# suburban = suburban low + suburban high
denver_age_sex <- denver_age_sex %>%
  mutate(
    broad_landscape = case_when(
      landscape %in% c("Urban Low", "Urban High") ~ "Urban",
      landscape %in% c("Suburban Low", "Suburban High") ~ "Suburban",
      TRUE ~ "Exurban"))

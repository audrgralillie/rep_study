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

# and then, we only need age variable; clean up the data
denver_age <- denver_age_sex %>%
  filter(str_detect(variable, "^B01001_")) %>%
  filter(!variable %in% c("B01001_001"))

# age + sex labels
male_vars <- c(
  "B01001_003","B01001_004","B01001_005","B01001_006",
  "B01001_007","B01001_008","B01001_009","B01001_010",
  "B01001_011","B01001_012","B01001_013","B01001_014",
  "B01001_015","B01001_016","B01001_017","B01001_018",
  "B01001_019","B01001_020","B01001_021","B01001_022",
  "B01001_023","B01001_024","B01001_025")

female_vars <- c(
  "B01001_027","B01001_028","B01001_029","B01001_030",
  "B01001_031","B01001_032","B01001_033","B01001_034",
  "B01001_035","B01001_036","B01001_037","B01001_038",
  "B01001_039","B01001_040","B01001_041","B01001_042",
  "B01001_043","B01001_044","B01001_045","B01001_046",
  "B01001_047","B01001_048","B01001_049")

age_labels <- c(
  "Under 5","5-9","10-14","15-17","18-19","20",
  "21","22-24","25-29","30-34","35-39","40-44",
  "45-49","50-54","55-59","60-61","62-64","65-66",
  "67-69","70-74","75-79","80-84","85+")


# clean up the data set
# need after not returning to this data for a week!
denver_male_data <- denver_age %>%
  filter(variable %in% male_vars) %>%
  mutate(
    sex = "Male",
    age_group = case_when(
      variable == "B01001_003" ~ "Under 5",
      variable == "B01001_004" ~ "5-9",
      variable == "B01001_005" ~ "10-14",
      variable == "B01001_006" ~ "15-17",
      variable == "B01001_007" ~ "18-19",
      variable == "B01001_008" ~ "20",
      variable == "B01001_009" ~ "21",
      variable == "B01001_010" ~ "22-24",
      variable == "B01001_011" ~ "25-29",
      variable == "B01001_012" ~ "30-34",
      variable == "B01001_013" ~ "35-39",
      variable == "B01001_014" ~ "40-44",
      variable == "B01001_015" ~ "45-49",
      variable == "B01001_016" ~ "50-54",
      variable == "B01001_017" ~ "55-59",
      variable == "B01001_018" ~ "60-61",
      variable == "B01001_019" ~ "62-64",
      variable == "B01001_020" ~ "65-66",
      variable == "B01001_021" ~ "67-69",
      variable == "B01001_022" ~ "70-74",
      variable == "B01001_023" ~ "75-79",
      variable == "B01001_024" ~ "80-84",
      variable == "B01001_025" ~ "85+"))

denver_female_data <- denver_age %>%
  filter(variable %in% female_vars) %>%
  mutate(
    sex = "Female",
    age_group = case_when(
      variable == "B01001_027" ~ "Under 5",
      variable == "B01001_028" ~ "5-9",
      variable == "B01001_029" ~ "10-14",
      variable == "B01001_030" ~ "15-17",
      variable == "B01001_031" ~ "18-19",
      variable == "B01001_032" ~ "20",
      variable == "B01001_033" ~ "21",
      variable == "B01001_034" ~ "22-24",
      variable == "B01001_035" ~ "25-29",
      variable == "B01001_036" ~ "30-34",
      variable == "B01001_037" ~ "35-39",
      variable == "B01001_038" ~ "40-44",
      variable == "B01001_039" ~ "45-49",
      variable == "B01001_040" ~ "50-54",
      variable == "B01001_041" ~ "55-59",
      variable == "B01001_042" ~ "60-61",
      variable == "B01001_043" ~ "62-64",
      variable == "B01001_044" ~ "65-66",
      variable == "B01001_045" ~ "67-69",
      variable == "B01001_046" ~ "70-74",
      variable == "B01001_047" ~ "75-79",
      variable == "B01001_048" ~ "80-84",
      variable == "B01001_049" ~ "85+"))

denver_pyramid_data <- bind_rows(denver_male_data, denver_female_data)


# aggregate by urban and suburban
denver_pyramid_summary <- denver_pyramid_data %>%
  filter(broad_landscape %in% c("Urban", "Suburban")) %>%
  group_by(broad_landscape, sex, age_group) %>%
  summarize(population = sum(estimate), .groups = "drop")

# make either male or female 'negative'
denver_pyramid_summary <- denver_pyramid_summary %>%
  mutate(population = ifelse(sex == "Male", -population, population)) # will make Male negative



# okay, first pyramid: Urban (1)
denver_urban_pyramid <- denver_pyramid_summary %>%
  filter(broad_landscape == "Urban")
ggplot(denver_urban_pyramid,
       aes(x = population, y = age_group, fill = sex)) +
  geom_col() +
  scale_fill_manual(values = c(
    "Male" = "#6baed6",
    "Female" = "#f8766d"
  ))+
  labs(
    title = "Urban Population Pyramid - Denver MSA",
    x = "Population",
    y = "Age Group"
  ) +
  theme_minimal()


# and second, Suburban (2)
denver_suburban_pyramid <- denver_pyramid_summary %>%
  filter(broad_landscape == "Suburban")
ggplot(denver_suburban_pyramid,
       aes(x = population, y = age_group, fill = sex)) +
  geom_col() +
  scale_fill_manual(values = c(
    "Male" = "#6baed6",
    "Female" = "#f8766d"
  ))+
  labs(
    title = "Suburban Population Pyramid - Denver MSA",
    x = "Population",
    y = "Age Group"
  ) +
  theme_minimal()

# export
ggsave("urban_pyramid.png", width = 8, height = 6)
ggsave("suburban_pyramid.png", width = 8, height = 6)
# Clean the Raw data 

install.packages("readxl")
install.packages("janitor")
library(tidyverse)
library(readxl)
library(janitor)

wage_raw <- read_excel("data/wage.xlsx")
unemp_raw <- read_excel("data/unemployment_rate.xlsx")
rpp_raw <- read_csv("data/RPP.csv", skip = 3)
rent_raw <- read_csv("data/Median Gross Rent.csv")


wage_clean <- wage_raw %>%
  clean_names() %>%
  filter(occ_title == "All Occupations") %>%
  transmute(
    state = area_title,
    average_salary = parse_number(a_mean)
  )

rpp_clean <- rpp_raw %>%
  clean_names() %>%
  filter(description == "RPPs: All items") %>%
  filter(geo_name != "United States") %>%
  select(
    state = geo_name,
    price_index = x2024
  )

unemp_clean <- unemp_raw %>%
  clean_names() %>%
  transmute(
    state = state,
    unemployment_rate = april_2026_p_rate
  )

rent_clean <- rent_raw %>%
  mutate(row_id = row_number()) %>%
  filter(row_id == 150) %>%
  select(row_id, ends_with("!!Estimate")) %>%
  pivot_longer(
    cols = -row_id,
    names_to = "state",
    values_to = "median_rent"
  ) %>%
  mutate(
    state = str_remove(state, "!!Estimate")
  ) %>%
  select(state, median_rent)

setdiff(wage_clean$state, unemp_clean$state)

setdiff(unemp_clean$state, wage_clean$state)

setdiff(rpp_clean$state, wage_clean$state)

setdiff(rent_clean$state, wage_clean$state)

wage_clean <- wage_clean %>%
  filter(
    !state %in% c(
      "Guam",
      "Puerto Rico",
      "Virgin Islands"
    )
  )

rent_clean <- rent_clean %>%
  filter(state != "Puerto Rico")



# Merge Data into state_data

state_data <- wage_clean %>%
  inner_join(rent_clean, by = "state") %>%
  inner_join(unemp_clean, by = "state") %>%
  inner_join(rpp_clean, by = "state") %>%
  mutate(
    real_salary = average_salary / (price_index / 100),
    rent_share = median_rent * 12 / average_salary * 100
  )

nrow(state_data)

write_csv(
  state_data,
  "state_data_clean.csv"
)

# providers

# Play -------------------------------------------------------------------------
# Want a table with number, %, and SR of mitigable admissions by provider
connection <- sc
library(sparklyr)
library(tidyverse)

# providers_numbers_over_time:
Formatting_providers_data_for_trends <- function(sex_group) {
  data <- dplyr::tbl(
    sc,
    dbplyr::in_catalog(
      "strategyunit",
      "default",
      "sl_af_describing_mitigators_providers"
    )
  ) |>
    dplyr::filter(fyear >= "201819", sex == sex_group) |>
    sparklyr::collect()
  
  numbers_over_time <- data |>
    identify_whether_bedday_or_admissions_or_both(6:34) |>
    mutate(episodes = ifelse(activity_group == "beddays", 0, episodes)) |>   #avoid counting admissions for efficiency only activity
    select(-activity_group, -number_of_cohorts, -icb, -sex) |>
    mutate_mechanism_columns() |>
    gather(
      key = "cohorts",
      value = "value",
      -fyear,
      -age_range,
      -provider,
      -episodes,
      -beddays
    ) |>
    filter(value == 1) |>
    group_by(age_range, fyear, provider, cohorts) |>
    summarise(episodes = sum(episodes),
              beddays = sum(beddays)) |>
    mutate(year = paste0(
      stringr::str_sub(fyear, 1, 4),
      "/",
      stringr::str_sub(fyear, 5, 6)
    ),
    sex = sex_group) |>
    ungroup()
  
  return(numbers_over_time)
}

providers_over_time_sex1 <- Formatting_providers_data_for_trends(1)
providers_over_time_sex2 <- Formatting_providers_data_for_trends(2)

providers_over_time <- providers_over_time_sex1 |> 
  rbind(providers_over_time_sex2) |>
  dplyr::mutate(sex = as.character(sex))

providers_pop <- dplyr::tbl(
    sc,
    dbplyr::in_catalog("nhp", "reference", "provider_catchments")
    ) |>
  sparklyr::collect() |>
  dplyr::mutate(
    age_range = dplyr::case_when(
      age >= 0 & age <= 4 ~ "0-4",
      age >= 5 & age <= 9 ~ "5-9",
      age >= 10 & age <= 14 ~ "10-14",
      age >= 15 & age <= 19 ~ "15-19",
      age >= 20 & age <= 24 ~ "20-24",
      age >= 25 & age <= 29 ~ "25-29",
      age >= 30 & age <= 34 ~ "30-34",
      age >= 35 & age <= 39 ~ "35-39",
      age >= 40 & age <= 44 ~ "40-44",
      age >= 45 & age <= 49 ~ "45-49",
      age >= 50 & age <= 54 ~ "50-54",
      age >= 55 & age <= 59 ~ "55-59",
      age >= 60 & age <= 64 ~ "60-64",
      age >= 65 & age <= 69 ~ "65-69",
      age >= 70 & age <= 74 ~ "70-74",
      age >= 75 & age <= 79 ~ "75-79",
      age >= 80 & age <= 84 ~ "80-84",
      age >= 85 & age <= 89 ~ "85-89",
      age >= 90 ~ "90+"
    ),
    year = paste0(
      stringr::str_sub(fyear, 1, 4),
      "/",
      stringr::str_sub(fyear, 5, 6)
    )
  ) |>
  dplyr::summarise(pop = sum(count),
                   .by = c(provider, fyear, year, age_range, sex))

generating_provider_age_sex_standardised_rates <- function(data,
                                                           provider_pop,
                                                           standard_pop,
                                                           activity_type) {
  expanded_data <- data |>
    filter(fyear != 201415) |>
    expand(age_range, sex, cohorts, year, provider)
  
  standardised_data <- expanded_data |>
    left_join(data[, c("provider",
                       "year",
                       "age_range",
                       "sex",
                       "cohorts",
                       "episodes",
                       "beddays")], 
              by = c("provider", "year", "age_range", "sex", "cohorts")) |>
    left_join(provider_pop[, c("provider", "year", "age_range", "sex", "pop")], 
              by = c("provider", "year", "age_range", "sex")) |>
    rename(provider_population = pop) |>
    left_join(standard_pop, by = c("age_range", "sex")) |>
    filter(!is.na(provider),
           age_range != "NA") |>
    filter(!is.na(provider_population)) |> # TEMP WAY TO REMOVE NON ACUTE
    mutate(episodes = ifelse(is.na(episodes), 0, episodes),
           beddays = ifelse(is.na(beddays), 0, beddays)) |>
    rename(activity = {{activity_type}}) |>
    group_by(provider, year, cohorts) |>
    PHEindicatormethods::calculate_dsr(
      x = activity,
      # observed number of events
      n = provider_population,
      # non-standard pops for each stratum
      stdpop = pop
    ) |>   # standard populations for England for each stratum
    mutate(value = janitor::round_half_up(value, 0))
  
  return(standardised_data)
  
}

provider_age_sex_standardised_rates_episodes <- generating_provider_age_sex_standardised_rates(
  providers_over_time,
  providers_pop,
  targets::tar_read(standard_england_pop_2021_census),
  "episodes")


total_beddays_admissions_by_provider <- dplyr::tbl(
  connection,
  dbplyr::in_catalog(
    "strategyunit",
    "default",
    "total_elective_emergency_activity_Dx"
  )
) |>
  sparklyr::collect() |>
  dplyr::filter(fyear == 202324) |>
  dplyr::select(dplyr::starts_with("total"), provider) |>
  #dplyr::distinct() |>
  dplyr::summarise(dplyr::across(dplyr::starts_with("total"), 
                                 ~ sum(., na.rm = TRUE)), 
                   .by = provider) |>
  sparklyr::collect() |>
  dplyr::mutate(
    total_episodes_both = total_episodes_emergency + total_episodes_elective,
    total_beddays_both = total_beddays_emergency + total_beddays_elective
  )

# comparative analysis:
comp <- get_summary_by_geography(
  data = provider_age_sex_standardised_rates_episodes,
  mitigator = "intentional_self_harm",
  total = total_beddays_admissions_by_provider,
  activity_type = "admissions",
  geography = "provider",
  treatment_lookup = mitigators_and_mechanisms_treatment_lookup)

get_summary_by_geography_table(comp, "admissions", "emergency")

# trend:
generating_provider_table<-function(data, cohort){
  
  data<-data|>
    filter(cohorts==cohort) 
  
  min_value <- data |>
    dplyr::summarise(min = min(value, na.rm = TRUE)) |>
    dplyr::pull()
  
  max_value <- data |>
    dplyr::summarise(max = max(value, na.rm = TRUE)) |>
    dplyr::pull()
  
  table_data<-data |>
    select(provider, year, value)|>
    spread(key=year, value=value) |>
    #rename(`Local Authority`=laname23)|>
    mutate(`Percentage Change`=janitor::round_half_up(((`2023/24`-`2018/19`)/`2018/19`)*100,1))|>
    as.data.frame() |>
    mutate(across(2:8, ~replace_na(as.character(.), "-")))|>
    mutate(across(2:7, ~factor(., levels = c("-", min_value:max_value))))
  
  
  return(table_data)
  
}

generating_provider_table(provider_age_sex_standardised_rates_episodes, "intentional_self_harm")



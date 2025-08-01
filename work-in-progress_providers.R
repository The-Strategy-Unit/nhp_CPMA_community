# providers

# Parallel ---------------------------------------------------------------------
# Need icb_age_sex_standardised_rates_episodes for provider to run get_summary_by_geography
icb <- targets::tar_read(icb_age_sex_standardised_rates_episodes)
# above has come from:
generating_icb_age_sex_standardised_rates(
  numbers_over_time, 
  icb_population_data,
  standard_england_pop_2021_census,
  episodes
)

# numbers_over_time:
Formatting_data_for_trends_analysis_cohorts("sl_af_describing_mitigators_fyear" , icb_population_data)

# icb_population_data:
wrangling_icb_population_data(
  "Z:/Strategic Analytics/Projects 2025/Describing and Quantifying the NHP mitigators/population_data/sapeicb202420112022.xlsx",
  "Z:/Strategic Analytics/Projects 2025/Describing and Quantifying the NHP mitigators/population_data/Integrated_Care_Boards_(December_2024)_Names_and_Codes_in_EN.csv"
)

# standard_england_pop_2021_census:
formatting_standard_england_population(
  "Z:/Strategic Analytics/Projects 2025/Describing and Quantifying the NHP mitigators/population_data/2021 census data for England.csv"
)

# Play -------------------------------------------------------------------------
# Want a table with number, %, and SR of mitigable admissions by provider
connection <- sc

providers <- dplyr::tbl(connection,
           dbplyr::in_catalog(
             "strategyunit",
             "default",
             "sl_af_describing_mitigators_providers"
           )) |>
  dplyr::filter(provider == "RTH", fyear >= "201819") |>
  sparklyr::collect()

denominators <- dplyr::tbl(connection,
                          dbplyr::in_catalog(
                            "strategyunit",
                            "default",
                            "total_elective_emergency_activity_Dx"
                          )) |>
  dplyr::filter(provider == "RTH") |>
  sparklyr::collect()

population <- dplyr::tbl(connection,
                         dbplyr::in_catalog("nhp", "reference", "provider_catchments")) |>
  dplyr::filter(provider == "RTH") |>
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
    )
  ) |>
  dplyr::summarise(pop = sum(count),
                   .by = c(provider, fyear, age_range, sex))

# To check number and perc later -----------------------------------------------
providers |>
  dplyr::filter(frail_elderly_high == 1) |>
  dplyr::summarise(episodes = sum(episodes), .by = c(provider, fyear)) |>
  dplyr::left_join(denominators |>
                     dplyr::summarise(total = sum(total_episodes_emergency, na.rm = TRUE), .by = c(provider, fyear)),
                   by = c("provider", "fyear")) |>
  dplyr::mutate(perc = janitor::round_half_up(episodes * 100 / total, 2))

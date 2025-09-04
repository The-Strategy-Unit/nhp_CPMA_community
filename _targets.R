# Targets for nhp_community_mitigators analysis.

# Load packages required to define the pipeline:
library(targets)

# Set target options:
tar_option_set(
  packages = c(
    "dbplyr",
    "dplyr",
    "janitor",
    "magick",
    "readxl",
    "sparklyr",
    "tibble",
    "tidyr",
    "ComplexUpset",
    "forcats",
    "plotly",
    "scales",
    "ggVennDiagram"
  )
)

# Databricks connection --------------------------------------------------------
sc <- sparklyr::spark_connect(
  master     = Sys.getenv("DATABRICKS_HOST"),
  cluster_id = Sys.getenv("DATABRICKS_CLUSTER_ID"),
  token      = Sys.getenv("DATABRICKS_TOKEN"),
  method     = "databricks_connect",
  envname    = Sys.getenv("DATABRICKS_ENVNAME")
)

# Mitigator details ------------------------------------------------------------
mitigator_summary_table <-
  readxl::read_excel("reference/summary_mitigators_table.xlsx") |>
  dplyr::mutate(mechanism = snakecase::to_snake_case(mechanism)) |>
  dplyr::arrange(mitigator_name)

mitigators <- mitigator_summary_table |>
  dplyr::select(mitigator_or_mechanism = mitigator_code,
                treatment_type = type_of_admission)

mechanisms <- mitigator_summary_table |>
  dplyr::summarise(treatment_type = paste(unique(type_of_admission), 
                                          collapse = ', '),
                   .by = mechanism) |>
  dplyr::mutate(treatment_type = ifelse(
    stringr::str_detect(treatment_type, "emergency & elective"),
    "both",
    treatment_type
  )) |>
  dplyr::rename(mitigator_or_mechanism = mechanism)

mitigators_and_mechanisms_treatment_lookup <- mitigators |>
  rbind(mechanisms) |>
  dplyr::mutate(treatment_type = stringr::str_replace(treatment_type, 
                                                      "emergency & elective", 
                                                      "both"))

mitigators_and_mechanisms <- mitigators_and_mechanisms_treatment_lookup |>
  dplyr::pull(mitigator_or_mechanism)

# Run the R scripts in the R/ folder with our custom functions:
tar_source()

# The target list:
list(
  tar_target(
    mitigator_totals,
    get_mitigators_totals(mitigator_summary_table, numbers_over_time)
  ),
  
  # Population data ------------------------------------------------------------
  # LA:
  tar_target(
    la_population_data,
    wrangling_la_population_data(
      "Z:/Strategic Analytics/Projects 2025/Describing and Quantifying the NHP mitigators/population_data/myebtablesenglandwales20112023.xlsx"
    )
  ),
  tarchetypes::tar_file(la_code_lookup_filepath, "reference/la_code_lookup.xlsx"),
  tar_target(la_code_lookup, read_excel(la_code_lookup_filepath)),
  
  # ICB:
  tar_target(
    icb_population_data,
    wrangling_icb_population_data(
      "Z:/Strategic Analytics/Projects 2025/Describing and Quantifying the NHP mitigators/population_data/sapeicb202420112022.xlsx",
      "Z:/Strategic Analytics/Projects 2025/Describing and Quantifying the NHP mitigators/population_data/Integrated_Care_Boards_(December_2024)_Names_and_Codes_in_EN.csv"
    )
  ),
  
  # Providers:
  tar_target(
    providers_population_data,
    dplyr::tbl(
      sc,
      dbplyr::in_catalog("nhp", "reference", "provider_catchments")
    ) |>
      sparklyr::collect() |>
      add_year_column() |>
      add_age_range_column() |>
      left_join(la_population_data |>
                  dplyr::mutate(sex = as.character(sex)),
                by = c("year" = "fyear", 
                       "area_code" = "ladcode23", 
                       "sex", 
                       "age_range")
                ) |>
      dplyr::mutate(provider_pop = pcnt * la_population) |>
      dplyr::summarise(pop = sum(provider_pop, na.rm = TRUE) |>
                         janitor::round_half_up(0), 
                       .by = c(provider, year, age_range, sex))
  ),
  tar_target(
    provider_reference,
    dplyr::tbl(
      sc,
      dbplyr::in_catalog("strategyunit", "default", "sl_af_providers_reference")
    ) |>
      sparklyr::collect() |>
      dplyr::left_join(
        icb_population_data |>
          dplyr::select(icb24cdh, icb_2024_name) |>
          unique(),
        by = c("main_icb" = "icb24cdh")
      ) |>
      dplyr::mutate(
        icb_2024_name = simplify_icb_name(icb_2024_name),
        org_name = simplify_provider_name(org_name)
      )
  ),
  
  # England:
  tar_target(
    standard_england_pop_2021_census,
    formatting_standard_england_population(
      "Z:/Strategic Analytics/Projects 2025/Describing and Quantifying the NHP mitigators/population_data/2021 census data for England.csv"
    )
  ),
  
  # Population by patient characteristics (sex, age, ethnicity, imd):
  tar_target(
    pop_by_sex,
    icb_population_data |>
      dplyr::filter(fyear == "2023/24") |>
      dplyr::summarise(pop = sum(icb_population), .by = sex) |>
      dplyr::mutate(
        sex = dplyr::case_when(sex == 2 ~ "female", 
                               sex == 1 ~ "male", 
                               .default = NULL)
      )
  ),
  tar_target(
    pop_by_age,
    icb_population_data |>
      dplyr::filter(fyear == "2023/24") |>
      dplyr::summarise(pop = sum(icb_population), .by = age_range)
  ),
  tarchetypes::tar_file(
    pop_by_ethnicity_filepath,
    r"{Z:\Strategic Analytics\Projects 2025\Describing and Quantifying the NHP mitigators\population_data\ethnicity_by_icb_2021.xlsx}"
  ),
  tar_target(
    pop_by_ethnicity,
    readxl::read_xlsx(pop_by_ethnicity_filepath) |>
      janitor::clean_names() |>
      dplyr::filter(integrated_care_boards != "Does not apply: Wales") |>
      dplyr::mutate(
        Ethnic_Category = dplyr::case_when(
          ethnic_group_20_categories_code %in% c(1, 3:5) ~ "asian",
          ethnic_group_20_categories_code %in% c(6:8) ~ "black",
          ethnic_group_20_categories_code %in% c(9:12) ~ "mixed",
          ethnic_group_20_categories_code %in% c(2, 18, 19) ~ "other",
          ethnic_group_20_categories_code %in% c(13:17) ~ "white",
          .default = "NULL"
        )
      ) |>
      dplyr::summarise(pop = sum(observation), .by = Ethnic_Category) |>
      dplyr::filter(Ethnic_Category != "NULL")
  ),
  tar_target(
    pop_by_imd,
    wrangling_imd_population_by_icb(
      "Z:/Strategic Analytics/Projects 2025/Describing and Quantifying the NHP mitigators/population_data/LSOA21_to_ICB.csv",
      "Z:/Strategic Analytics/Projects 2025/Describing and Quantifying the NHP mitigators/population_data/LSOA11_to_LSOA21.csv",
      "Z:/Strategic Analytics/Projects 2025/Describing and Quantifying the NHP mitigators/population_data/LSOA_pop.xlsx",
      "Z:/Strategic Analytics/Projects 2025/Describing and Quantifying the NHP mitigators/population_data/LSOA_and_IMD.csv"
    ) |>
      dplyr::filter(!is.na(icb24cdh)) |>
      summarise(pop = sum(total), .by = c(imd19_decile))
    
  ),
  
  # Standardised rates ---------------------------------------------------------
  ## ICB -----------------------------------------------------------------------
  # For mitigators/mechanisms:
  tar_target(
    icb_age_sex_standardised_rates_episodes,
    generating_icb_age_sex_standardised_rates(
      numbers_over_time,
      icb_population_data,
      standard_england_pop_2021_census,
      episodes
    )
  ),
  tar_target(
    icb_age_sex_standardised_rates_beddays,
    generating_icb_age_sex_standardised_rates(
      numbers_over_time,
      icb_population_data,
      standard_england_pop_2021_census,
      beddays
    )
  ),
  
  # For total mitigation:
  tar_target(
    icb_age_sex_standardised_rates_episodes_total_mitigation,
    generating_icb_age_sex_standardised_rates(
      numbers_over_time_total_mitigation |>
        mutate(cohorts = "all"),
      icb_population_data,
      standard_england_pop_2021_census,
      episodes
    )
  ),
  tar_target(
    icb_age_sex_standardised_rates_beddays_total_mitigation,
    generating_icb_age_sex_standardised_rates(
      numbers_over_time_total_mitigation |>
        mutate(cohorts = "all"),
      icb_population_data,
      standard_england_pop_2021_census,
      beddays
    )
  ),
  
  ## England -------------------------------------------------------------------
  # For mitigators/mechanisms:
  tar_target(
    england_age_sex_standardised_rates_episodes,
    generating_england_age_sex_standardised_rates(
      numbers_over_time,
      icb_population_data,
      standard_england_pop_2021_census,
      episodes
    )
  ),
  tar_target(
    england_age_sex_standardised_rates_beddays,
    generating_england_age_sex_standardised_rates(
      numbers_over_time,
      icb_population_data,
      standard_england_pop_2021_census,
      beddays
    )
  ),
  
  # For total mitigation:
  tar_target(
    england_age_sex_standardised_rates_episodes_total_mitigation,
    generating_england_age_sex_standardised_rates(
      numbers_over_time_total_mitigation |>
        mutate(cohorts =
                 "all"),
      icb_population_data,
      standard_england_pop_2021_census,
      episodes
    )
  ),
  tar_target(
    england_age_sex_standardised_rates_beddays_total_mitigation,
    generating_england_age_sex_standardised_rates(
      numbers_over_time_total_mitigation |>
        mutate(cohorts =
                 "all"),
      icb_population_data,
      standard_england_pop_2021_census,
      beddays
    )
  ),
  
  ## LA ------------------------------------------------------------------------
  # For mitigators/mechanisms:
  tar_target(
    la_age_sex_standardised_rates_episodes_over_time,
    generating_la_age_sex_standardised_rates_for_trends(
      numbers_over_time_local_authority_sex1,
      numbers_over_time_local_authority_sex2,
      la_code_lookup,
      la_population_data,
      standard_england_pop_2021_census,
      episodes
    )
  ),
  tar_target(
    la_age_sex_standardised_rates_beddays_over_time,
    generating_la_age_sex_standardised_rates_for_trends(
      numbers_over_time_local_authority_sex1,
      numbers_over_time_local_authority_sex2,
      la_code_lookup,
      la_population_data,
      standard_england_pop_2021_census,
      beddays
    )
  ),
  
  # For total mitigation:
  tar_target(
    la_age_sex_standardised_rates_episodes_total_mitigation,
    generating_la_age_sex_standardised_rates_for_trends(
      rbind(
        numbers_over_time_local_authority_total_mitigation_sex1,
        numbers_over_time_local_authority_total_mitigation_sex2
      ) |>
        mutate(cohorts = "all"),
      NA,
      la_code_lookup,
      la_population_data,
      standard_england_pop_2021_census,
      episodes
    )
  ),
  tar_target(
    la_age_sex_standardised_rates_beddays_total_mitigation,
    generating_la_age_sex_standardised_rates_for_trends(
      rbind(
        numbers_over_time_local_authority_total_mitigation_sex1,
        numbers_over_time_local_authority_total_mitigation_sex2
      ) |>
        mutate(cohorts = "all"),
      NA,
      la_code_lookup,
      la_population_data,
      standard_england_pop_2021_census,
      beddays
    )
  ),
  
  ## Provider ------------------------------------------------------------------
  # For mitigators/mechanisms:
  tar_target(
    provider_age_sex_standardised_rates_episodes,
    generating_provider_age_sex_standardised_rates(
      providers_over_time,
      providers_population_data,
      standard_england_pop_2021_census,
      "episodes"
    ) |>
      dplyr::left_join(provider_reference, "provider")
  ),
  tar_target(
    provider_age_sex_standardised_rates_beddays,
    generating_provider_age_sex_standardised_rates(
      providers_over_time,
      providers_population_data,
      standard_england_pop_2021_census,
      "beddays"
    ) |>
      dplyr::left_join(provider_reference, "provider")
  ),
  
  # For total mitigation:
  tar_target(
    provider_age_sex_standardised_rates_episodes_total_mitigation,
    generating_provider_age_sex_standardised_rates(
      providers_over_time_total_mitigation,
      providers_population_data,
      standard_england_pop_2021_census,
      "episodes"
    ) |>
      dplyr::left_join(provider_reference, "provider")
  ),
  tar_target(
    provider_age_sex_standardised_rates_beddays_total_mitigation,
    generating_provider_age_sex_standardised_rates(
      providers_over_time_total_mitigation,
      providers_population_data,
      standard_england_pop_2021_census,
      "beddays"
    ) |>
      dplyr::left_join(provider_reference, "provider")
  ),
  
  # Descriptive analysis -------------------------------------------------------
  ## Overview of mitigator -----------------------------------------------------
  tar_target(total_beddays_admissions, get_total_beddays_admissions(sc)),
  tarchetypes::tar_map(
    list(mitigator = mitigators_and_mechanisms),
    tar_target(
      overview,
      get_overview_of_mitigator(
        mitigator,
        total_beddays_admissions,
        mitigators_and_mechanisms_treatment_lookup
      )
    )
  ),
  
  ## Percentage breakdowns -----------------------------------------------------
  tarchetypes::tar_map(
    list(
      group = rep(c("age", "ethnicity", "imd", "sex"), each = 66),
      mitigator = rep(mitigators_and_mechanisms, 8),
      activity_type = rep(rep(c(
        "admissions", "beddays"
      ), each = 33), 4)
    ),
    tar_target(perc, get_perc_by_group(mitigator, group, activity_type))
  ),
  
  ## Rates per 100,000 population ----------------------------------------------
  tarchetypes::tar_map(
    list(
      mitigator = rep(mitigators_and_mechanisms, 2),
      activity_type = rep(c("admissions", "beddays"), each = 33)
    ),
    tar_target(
      rates_age,
      get_rates_by_group(mitigator, "age", pop_by_age, activity_type)
    )
  ),
  tarchetypes::tar_map(
    list(
      mitigator = rep(mitigators_and_mechanisms, 2),
      activity_type = rep(c("admissions", "beddays"), each = 33)
    ),
    tar_target(
      rates_ethnicity,
      get_rates_by_group(mitigator, 
                         "ethnicity", 
                         pop_by_ethnicity, 
                         activity_type)
    )
  ),
  tarchetypes::tar_map(
    list(
      mitigator = rep(mitigators_and_mechanisms, 2),
      activity_type = rep(c("admissions", "beddays"), each = 33)
    ),
    tar_target(
      rates_imd,
      get_rates_by_group(mitigator, "imd", pop_by_imd, activity_type)
    )
  ),
  tarchetypes::tar_map(
    list(
      mitigator = rep(mitigators_and_mechanisms, 2),
      activity_type = rep(c("admissions", "beddays"), each = 33)
    ),
    tar_target(
      rates_sex,
      get_rates_by_group(mitigator, "sex", pop_by_sex, activity_type)
    )
  ),
  
  ## Specialty -----------------------------------------------------------------
  tar_target(
    specialty_url,
    r"{https://digital.nhs.uk/binaries/content/assets/website-assets/isce/dcb0028/0028452019codelistspecificationv1.2.xlsx}"
  ),
  tar_target(
    specialty_key,
    scrape_xls(specialty_url, sheet = 2) |>
      dplyr::rename(specialty = treatment_function_title, 
                    specialty_group = group)
  ),
  tarchetypes::tar_map(
    list(
      mitigator = rep(mitigators_and_mechanisms, 2),
      activity_type = rep(c("admissions", "beddays"), each = 33)
    ),
    tar_target(
      specialty,
      get_top_ten(mitigator, activity_type, "tretspef", specialty_key)
    )
  ),
  
  ## Diagnoses -----------------------------------------------------------------
  tar_target(
    diagnosis_filename,
    r"{Z:\Strategic Analytics\Projects 2025\Describing and Quantifying the NHP mitigators\population_data\ICD10_diagnosis_code_descriptions.csv}"
  ),
  tar_target(
    diagnosis_key,
    read.csv(diagnosis_filename) |>
      janitor::clean_names() |>
      dplyr::mutate(
        description = dplyr::case_when(
          # to make description more understandable:
          code == "U07.0" ~ "Vaping-related disorder",
          code %in% c("U07", "U08", "U09") ~ "COVID-19",
          .default = description
        )
      )
  ),
  tarchetypes::tar_map(
    list(
      mitigator = rep(mitigators_and_mechanisms, 2),
      activity_type = rep(c("admissions", "beddays"), each = 33)
    ),
    tar_target(
      diagnosis,
      get_top_ten(mitigator, activity_type, "diagnosis", diagnosis_key)
    )
  ),
  
  ## Length of Stay ------------------------------------------------------------
  tarchetypes::tar_map(
    list(mitigator = mitigators_and_mechanisms),
    tar_target(perc_los, get_perc_by_los(mitigator))
  ),
  
  # For LOS range trends:
  tar_target(
    los_over_time,
    dplyr::tbl(
      sc,
      dbplyr::in_catalog(
        "strategyunit",
        "default",
        "SL_AF_describing_mitigators_los_over_time"
      )
    ) |>
      dplyr::filter(fyear >= "201819") |>
      sparklyr::collect() |>
      add_year_column() |>
      dplyr::select(-fyear)
  ),
  tarchetypes::tar_map(
    list(mitigator = mitigators_and_mechanisms),
    tar_target(
      perc_los_trends,
      get_perc_by_los_trends(los_over_time, mitigator)
    )
  ),
  
  # For average LOS trends:
  tarchetypes::tar_map(
    list(
      geography = rep(c("england", "icb"), each = 33),
      mitigator = rep(mitigators_and_mechanisms, 2)
    ),
    tar_target(
      avg_los_trends,
      get_average_los_trends(
        geography,
        mitigator,
        icb_population_data |>
          dplyr::filter(fyear == "2023/24") |>
          dplyr::select(icb24cdh, icb_2024_name) |>
          unique()
      )
    )
  ),
  
  # Cohort analysis ------------------------------------------------------------
  # For mitigators:
  tar_target(
    total_cohort_numbers_2324,
    Formatting_data_for_cohort_overlap("sl_af_describing_mitigators_fyear") |>
      filter(fyear == "202324")
  ),
  
  # For mechanisms:
  tar_target(
    cohort_overlap_numbers_redirection,
    total_cohort_numbers_2324 |>
      filter(fyear == "202324") |>
      summarise(
        episodes = sum(episodes),
        beddays = sum(beddays),
        .by = c(
          ambulatory_care_conditions_acute,
          ambulatory_care_conditions_chronic,
          ambulatory_care_conditions_vaccine_preventable,
          eol_care_2_days,
          eol_care_3_to_14_days,
          falls_related_admissions,
          frail_elderly_high,
          frail_elderly_intermediate,
          medicines_related_admissions_explicit,
          medicines_related_admissions_implicit_anti_diabetics,
          medicines_related_admissions_implicit_benzodiasepines,
          medicines_related_admissions_implicit_diurectics,
          medicines_related_admissions_implicit_nsaids,
          readmission_within_28_days,
          zero_los_no_procedure_adult,
          zero_los_no_procedure_child
        )
      ) |>
      mutate(number_of_cohorts = rowSums(pick(1:16), na.rm = TRUE)) |>
      filter(number_of_cohorts != 0)
  ),
  tar_target(
    cohort_overlap_numbers_prevention,
    total_cohort_numbers_2324 |>
      filter(fyear == "202324") |>
      summarise(
        episodes = sum(episodes),
        beddays = sum(beddays),
        .by = c(
          alcohol_partially_attributable_acute,
          alcohol_partially_attributable_chronic,
          alcohol_wholly_attributable,
          obesity_related_admissions,
          smoking,
          raid_ae,
          intentional_self_harm,
          medically_unexplained_related_admissions
        )
      ) |>
      mutate(number_of_cohorts = rowSums(pick(1:8), na.rm = TRUE)) |>
      filter(number_of_cohorts != 0)
  ),
  tar_target(
    cohort_overlap_numbers_relocation,
    total_cohort_numbers_2324 |>
      filter(fyear == "202324") |>
      summarise(
        episodes = sum(episodes),
        beddays = sum(beddays),
        .by = c(
          virtual_wards_activity_avoidance_ari,
          virtual_wards_activity_avoidance_heart_failure
        )
      ) |>
      mutate(number_of_cohorts = rowSums(pick(1:2), na.rm = TRUE)) |>
      filter(number_of_cohorts != 0)
  ),
  tar_target(
    cohort_overlap_numbers_efficiencies,
    total_cohort_numbers_2324 |>
      filter(fyear == "202324") |>
      summarise(
        episodes = sum(episodes),
        beddays = sum(beddays),
        .by = c(emergency_elderly, stroke_early_supported_discharge, raid_ip)
      ) |>
      mutate(number_of_cohorts = rowSums(pick(1:3), na.rm = TRUE)) |>
      filter(number_of_cohorts != 0)
  ),
  
  # Trends analysis ------------------------------------------------------------
  ## ICB -----------------------------------------------------------------------
  # For mitigators/mechanisms:
  tar_target(
    numbers_over_time,
    Formatting_data_for_trends_analysis_cohorts(
      "sl_af_describing_mitigators_fyear", 
      icb_population_data)
  ),
  tar_target(
    denominator_over_time,
    Formatting_data_for_trends_analysis_denominator(
      "sl_af_total_elective_emergency_activity",
      icb_population_data
    )
  ),
  
  # For total mitigation:
  tar_target(
    numbers_over_time_total_mitigation,
    Formatting_data_for_trends_analysis_total_mitigation(
      "sl_af_describing_mitigators_fyear", 
      icb_population_data)
  ),
  
  ## LA ------------------------------------------------------------------------
  # For mitigators/mechanisms:
  tar_target(
    numbers_over_time_local_authority_sex1,
    Formatting_la_data_for_trends(
      "SL_AF_describing_mitigators_local_authority_by_yr", 
      1)
  ),
  tar_target(
    numbers_over_time_local_authority_sex2,
    Formatting_la_data_for_trends(
      "SL_AF_describing_mitigators_local_authority_by_yr", 
      2)
  ),
  
  # For total mitigation:
  tar_target(
    numbers_over_time_local_authority_total_mitigation_sex1,
    Formatting_la_data_for_trends_total_mitigation(
      "SL_AF_describing_mitigators_local_authority_by_yr",
      1,
      la_population_data
    )
  ),
  tar_target(
    numbers_over_time_local_authority_total_mitigation_sex2,
    Formatting_la_data_for_trends_total_mitigation(
      "SL_AF_describing_mitigators_local_authority_by_yr",
      2,
      la_population_data
    )
  ),
  
  ## Provider ------------------------------------------------------------------
  # For mitigators/mechanisms:
  tar_target(
    providers_over_time_sex1,
    Formatting_providers_data_for_trends(1)
  ),
  tar_target(
    providers_over_time_sex2,
    Formatting_providers_data_for_trends(2)
  ),
  tar_target(
    providers_over_time,
    providers_over_time_sex1 |>
      rbind(providers_over_time_sex2) |>
      dplyr::mutate(sex = as.character(sex))
  ),
  
  # For total mitigation:
  tar_target(
    providers_over_time_sex1_total_mitigation,
    Formatting_providers_data_for_trends_total_mitigation(1)
  ),
  tar_target(
    providers_over_time_sex2_total_mitigation,
    Formatting_providers_data_for_trends_total_mitigation(2)
  ),
  tar_target(
    providers_over_time_total_mitigation,
    providers_over_time_sex1_total_mitigation |>
      rbind(providers_over_time_sex2_total_mitigation) |>
      dplyr::mutate(sex = as.character(sex), cohorts = "all")
  ),
  
  # Comparative analysis -------------------------------------------------------
  ## ICB -----------------------------------------------------------------------
  tar_target(
    total_beddays_admissions_by_icb,
    dplyr::tbl(
      sc,
      dbplyr::in_catalog(
        "strategyunit",
        "default",
        "sl_af_describing_mitigators_final_2324_icb"
      )
    ) |>
      dplyr::select(dplyr::starts_with("total"), icb) |>
      dplyr::distinct() |>
      dplyr::summarise(dplyr::across(dplyr::starts_with("total"), 
                                     ~ sum(.)), 
                       .by = icb) |>
      sparklyr::collect() |>
      dplyr::mutate(
        total_episodes_both = total_episodes_emergency + total_episodes_elective,
        total_beddays_both = total_beddays_emergency + total_beddays_elective
      )
  ),
  tarchetypes::tar_map(
    list(mitigator = mitigators_and_mechanisms),
    tar_target(
      summary_icb_admissions,
      get_summary_by_geography(
        icb_age_sex_standardised_rates_episodes,
        mitigator,
        total_beddays_admissions_by_icb,
        "admissions",
        "icb",
        mitigators_and_mechanisms_treatment_lookup
      )
    )
  ),
  tarchetypes::tar_map(
    list(mitigator = mitigators_and_mechanisms),
    tar_target(
      summary_icb_beddays,
      get_summary_by_geography(
        icb_age_sex_standardised_rates_beddays,
        mitigator,
        total_beddays_admissions_by_icb,
        "beddays",
        "icb",
        mitigators_and_mechanisms_treatment_lookup
      )
    )
  ),
  
  ## LA ------------------------------------------------------------------------
  tar_target(
    total_beddays_admissions_by_la,
    dplyr::tbl(
      sc,
      dbplyr::in_catalog(
        "strategyunit",
        "default",
        "sl_af_describing_mitigators_final_2324_local_authority"
      )
    ) |>
      dplyr::select(dplyr::starts_with("total"), resladst_ons) |>
      dplyr::distinct() |>
      dplyr::summarise(dplyr::across(dplyr::starts_with("total"), 
                                     ~ sum(.)), 
                       .by = resladst_ons) |>
      dplyr::filter(startsWith(resladst_ons, "E")) |>
      dplyr::rename(ladcode23 = resladst_ons) |>
      sparklyr::collect() |>
      dplyr::mutate(
        total_episodes_both = total_episodes_emergency + total_episodes_elective,
        total_beddays_both = total_beddays_emergency + total_beddays_elective
      )
  ),
  tarchetypes::tar_map(
    list(mitigator = mitigators_and_mechanisms),
    tar_target(
      summary_la_admissions,
      get_summary_by_geography(
        la_age_sex_standardised_rates_episodes_over_time,
        mitigator,
        total_beddays_admissions_by_la,
        "admissions",
        "ladcode23",
        mitigators_and_mechanisms_treatment_lookup
      )
    )
  ),
  tarchetypes::tar_map(
    list(mitigator = mitigators_and_mechanisms),
    tar_target(
      summary_la_beddays,
      get_summary_by_geography(
        la_age_sex_standardised_rates_beddays_over_time,
        mitigator,
        total_beddays_admissions_by_la,
        "beddays",
        "ladcode23",
        mitigators_and_mechanisms_treatment_lookup
      )
    )
  ),
  
  ## Provider ------------------------------------------------------------------
  tar_target(
    total_beddays_admissions_by_provider,
    dplyr::tbl(
      sc,
      dbplyr::in_catalog(
        "strategyunit",
        "default",
        "total_elective_emergency_activity_Dx"
      )
    ) |>
      sparklyr::collect() |>
      dplyr::filter(fyear == 202324) |>
      dplyr::select(dplyr::starts_with("total"), provider) |>
      dplyr::summarise(dplyr::across(
        dplyr::starts_with("total"), ~ sum(., na.rm = TRUE)
      ), .by = provider) |>
      sparklyr::collect() |>
      dplyr::mutate(
        total_episodes_both = total_episodes_emergency + total_episodes_elective,
        total_beddays_both = total_beddays_emergency + total_beddays_elective
      )
  ),
  tarchetypes::tar_map(
    list(mitigator = mitigators_and_mechanisms),
    tar_target(
      summary_provider_admissions,
      get_summary_by_geography(
        provider_age_sex_standardised_rates_episodes,
        mitigator,
        total_beddays_admissions_by_provider,
        "admissions",
        "provider",
        mitigators_and_mechanisms_treatment_lookup
      )
    )
  ),
  tarchetypes::tar_map(
    list(mitigator = mitigators_and_mechanisms),
    tar_target(
      summary_provider_beddays,
      get_summary_by_geography(
        provider_age_sex_standardised_rates_beddays,
        mitigator,
        total_beddays_admissions_by_provider,
        "beddays",
        "provider",
        mitigators_and_mechanisms_treatment_lookup
      )
    )
  ),
  tar_target(
    provider_type,
    get_mitigation_by_provider_type(providers_over_time_total_mitigation,
                                    provider_reference)
  ),
  tar_target(
    provider_type_2324_table,
    provider_type |>
      dplyr::filter(year == "2023/24") |>
      dplyr::select(org_type, 
                    admissions, 
                    perc_admissions, 
                    beddays, 
                    perc_beddays) |>
      dplyr::arrange(org_type) |>
      dplyr::rename("Provider Type" = org_type) |>
      dplyr::rename_with(~ format_as_title(.)) |>
      get_table() |>
      flextable::delete_part(part = "footer") |>
      flextable::align(part = "all", align = "left")
  ),
  
  ## For total mitigation -------------------------------------------------------
  # ICB:
  tar_target(
    summary_icb_admissions_all,
    get_summary_by_geography(
      icb_age_sex_standardised_rates_episodes_total_mitigation,
      "all",
      total_beddays_admissions_by_icb,
      "admissions",
      "icb"
    )
  ),
  tar_target(
    summary_icb_beddays_all,
    get_summary_by_geography(
      icb_age_sex_standardised_rates_beddays_total_mitigation,
      "all",
      total_beddays_admissions_by_icb,
      "beddays",
      "icb"
    )
  ),
  
  # LA:
  tar_target(
    summary_la_admissions_all,
    get_summary_by_geography(
      la_age_sex_standardised_rates_episodes_total_mitigation,
      "all",
      total_beddays_admissions_by_la,
      "admissions",
      "ladcode23"
    )
  ),
  tar_target(
    summary_la_beddays_all,
    get_summary_by_geography(
      la_age_sex_standardised_rates_beddays_total_mitigation,
      "all",
      total_beddays_admissions_by_la,
      "beddays",
      "ladcode23"
    )
  ),
  
  # Provider:
  tar_target(
    summary_provider_admissions_all,
    get_summary_by_geography(
      provider_age_sex_standardised_rates_episodes_total_mitigation,
      "all",
      total_beddays_admissions_by_provider,
      "admissions",
      "provider"
    )
  ),
  tar_target(
    summary_provider_beddays_all,
    get_summary_by_geography(
      provider_age_sex_standardised_rates_beddays_total_mitigation,
      "all",
      total_beddays_admissions_by_provider,
      "beddays",
      "provider"
    )
  )
)
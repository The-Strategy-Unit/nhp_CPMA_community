# Targets for nhp_community_mitigators analysis.

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
  packages = c(
    "dbplyr",
    "dplyr",
    "janitor",
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
# Connection to Databricks
sc <- sparklyr::spark_connect(
  master     = Sys.getenv("DATABRICKS_HOST"),
  cluster_id = Sys.getenv("DATABRICKS_CLUSTER_ID"),
  token      = Sys.getenv("DATABRICKS_TOKEN"),
  method     = "databricks_connect",
  envname    = Sys.getenv("DATABRICKS_ENVNAME")
)

# Mitigator details ------------------------------------------------------------
mitigator_summary_table <- readxl::read_excel("summary_mitigators_table.xlsx")

mitigators <- mitigator_summary_table |>
  dplyr::select(mitigator_or_mechanism = mitigator_code, treatment_type = type_of_admission) 

mechanisms <- mitigator_summary_table |>
  dplyr::mutate(mechanism = snakecase::to_snake_case(mechanism)) |>
  dplyr::summarise(treatment_type = paste(unique(type_of_admission), 
                                          collapse = ', '), 
                   .by = mechanism) |>
  dplyr::mutate(
    treatment_type = ifelse(stringr::str_detect(treatment_type, 
                                                "emergency & elective"), 
                            "both", 
                            treatment_type)) |>
dplyr::rename(mitigator_or_mechanism = mechanism)

mitigators_and_mechanisms_treatment_lookup <- mitigators |>
  rbind(mechanisms)

mitigators_and_mechanisms <- mitigators_and_mechanisms_treatment_lookup |>
  dplyr::pull(mitigator_or_mechanism)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()

# Replace the target list below with your own:
list(
  # Population data ------------------------------------------------------------
  tar_target(
    la_population_data,
    wrangling_la_population_data(
      "Z:/Strategic Analytics/Projects 2025/Describing and Quantifying the NHP mitigators/population_data/myebtablesenglandwales20112023.xlsx"
    )
  ),
  
  tar_target(
    icb_population_data,
    wrangling_icb_population_data(
      "Z:/Strategic Analytics/Projects 2025/Describing and Quantifying the NHP mitigators/population_data/sapeicb202420112022.xlsx",
      "Z:/Strategic Analytics/Projects 2025/Describing and Quantifying the NHP mitigators/population_data/Integrated_Care_Boards_(December_2024)_Names_and_Codes_in_EN.csv"
    )
  ),
  
  tar_target(
    standard_england_pop_2021_census,
    formatting_standard_england_population(
      "Z:/Strategic Analytics/Projects 2025/Describing and Quantifying the NHP mitigators/population_data/2021 census data for England.csv"
    )
  ),
  
  tar_target(
    pop_by_sex,
    icb_population_data |>
      dplyr::filter(fyear == "2023/24") |>
      dplyr::summarise(pop = sum(icb_population), .by = sex) |>
      dplyr::mutate(
        sex = dplyr::case_when(sex == 2 ~ "female", sex == 1 ~ "male", .default = NULL)
      )
  ),
  tar_target(
    pop_by_age,
    icb_population_data |>
      dplyr::filter(fyear == "2023/24") |>
      dplyr::summarise(pop = sum(icb_population), .by = age_range)
  ),
  
  tarchetypes::tar_file(
    pop_by_ethnicity_url,
    r"{Z:\Strategic Analytics\Projects 2025\Describing and Quantifying the NHP mitigators\population_data\ethnicity_by_icb_2021.xlsx}"
  ),
  tar_target(
    pop_by_ethnicity,
    readxl::read_xlsx(pop_by_ethnicity_url) |>
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
    pop_by_imd_url,
    r"{https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationprojections/adhocs/15363monthlypopulationsbyindexofmultipledeprivationimddecileenglandjanuary2019toaugust2022/monthlyimdpopulations.xlsx}"
  ),
  tar_target(
    pop_by_imd,
    scrape_xls(pop_by_imd_url, sheet = "1", skip = 3) |>
      dplyr::summarise(pop = sum(august_2022), .by = imd_decile) |>
      dplyr::rename(imd19_decile = imd_decile)
  ),
  
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
  
  
  tar_target(
    icb_age_sex_standardised_rates_episodes_total_mitigation,
    generating_icb_age_sex_standardised_rates(
      numbers_over_time_total_mitigation|>
        mutate(cohorts="all"),
      icb_population_data,
      standard_england_pop_2021_census,
      episodes
    )
  ),
  
  tar_target(
    icb_age_sex_standardised_rates_beddays_total_mitigation,
    generating_icb_age_sex_standardised_rates(
      numbers_over_time_total_mitigation|>
        mutate(cohorts="all"),
      icb_population_data,
      standard_england_pop_2021_census,
      beddays
    )
  ),
  tar_target(england_age_sex_standardised_rates_episodes,
            generating_england_age_sex_standardised_rates(numbers_over_time, 
                                                          icb_population_data, 
                                                          standard_england_pop_2021_census, 
                                                          episodes)
 ),
 
 tar_target(england_age_sex_standardised_rates_beddays,
            generating_england_age_sex_standardised_rates(numbers_over_time, 
                                                          icb_population_data, 
                                                          standard_england_pop_2021_census, 
                                                          beddays)
 ),
 
 tar_target(england_age_sex_standardised_rates_episodes_total_mitigation,
            generating_england_age_sex_standardised_rates(numbers_over_time_total_mitigation|>
                                                            mutate(cohorts="all"), 
                                                          icb_population_data, 
                                                          standard_england_pop_2021_census, 
                                                          episodes)
 ),
 
 tar_target(england_age_sex_standardised_rates_beddays_total_mitigation,
            generating_england_age_sex_standardised_rates(numbers_over_time_total_mitigation|>
                                                            mutate(cohorts="all"), 
                                                          icb_population_data, 
                                                          standard_england_pop_2021_census, 
                                                          beddays)
 ),
 
 # Local authority
   tar_target(la_numbers,
              dplyr::tbl(
                sc,
                dbplyr::in_catalog(
                  "strategyunit",
                  "default",
                  "SL_AF_describing_mitigators_2324_local_authority"
                )
              ) |>
                sparklyr::collect() |>
                tidyr::pivot_longer(cols = !c(sex, age_range, resladst_ons, episodes, beddays),
                                    names_to = "cohorts",
                                    values_to = "value") |>
                dplyr::filter(value == 1) |>
                dplyr::summarise(episodes = sum(episodes),
                                 beddays = sum(beddays),
                                 .by = c(age_range, sex, resladst_ons, cohorts)) |>
                dplyr::rename(ladcode23 = resladst_ons)),
   
   tar_target(
     la_age_sex_standardised_rates_episodes,
     generating_la_age_sex_standardised_rates(
       la_numbers,
       la_population_data |> 
         dplyr::mutate(sex = as.character(sex)) |> 
         dplyr::filter(fyear == "2023/24"),
       standard_england_pop_2021_census,
       episodes
     )
   ),
 tar_target(
   la_age_sex_standardised_rates_beddays,
   generating_la_age_sex_standardised_rates(
     la_numbers,
     la_population_data |> 
       dplyr::mutate(sex = as.character(sex)) |> 
       dplyr::filter(fyear == "2023/24"),
     standard_england_pop_2021_census,
     beddays
   )
  ),
   
  # Descriptive analysis -------------------------------------------------------
  ## Overview of mitigator -----------------------------------------------------
  tar_target(
    total_beddays_admissions,
    dplyr::tbl(
      sc,
      dbplyr::in_catalog(
        "strategyunit",
        "default",
        "sl_af_describing_mitigators_final_2324_sex"
      )
    ) |>
      dplyr::select(dplyr::starts_with("total")) |>
      dplyr::distinct() |>
      dplyr::summarise(dplyr::across(dplyr::everything(), ~ sum(.))) |>
      sparklyr::collect() |>
      tidyr::pivot_longer(
        cols = dplyr::everything(),
        names_to = "activity_treatment",
        values_to = "total"
      ) |>
      tidyr::separate_wider_delim(activity_treatment, 
                                  "_", 
                                  names = c("a", 
                                            "activity_type", 
                                            "treatment_type")) |>
      dplyr::mutate(activity_type = stringr::str_replace(activity_type,
                                                         "episodes", 
                                                         "admissions"))
  ),
  tarchetypes::tar_map(
    list(mitigator = mitigators_and_mechanisms),
    tar_target(
      overview,
      get_overview_of_mitigator(mitigator,
                                total_beddays_admissions,
                                mitigators_and_mechanisms_treatment_lookup)
    )
  ),
  
  ## Percentage breakdowns -----------------------------------------------------
  tarchetypes::tar_map(
    list(
      mitigator = rep(mitigators_and_mechanisms, 8),
      group = rep(c("age", "ethnicity", "imd", "sex"), each = 66),
      activity_type = rep(rep(c("admissions", "beddays"), each = 33), 4)),
    tar_target(
      perc,
      get_perc_by_group(mitigator, 
                        group, 
                        activity_type)
    )
  ),
  
  ## Rates per 100,000 population ----------------------------------------------
  tarchetypes::tar_map(
    list(
      mitigator = rep(mitigators_and_mechanisms, 2),
      activity_type = rep(c("admissions", "beddays"), each = 33)),
    tar_target(
      rates_age,
      get_rates_by_group(mitigator, "age", pop_by_age, activity_type)
    )
  ),

  tarchetypes::tar_map(
    list(
      mitigator = rep(mitigators_and_mechanisms, 2),
      activity_type = rep(c("admissions", "beddays"), each = 33)),
    tar_target(
      rates_ethnicity,
      get_rates_by_group(mitigator, "ethnicity",  pop_by_ethnicity, activity_type)
    )
  ),

  tarchetypes::tar_map(
    list(
      mitigator = rep(mitigators_and_mechanisms, 2),
      activity_type = rep(c("admissions", "beddays"), each = 33)),
    tar_target(
      rates_imd,
      get_rates_by_group(mitigator, "imd", pop_by_imd, activity_type)
    )
  ),

  tarchetypes::tar_map(
    list(
      mitigator = rep(mitigators_and_mechanisms, 2),
      activity_type = rep(c("admissions", "beddays"), each = 33)),
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
      dplyr::rename(specialty = treatment_function_title, specialty_group = group)
  ),
  tarchetypes::tar_map(
    list(
      mitigator = rep(mitigators_and_mechanisms, 2),
      activity_type = rep(c("admissions", "beddays"), each = 33)),
    tar_target(
      specialties_top_ten,
      get_top_ten_specialties(mitigator, specialty_key, activity_type)
    )
  ),
 
 ## Length of Stay -------------------------------------------------------------
 tarchetypes::tar_map(
   list(mitigator = mitigators_and_mechanisms),
   tar_target(
     perc_los,
     get_perc_by_los(mitigator)
   )
 ),
 
  # Cohort analysis ------------------------------------------------------------
  
  tar_target(
    total_cohort_numbers_2324,
    Formatting_data_for_cohort_overlap("sl_af_describing_mitigators_fyear")|>
      filter(fyear=="202324")
  ),
 

 # Trends analysis -------------------------------------------------------------

tar_target(
  numbers_over_time,
  Formatting_data_for_trends_analysis_cohorts( "sl_af_describing_mitigators_fyear" , icb_population_data)
),

tar_target(
  numbers_over_time_total_mitigation,
  Formatting_data_for_trends_analysis_total_mitigation( "sl_af_describing_mitigators_fyear", icb_population_data )
),


tar_target(
  denominator_over_time,
  Formatting_data_for_trends_analysis_denominator( "sl_af_total_elective_emergency_activity", icb_population_data )
),

# Comparative analysis ---------------------------------------------------------
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
      dplyr::summarise(dplyr::across(dplyr::starts_with("total"), ~ sum(.)),
                       .by = icb) |>
      sparklyr::collect()
  ),
  tar_target(
    summary_frail_elderly_high_icb_admissions,
    get_summary_by_geography(
      icb_age_sex_standardised_rates_episodes,
      "frail_elderly_high",
      total_beddays_admissions_by_icb,
      "admissions",
      "emergency",
      "icb")),  
  tar_target(
    summary_frail_elderly_high_icb_beddays,
    get_summary_by_geography(
      icb_age_sex_standardised_rates_beddays,
      "frail_elderly_high",
      total_beddays_admissions_by_icb,
      "beddays",
      "emergency",
      "icb")),

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
      sparklyr::collect()),
  tar_target(
    summary_frail_elderly_high_la_admissions,
    get_summary_by_geography(
      la_age_sex_standardised_rates_episodes,
      "frail_elderly_high",
      total_beddays_admissions_by_la,
      "admissions",
      "emergency",
      "ladcode23")
  ),
  tar_target(
    summary_frail_elderly_high_la_beddays,
    get_summary_by_geography(
      la_age_sex_standardised_rates_beddays,
      "frail_elderly_high",
      total_beddays_admissions_by_la,
      "beddays",
      "emergency",
      "ladcode23")
  )
)
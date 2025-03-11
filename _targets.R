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
    "scales"
  )
)

# Connection to Databricks
sc <- sparklyr::spark_connect(
  master     = Sys.getenv("DATABRICKS_HOST"),
  cluster_id = Sys.getenv("DATABRICKS_CLUSTER_ID"),
  token      = Sys.getenv("DATABRICKS_TOKEN"),
  method     = "databricks_connect",
  envname    = Sys.getenv("DATABRICKS_ENVNAME")
)

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
  
 tar_target(england_age_sex_standardised_rates_episodes,
            generating_england_age_sex_standardised_rates(numbers_over_time, icb_population_data, standard_england_pop_2021_census, episodes)
 ),
 
 tar_target(england_age_sex_standardised_rates_beddays,
            generating_england_age_sex_standardised_rates(numbers_over_time, icb_population_data, standard_england_pop_2021_census, beddays)
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
  tar_target(
    overview_frail_elderly_high,
    get_overview_of_mitigator("emergency", "frail_elderly_high == 1", total_beddays_admissions)
  ),
  
  ## Percentage breakdowns -----------------------------------------------------
  tarchetypes::tar_map(
    list(group = rep(
      c("age", "ethnicity", "imd", "sex"), 2), 
      activity_type = rep(c("admissions", "beddays"), each = 4)),
    tar_target(
      perc_frail_elderly_high,
      get_perc_by_group(group, "frail_elderly_high == 1", activity_type)
    )
  ),
  
  ## Rates per 100,000 population ----------------------------------------------
  tarchetypes::tar_map(
    list(activity_type = c("admissions", "beddays")),
    tar_target(
      rates_frail_elderly_high_age,
      get_rates_by_group("age", "frail_elderly_high == 1", pop_by_age, activity_type)
    )
  ),
  
  tarchetypes::tar_map(
    list(activity_type = c("admissions", "beddays")),
    tar_target(
      rates_frail_elderly_high_ethnicity,
      get_rates_by_group("ethnicity", "frail_elderly_high == 1", pop_by_ethnicity, activity_type)
    )
  ),
  
  tarchetypes::tar_map(
    list(activity_type = c("admissions", "beddays")),
    tar_target(
      rates_frail_elderly_high_imd,
      get_rates_by_group("imd", "frail_elderly_high == 1", pop_by_imd, activity_type)
    )
  ),
  
  tarchetypes::tar_map(
    list(activity_type = c("admissions", "beddays")),
    tar_target(
      rates_frail_elderly_high_sex,
      get_rates_by_group("sex", "frail_elderly_high == 1", pop_by_sex, activity_type)
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
    list(activity_type = c("admissions", "beddays")),
    tar_target(
      frail_elderly_high_specialties_top_ten,
      get_top_ten_specialties("frail_elderly_high == 1", specialty_key, activity_type)
    )
  ),
 
 ## Length of Stay -------------------------------------------------------------
 tar_target(
   perc_frail_elderly_high_los,
   get_perc_by_los("frail_elderly_high == 1")
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
tar_target(summary_frail_elderly_high_icb_admissions,
            get_summary_by_icb(icb_age_sex_standardised_rates_episodes,
                               "frail_elderly_high",
                                total_beddays_admissions_by_icb,
                                "admissions",
                                "emergency")),
  tar_target(summary_frail_elderly_high_icb_beddays,
           get_summary_by_icb(icb_age_sex_standardised_rates_beddays,
                              "frail_elderly_high",
                              total_beddays_admissions_by_icb,
                              "beddays",
                              "emergency"))
)
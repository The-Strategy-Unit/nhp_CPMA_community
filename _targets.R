# Targets for nhp_community_mitigators analysis.

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(packages = c(
  "dbplyr",
  "dplyr",
  "janitor",
  "readxl",
  "sparklyr",
  "tibble",
  "tidyr"
))

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
  # Data wrangling
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
  
  # Data from Databricks -------------------------------------------------------
  tarchetypes::tar_map(
    list(
      group = c("age", "ethnicity", "icb", "imd", "los", "mainspef", "sex")
    ),
    tar_target(
      perc_spells_frail,
      get_perc_spells_by_group(group, "frail_elderly_high == 1")
    )
  ),
  tar_target(
    specialty_url,
    r"{https://digital.nhs.uk/binaries/content/assets/website-assets/isce/dcb0028/0028452019codelistspecificationv1.2.xlsx}"
  ),
  tar_target(specialty_key, scrape_xls(specialty_url, sheet = 3)),
  tar_target(
    total_beddays_episodes,
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
        names_to = "metric_type",
        values_to = "total"
      ) |>
      tidyr::separate_wider_delim(metric_type, 
                                  "_", 
                                  names = c("a", "metric", "type"))
  ),
  tar_target(total_beddays_episodes_frail,
             get_perc_spells_beddays("emergency", 
                                     "frail_elderly_high == 1", 
                                     total_beddays_episodes)),
  
  
  tar_target(
    total_cohort_numbers,
    Formatting_data_for_cohort_overlap("sl_af_describing_mitigators_final_2324_sex")
  )
)

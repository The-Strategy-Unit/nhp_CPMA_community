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
  envname    = "r-reticulate" # "r-sparklyr-databricks-15.4" #
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
    list(category = c("age", "sex")),
    tar_target(perc_episodes_frail,
               get_perc_episodes_by_group(category, "frail_elderly_high == 1"))
  )
)

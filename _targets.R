# Targets for nhp_community_mitigators analysis.

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
  packages = c("tibble", "dplyr", "tidyr", "janitor","readxl") 
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# tar_source("other_functions.R") # Source other scripts as needed.

# Replace the target list below with your own:
list(
  
  # Data wrangling
  tar_target(
    la_population_data,
    wrangling_la_population_data("Z:/Strategic Analytics/Projects 2025/Describing and Quantifying the NHP mitigators/population_data/myebtablesenglandwales20112023.xlsx"
)),

tar_target(
  icb_population_data,
  wrangling_icb_population_data("Z:/Strategic Analytics/Projects 2025/Describing and Quantifying the NHP mitigators/population_data/sapeicb202420112022.xlsx",
                                "Z:/Strategic Analytics/Projects 2025/Describing and Quantifying the NHP mitigators/population_data/Integrated_Care_Boards_(December_2024)_Names_and_Codes_in_EN.csv"
  ))
  
  
)

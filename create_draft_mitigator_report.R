# This is a script to create a draft `.qmd` file for each mitigator and mechanism.
# Once drafts are created, we can tweak each file individually and add narrative.

# Building blocks --------------------------------------------------------------
## Functions -------------------------------------------------------------------
get_cohort_title <- function(mitigator, summary) {
  title <- summary |>
    dplyr::filter(mitigator_code == mitigator) |>
    dplyr::pull(mitigator_name)
  
  return(title)
}

get_filename <- function(mitigator) {
  type <- if (mitigator %in% c(
    "redirection_subsititution",
    "prevention",
    "efficiencies",
    "efficiencies_relocation"
  )) {
    "mechanism"
  } else {
    "mitigator"
  }
  
  filename <- glue::glue("DRAFT_{type}_{mitigator}.qmd") # will remove draft prefix after testing
  
  return(filename)
}

get_global_variables <- function(mitigator,
                                 summary_table,
                                 treatment_lookup) {
  treatment_type <- get_treatment_type(mitigator, treatment_lookup)
  
  code <- c(
    "```{r global_variables}",
    paste0("cohort <- \"", mitigator, "\""),
    paste0(
      "cohort_title <- \"",
      get_cohort_title(mitigator, summary_table),
      "\""
    ),
    paste0("treatment_type <- \"", treatment_type, "\""),
    paste0(
      "treatment_type_title <- format_treatment_for_caption(\"",
      treatment_type,
      "\")"
    ),
    "```",
    ""
  )
  
  return(code)
}

get_treatment_type <- function(mitigator, lookup) {
  treatment_type <- lookup |>
    dplyr::filter(mitigator_or_mechanism == mitigator) |>
    dplyr::pull(treatment_type)
  
  return(treatment_type)
}

rename_admissions_as_episodes <- function(activity_type) {
  renamed_activity_type <- if (activity_type == "admissions") {
    "episodes"
  } else {
    activity_type
  }
  
  return(renamed_activity_type)
}

## Strings ---------------------------------------------------------------------

admission_characteristics_section <- c(
  "### Admission Characteristics",
  "",
  "In this section, mitigable activity for the `r cohort_title` cohort are shown by admission characteristics.",
  "",
  "```{r}",
  "#| output: asis",
  "knitr::knit_child(
    input = \"child-dir/_child-descriptive_admission-characteristics.qmd\",
    envir = environment(),
    quiet = TRUE
  ) |>
  cat(sep = '\\n')",
  "```",
  ""
)

comparative_section <- c(
  "### Integrated Care Board",
  "",
  "```{r}",
  "#| output: asis",
  "knitr::knit_child(
                  input = \"child-dir/_child-comparative_icb.qmd\",
                  envir = environment(),
                  quiet = TRUE
                ) |>
                  cat(sep = '\\n')",
  "```",
  "",
  "### Local Authority",
  "",
  "```{r}",
  "#| output: asis",
  "knitr::knit_child(
                  input = \"child-dir/_child-comparative_local-authority.qmd\",
                  envir = environment(),
                  quiet = TRUE
                ) |>
                  cat(sep = '\\n')",
  "```",
  ""
)

data <- c(
  "```{r data}",
  "#| output: false",
  "mitigator_summary_table <- readxl::read_excel(\"summary_mitigators_table.xlsx\") |>
  dplyr::mutate(mechanism = snakecase::to_snake_case(mechanism))",
  "",
  "england_age_sex_standardised_rates_episodes <- tar_read(england_age_sex_standardised_rates_episodes) |>
    filter(cohorts == cohort)",
  "",
  "england_age_sex_standardised_rates_beddays <- tar_read(england_age_sex_standardised_rates_beddays) |>
    filter(cohorts == cohort)",
  "",
  "total_beddays_admissions <- targets::tar_read(total_beddays_admissions)",
  "",
  "icb_23_shp <- sf::st_read(\"https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Integrated_Care_Boards_April_2023_EN_BGC/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson\") |>
    janitor::clean_names() |>
    dplyr::mutate(icb23nm = simplify_icb_name(icb23nm))",
  "```",
  ""
)

overview_section <-  c(
  "```{r}",
  "#| output: asis",
  "knitr::knit_child(
      input = \"child-dir/_child-descriptive_overview.qmd\",
      envir = environment(),
      quiet = TRUE
    ) |>
      cat(sep = '\\n')",
  "```",
  ""
)

packages_and_options <- c(
  "```{r packages_and_options}",
  "library(\"flextable\")",
  "library(\"tidyr\")",
  "library(\"dplyr\")",
  "library(\"ggplot2\")",
  "library(\"StrategyUnitTheme\")",
  "library(\"forcats\")",
  "library(\"ComplexUpset\")",
  "library(\"scales\")",
  "library(\"targets\")",
  "library(\"plotly\")",
  "library(\"patchwork\")",
  "library(\"DT\")",
  "",
  "source(\"R/captions.R\")",
  "source(\"R/general.R\")",
  "source(\"R/descriptive_analyses.R\")",
  "source(\"R/comparative_analyses.R\")",
  "source(\"R/Functions for cohort overlap.R\")",
  "source(\"R/Functions for trend analysis.R\")",
  "",
  "options(scipen = 999)",
  "options(knitr.duplicate.label = \"allow\")",
  "```",
  ""
)

patient_characteristics_section <- c(
  "### Patient Characteristics",
  "",
  "In this section, the number and percentage of mitigable activity for the `r cohort_title` cohort are shown by patient characteristics: age, ethnic category, IMD decile and sex. Crude rates per 100,000 population are also provided for context.",
  "",
  "Outputs are presented for both admissions and beddays, but patterns are broadly similar across the two.",
  "",
  "::: panel-tabset",
  
  "#### Age",
  "```{r}",
  "patient_characteristic <- \"age\"",
  "patient_characteristic_col_name <- \"age_range\"",
  "```",
  "",
  "```{r}",
  "#| output: asis",
  "knitr::knit_child(
              input = \"child-dir/_child-descriptive_patient-characteristics.qmd\",
              envir = environment(),
              quiet = TRUE
             ) |>
            cat(sep = '\\n')",
  "```",
  "",
  
  "#### Ethnicity",
  "```{r}",
  "patient_characteristic <- \"ethnicity\"",
  "patient_characteristic_col_name <- \"Ethnic_Category\"",
  "```",
  "",
  "```{r}",
  "#| output: asis",
  "knitr::knit_child(
              input = \"child-dir/_child-descriptive_patient-characteristics.qmd\",
              envir = environment(),
              quiet = TRUE
             ) |>
            cat(sep = '\\n')",
  "```",
  "",
  
  "#### IMD Decile",
  "```{r}",
  "patient_characteristic <- \"imd\"",
  "patient_characteristic_col_name <- \"imd19_decile\"",
  "```",
  "",
  "```{r}",
  "#| output: asis",
  "knitr::knit_child(
              input = \"child-dir/_child-descriptive_patient-characteristics.qmd\",
              envir = environment(),
              quiet = TRUE
             ) |>
            cat(sep = '\\n')",
  "```",
  "",
  
  "#### Sex",
  "```{r}",
  "patient_characteristic <- \"sex\"",
  "patient_characteristic_col_name <- \"sex\"",
  "```",
  "",
  "```{r}",
  "#| output: asis",
  "knitr::knit_child(
              input = \"child-dir/_child-descriptive_patient-characteristics.qmd\",
              envir = environment(),
              quiet = TRUE
             ) |>
            cat(sep = '\\n')",
  "```",
  "",
  ":::",
  ""
)

# Function for creating draft reports ------------------------------------------
create_draft_mitigator_qmd <- function(mitigator,
                                       my_file,
                                       summary_table,
                                       treatment_lookup) {
  code <- cat(
    paste0("## ", get_cohort_title(mitigator, summary_table), " {#sec-", cohort, " .unnumbered}"),
    "",
    packages_and_options,
    get_global_variables(mitigator, summary_table, treatment_lookup),
    data,
    
    "## Descriptive Analysis",
    "",
    overview_section,
    patient_characteristics_section,
    admission_characteristics_section,
    "",
    
    "## Comparative Analysis",
    "",
    comparative_section,
    
    sep = "\n",
    append = FALSE,
    file = my_file
  )
  
  return(code)
}

# Mitigator details ------------------------------------------------------------
# This is a copy of what's at the top of the targets pipeline
mitigator_summary_table <-
  readxl::read_excel("summary_mitigators_table.xlsx") |>
  dplyr::mutate(mechanism = snakecase::to_snake_case(mechanism))

mitigators <- mitigator_summary_table |>
  dplyr::select(mitigator_or_mechanism = mitigator_code,
                treatment_type = type_of_admission)

mechanisms <- mitigator_summary_table |>
  dplyr::summarise(treatment_type = paste(unique(type_of_admission), collapse = ', '),
                   .by = mechanism) |>
  dplyr::mutate(treatment_type = ifelse(
    stringr::str_detect(treatment_type, "emergency & elective"),
    "both",
    treatment_type
  )) |>
  dplyr::rename(mitigator_or_mechanism = mechanism)

mitigators_and_mechanisms_treatment_lookup <- mitigators |>
  rbind(mechanisms) |>
  dplyr::mutate(treatment_type = stringr::str_replace(treatment_type, "emergency & elective", "both"))

mitigators_and_mechanisms <- mitigators_and_mechanisms_treatment_lookup |>
  dplyr::pull(mitigator_or_mechanism)

# Creating draft quarto reports ------------------------------------------------
# Whilst testing have limited to just one mitigator:
mitigators_and_mechanisms <- c("eol_care_2_days")
# exclude already existing ones?

invisible(purrr::map(
  mitigators_and_mechanisms,
  ~ create_draft_mitigator_qmd(
    .,
    my_file = get_filename(mitigators_and_mechanisms),
    summary_table = mitigator_summary_table,
    treatment_lookup = mitigators_and_mechanisms_treatment_lookup
  )
))

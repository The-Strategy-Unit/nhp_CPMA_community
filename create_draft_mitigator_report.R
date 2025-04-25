# This is a script to create a draft `.qmd` file for each mitigator and mechanism.
# Once drafts are created, we can tweak each file individually and add narrative.

# Building blocks --------------------------------------------------------------
source("R/general.R")

## Functions -------------------------------------------------------------------
get_cohort_overlap_section <- function(mitigator) {
  if (mitigator %in% c(
    "redirection_substitution",
    "prevention",
    "efficiencies",
    "efficiencies_relocation"
  )) {
    
    mechanism <- if (mitigator == "redirection_substitution") {
      "redirection"
    } else if (mitigator == "efficiencies_relocation") {
      "relocation"
    } else {
      mitigator
    }
    
    data <- glue::glue("cohort_overlap_numbers_{mechanism}")
    
    filter <- ""
    
    inclusion_subsection <- ""
  } else {
    data <- "total_cohort_numbers_2324"
    
    filter <- "|>
    filter(!!rlang::sym(cohort) == 1)"
    
    inclusion_subsection <- c(
      "### Inclusion within other mitigable activity cohorts",
      "",
      "```{r}",
      "#| output: asis",
      "knitr::knit_child(
        input = \"child-dir/_child-overlap_inclusion-within-others.qmd\",
        envir = environment(),
        quiet = TRUE
        ) |>
      cat(sep = '\\n')",
      "```",
      ""
    )
  }
  
  code <- c(
    "",
    "```{r}",
    paste0(
      "overlap_numbers <- targets::tar_read(",
      data,
      ") ",
      filter
    ),
    "```",
    "",
    "```{r}",
    "#| output: asis",
    "knitr::knit_child(
      input = \"child-dir/_child-overlap_number-within-others.qmd\",
      envir = environment(),
      quiet = TRUE
      ) |>
    cat(sep = '\\n')",
    "```",
    "",
    
    inclusion_subsection,
    
    "### Most common cohort overlaps",
    "",
    "```{r}",
    "#| output: asis",
    "knitr::knit_child(
      input = \"child-dir/_child-overlap_most-common.qmd\",
      envir = environment(),
      quiet = TRUE
      ) |>
    cat(sep = '\\n')",
    "```",
    ""
  )
  
  return(code)
}

get_cohort_title <- function(mitigator, summary) {
  if (check_if_mechanism(mitigator, summary)) {
    if (mitigator == "efficiencies_relocation") {
      mitigator <- mitigator |>
        stringr::str_replace("_", " & ") |>
        stringr::str_to_title()
    } else if (mitigator == "redirection_substitution") {
      mitigator <- mitigator |>
        stringr::str_replace("_", "/") |>
        stringr::str_to_title()
    }
    
    title <- mitigator |>
      stringr::str_to_title()
  }
  
  else {
    title <- summary |>
      dplyr::filter(mitigator_code == mitigator) |>
      dplyr::pull(mitigator_name)
  }
  
  return(title)
}

get_cohort_group <- function(mitigator, summary) {
  
  if(check_if_mechanism(mitigator, summary)) {
    cohort_group <- "mechanism group"
  } else {
    cohort_group <- "cohort"
  }
  
  return(cohort_group)
}

get_data_section <- function(mitigator, summary_table) {
  standardised_rates_episodes <- if (check_if_efficiency_mitigator(mitigator, summary_table)) {
    ""
  } else {
    c(
      "england_age_sex_standardised_rates_episodes <- tar_read(england_age_sex_standardised_rates_episodes) |>
    filter(cohorts == cohort)",
      ""
    )
  }
  
  standardised_rates_beddays <- if (check_if_zero_los_mitigator(mitigator)) {
    ""
  } else {
    c(
      "england_age_sex_standardised_rates_beddays <- tar_read(england_age_sex_standardised_rates_beddays) |>
    filter(cohorts == cohort)",
      ""
    )
  }
  
  code <- c(
    "```{r data}",
    "#| output: false",
    "mitigator_summary_table <- readxl::read_excel(\"summary_mitigators_table.xlsx\") |>
  dplyr::mutate(mechanism = snakecase::to_snake_case(mechanism))",
    "",
    standardised_rates_episodes,
    standardised_rates_beddays,
    "total_beddays_admissions <- targets::tar_read(total_beddays_admissions)",
    "",
    "icb_23_shp <- sf::st_read(\"https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Integrated_Care_Boards_April_2023_EN_BSC/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson\") |>
    janitor::clean_names() |>
    dplyr::mutate(icb23nm = simplify_icb_name(icb23nm))",
    "```",
    ""
  )
  
  return(code)
}

get_filename <- function(mitigator) {
  type <- if (mitigator %in% c(
    "redirection_substitution",
    "prevention",
    "efficiencies",
    "efficiencies_relocation"
  )) {
    "mechanism"
  } else {
    "mitigator"
  }
  
  filename <- glue::glue("{type}_{mitigator}.qmd")
  
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
    "",
    paste0("treatment_type <- \"", treatment_type, "\""),
    paste0("cohort_group <- \"", get_cohort_group(mitigator, summary_table), "\""),
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

## Strings ---------------------------------------------------------------------
admission_characteristics_section <- c(
  "### Admission Characteristics",
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
  "```{r}",
  "#| output: asis",
  "knitr::knit_child(
                  input = \"child-dir/_child-comparative.qmd\",
                  envir = environment(),
                  quiet = TRUE
                ) |>
                  cat(sep = '\\n')",
  "```",
  ""
)

including_activity_types <- c(
  "```{r including_activity_types}",
  "include_admissions <- check_include_admissions(cohort, mitigator_summary_table)",
  "include_beddays <- check_include_beddays(cohort)",
  "include_admissions_and_beddays <- include_admissions & include_beddays",
  "activity_type_label <- get_activity_type_label(include_admissions, include_beddays)",
  "```",
  ""
)

overview_section <-  c(
  "```{r}",
  "#| output: asis",
  "knitr::knit_child(
      input = \"child-dir/_child-overview.qmd\",
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
  "source(\"R/manipulating_mitigators_and_mechanisms.R\")",
  "",
  "options(scipen = 999)",
  "options(knitr.duplicate.label = \"allow\")",
  "```",
  ""
)

patient_characteristics_section <- c(
  "### Patient Characteristics",
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
  ""
)

trend_section <- c(
  
  "",
  "```{r}",
  "#| output: asis",
  "knitr::knit_child(
    input = \"child-dir/_child-trends_england.qmd\",
    envir = environment(),
    quiet = TRUE
    ) |>
  cat(sep = '\\n')",
  "```",
  "",
  
  "### Integrated Care Board",
  "",
  "```{r}",
  "#| output: asis",
  "knitr::knit_child(
    input = \"child-dir/_child-trends_icb.qmd\",
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
    input = \"child-dir/_child-trends_local-authority.qmd\",
    envir = environment(),
    quiet = TRUE
    ) |>
  cat(sep = '\\n')",
  "```",
  ""
)

# Function for creating draft reports ------------------------------------------
create_draft_mitigator_qmd <- function(mitigator,
                                       my_file,
                                       summary_table,
                                       treatment_lookup) {
  code <- cat(
    paste0(
      "## ",
      get_cohort_title(mitigator, summary_table),
      " {#sec-",
      mitigator,
      " .unnumbered}"
    ),
    "",
    packages_and_options,
    get_global_variables(mitigator, summary_table, treatment_lookup),
    get_data_section(mitigator, summary_table),
    including_activity_types,
    "`r get_mitigator_description(cohort)`",
    "",
    overview_section,
    
    "## Descriptive Analysis",
    "",
    patient_characteristics_section,
    admission_characteristics_section,
    
    "## Cohort overlap",
    "",
    get_cohort_overlap_section(mitigator),
    
    "## Comparative Analysis",
    "",
    comparative_section,
    
    "## Trend Analysis",
    "",
    trend_section,
    
    sep = "\n",
    append = FALSE,
    file = my_file
  )
  
  return(code)
}

# Mitigator details ------------------------------------------------------------
# This is a copy of what's at the top of the targets pipeline
source("R/manipulating_mitigators_and_mechanisms.R")

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
invisible(purrr::map(
  mitigators_and_mechanisms,
  ~ create_draft_mitigator_qmd(
    .,
    my_file = get_filename(.),
    summary_table = mitigator_summary_table,
    treatment_lookup = mitigators_and_mechanisms_treatment_lookup
  )
))


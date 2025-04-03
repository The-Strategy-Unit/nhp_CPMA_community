# Functions used for general tasks, like creating tables.

#' Convert a dataframe into a datatable.
#'
#' @param A dataframe.
#'
#' @return A table.
create_dt <- function(x) {
  DT::datatable(
    x,
    rownames = FALSE,
    options = list(
      dom = "Blfrtip",
      lengthChange = FALSE,
      autoWidth = TRUE,
      #paging = FALSE,
      bInfo = FALSE,
      class = 'cell-border stripe',
      columnDefs = list(list(
        className = 'dt-left', targets = 0
      )),
      lengthMenu = list(c(10, 25, 50, -1), c(10, 25, 50, "All"))
    )
  )
}

#' Filter a dataframe to data for a mitigator or mechanism.
#'
#' @param data A dataframe.
#' @param mitigator The mitigator or mechanism.
#'
#' @return A dataframe.
filter_to_mitigator_or_mechanism <- function(data, mitigator) {
  filtered <- data |>
    mutate_mechanism_columns() |>
    dplyr::filter(!!rlang::sym(mitigator) == 1)
  
  return(filtered)
}

#' Formats a string into a title for tables and plots.
#'
#' @param col_name A string.
#'
#' @return A string.
format_as_title <- function(col_name) {
  title <- col_name |>
    stringr::str_replace("_", " ") |>
    stringr::str_to_title() |>
    stringr::str_replace_all(
      c(
        "Imd19" = "IMD",
        "Perc" = "Percentage",
        "Pop" = "Population",
        "Lowercl" = "Lower CL",
        "Uppercl" = "Upper CL",
        "Los Range" = "LOS Range",
        "Icb" = "ICB",
        "Total Episodes_emergency" = "Total Emergency Admissions",
        "Total Episodes_elective" = "Total Elective Admissions",
        "Total Beddays_emergency" = "Total Emergency Beddays",
        "Total Beddays_elective" = "Total Elective Beddays",
        "Total Episodes_both" = "Total Admissions",
        "Total Beddays_both" = "Total Beddays",
        "Mitigable Episodes" = "Mitigable Admissions"
      )
    )
  
  return(title)
}

#' Get the column name from the group.
#'
#' In Databricks, the grouped tables have a suffix to indicate the group, but
#' this does not always match the name of the column that indicates which group
#' the row belongs to. For example, the table split by age has the suffix "age",
#' but the column name is "age_range". This function is to get the associated
#' column name from the group so in other functions only the group needs to be
#' provided.
#'
#' @param group A string of the group that the table is for.
#'
#' @return A string.
get_col_name_from_group <- function(group) {
  col_name <- if (group == "age") {
    "age_range"
  } else if (group == "ethnicity") {
    "Ethnic_Category"
  } else if (group == "imd") {
    "imd19_decile"
  } else if (group == "los") {
    "los_range"
  } else {
    group
  }
  
  return(col_name)
}

#' Formats data into a standard table.
#'
#' @param data A dataframe.
#'
#' @return A formatted table.
get_table <- function(data) {
  flextable::set_flextable_defaults(digits = 2)
  
  table <- data |>
    flextable::as_flextable(
      hide_grouplabel = TRUE,
      max_row = 50,
      show_coltype = FALSE
    ) |>
    flextable::align(part = "header", align = "center") |>
    flextable::bg(bg = "#f9bf07", part = "header") |>
    flextable::bold(bold = TRUE, part = "header") |>
    flextable::fontsize(size = 12, part = "all") |>
    flextable::padding(padding = 2,
                       part = "all",
                       padding.top = NULL) |>
    flextable::autofit()
  
  return(table)
}

#' Add columns flagging if a row is part of each mechanism.
#'
#' @param data A dataframe with columns for each mitigator. 
#'
#' @return A dataframe.
mutate_mechanism_columns <- function(data) {
  wrangled <- data |>
    dplyr::mutate(
      prevention = ifelse(
        alcohol_partially_attributable_acute == 1 |
          alcohol_partially_attributable_chronic == 1 |
          alcohol_wholly_attributable == 1 |
          obesity_related_admissions == 1 |
          smoking == 1 |
          raid_ae == 1 |
          intentional_self_harm == 1 |
          medically_unexplained_related_admissions == 1,
        1,
        0
      ),
      redirection_substitution = ifelse(
        ambulatory_care_conditions_acute == 1 |
          ambulatory_care_conditions_chronic == 1 |
          ambulatory_care_conditions_vaccine_preventable == 1 |
          eol_care_2_days == 1 |
          eol_care_3_to_14_days == 1 |
          falls_related_admissions == 1 |
          frail_elderly_high == 1 |
          frail_elderly_intermediate == 1 |
          medicines_related_admissions_explicit == 1 |
          medicines_related_admissions_implicit_anti_diabetics == 1 |
          medicines_related_admissions_implicit_benzodiasepines == 1 |
          medicines_related_admissions_implicit_diurectics == 1 |
          medicines_related_admissions_implicit_nsaids == 1 |
          readmission_within_28_days == 1 |
          zero_los_no_procedure_adult == 1 |
          zero_los_no_procedure_child == 1,
        1,
        0
      ),
      efficiencies_relocation = ifelse(
        virtual_wards_activity_avoidance_ari == 1 |
          virtual_wards_activity_avoidance_heart_failure == 1,
        1,
        0
      ),
      efficiencies = ifelse(
        emergency_elderly == 1 |
          stroke_early_supported_discharge == 1 |
          raid_ip == 1,
        1,
        0
      )
    )
  
  return(wrangled)
}

#' Order the levels of factor variables.
#'
#' If a dataframe contains one a column for imd19_decile, age_range or
#' los_range, that column will be converted to a factor with the levels ordered
#' as below.
#'
#' @param data A dataframe.
#'
#' @return A dataframe.
order_levels_of_factors <- function(data) {
  wrangled <- data |>
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of("imd19_decile"),
        ~ factor(., levels = as.character(1:10))
      ),
      dplyr::across(dplyr::any_of("los_range"), ~ factor(
        ., levels = c("0", "1", "2", "3", "4-7", "8-14", "15-21", "22+")
      )),
      dplyr::across(dplyr::any_of("age_range"), ~ factor(
        .,
        levels = c(
          "0-4",
          "5-9",
          "10-14",
          "15-19",
          "20-24",
          "25-29",
          "30-34",
          "35-39",
          "40-44",
          "45-49",
          "50-54",
          "55-59",
          "60-64",
          "65-69",
          "70-74",
          "75-79",
          "80-84",
          "85-89",
          "90+"
        )
      ))
    )
  
  return(wrangled)
}

#' Scrape excel data from URL.
#'
#' @param url The URL where the excel file is.
#' @param sheet Default is to read sheet 1 but can specify other sheet number.
#' @param skip Default is to read all rows, but can specify number of rows to
#' skip.
#'
#' @return A dataframe.
scrape_xls <- function(url, sheet = 1, skip = 0) {
  tmp <- tempfile(fileext = "")
  
  download.file(url = url,
                destfile = tmp,
                mode = "wb")
  
  data <- readxl::read_excel(path = tmp,
                             sheet = sheet,
                             skip = skip) |>
    janitor::clean_names()
  
  return(data)
  
}

#' Simplify ICB names.
#'
#' @param column The ICB name column.
#'
#' @return A column of simplified ICB names.
simplify_icb_name <- function(column) {
  simplified <- column |>
    stringr::str_remove(" Integrated Care Board") |>
    stringr::str_remove("NHS ")
  
  return(simplified)
}
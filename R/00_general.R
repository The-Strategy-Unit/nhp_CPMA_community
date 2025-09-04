# Functions used for general tasks, like creating tables.


#' Adds a new column for age range.
#'
#' @param data A dataframe with an `age` column.
#'
#' @returns A dataframe.
add_age_range_column <- function(data) {
  wrangled <- data |>
    dplyr::mutate(age_range = dplyr::case_when(
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
    ))
  
  return(wrangled)
  
}

#' Adds grey and labelled box for COVID-19 pandemic.
#'
#' @param plot A ggplot.
#' @param max_y The max y value.
#'
#' @returns A plot with COVID-19 box added.
add_covid_box_to_plot <- function(plot, max_y){
  
  annotated <- plot +
    ggplot2::geom_rect(
      ggplot2::aes(NULL, NULL, xmin = "2019/20", xmax = "2021/22"),
      ymin = 0,
      ymax = max_y * 1.1,
      fill = "#686f73",
      size = 0.5,
      alpha = 0.01
    ) +
    ggplot2::annotate(
      "text",
      x = "2020/21",
      y = max_y * 1.08,
      label = "COVID-19 pandemic",
      size = 2.7
    )
  
  return(annotated)
}

#' Adds new column for year by converting financial year from YYYYYY to YYYY/YY.
#'
#' @param data A dataframe with a column for `fyear`.
#'
#' @returns A dataframe.
add_year_column <- function(data) {
  wrangled <- data |>
    dplyr::mutate(
      year = paste0(
        stringr::str_sub(fyear, 1, 4),
        "/",
        stringr::str_sub(fyear, 5, 6)
      )
    )
  
  return(wrangled)
}

#' Convert a dataframe into a datatable.
#'
#' @param df A dataframe.
#'
#' @return A table.
create_dt <- function(df) {
  # Identify the factor columns
  factor_cols <- sapply(df, is.factor)
  if (any(factor_cols)) {
    # Create a list of column definitions for factor columns
    factor_defs <- do.call(c, unname(Map(
      function(orig, lvl)
        list(
          list(targets = orig, orderData = lvl),
          list(targets = lvl, visible = FALSE)
        ),
      which(factor_cols) - 1,
      ncol(df) + seq_len(sum(factor_cols)) - 1
    )))
    df <- cbind(df, lapply(df[, factor_cols, drop = FALSE], function(z)
      match(z, levels(z))))
  } else
    factor_defs <- NULL
  
  # Create the datatable with column definitions
  DT::datatable(
    df,
    extensions = "Buttons",
    options = list(dom = "Blfrtip",
                   orderClasses = TRUE, 
                   columnDefs = factor_defs,
                   buttons = c("csv")),
    rownames = FALSE,
    class = 'cell-border stripe'
  )
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
        "Mitigable Episodes" = "Mitigable Admissions",
        "Avg Los" = "Average LOS"
      )
    )
  
  return(title)
}

#' Keeps only first three characters of diagnoses codes.
#'
#' @param data A dataframe.
#' @param group A string of the group that the table is for. 
#'
#' @returns A dataframe.
format_diagnoses_codes <- function(data, group) {
  if (group == "diagnosis") {
    formatted <- data |>
      dplyr::mutate(primary_diagnosis = ifelse(
        # want to use first three characters, but also to distinguish vaping from covid:
        primary_diagnosis == "U07.0",
        "U07.0", 
        stringr::str_sub(primary_diagnosis, start = 1, end = 3)
      ))
    
  } else {
    formatted <- data
  }
  
  return(formatted)
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
  } else if (group == "diagnosis"){
    "primary_diagnosis"
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

#' Get Databricks table name.
#'
#' @param group A string of the group that the table is for.
#'
#' @returns A string.
get_table_name <- function(group){
  
  if(group == "diagnosis"){
    table_name <- paste0("sl_af_describing_mitigators_", group)
  } else {
    table_name <- paste0("sl_af_describing_mitigators_final_2324_", group)
  }
  
  return(table_name)
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
      dplyr::across(dplyr::any_of("los_range2"), ~ factor(
        ., levels = c("0", "1", "2-7", "8-14", "15+")
      )),
      dplyr::across(dplyr::any_of("los_range3"), ~ factor(
        ., levels = c("0", "1", "2", "3-7", "8-14", "15+")
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

#' Renames `"admissions"` to `"episodes"`.
#'
#' @param activity_type A string.
#'
#' @return A string.
rename_admissions_as_episodes <- function(activity_type) {
  renamed_activity_type <- if (activity_type == "admissions") {
    "episodes"
  } else {
    activity_type
  }
  
  return(renamed_activity_type)
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

#' Simplify provider names.
#'
#' @param column The provider name column.
#'
#' @return A column of simplified provider names.
simplify_provider_name <- function(column) {
  simplified <- column |>
    stringr::str_to_title() |>
    stringr::str_replace_all(c(" Nhs" = " NHS ",
                               " Foundation" = "F",
                               " Trust" = "T"))
  
  return(simplified)
}

#' Suppress small numbers in descriptive analyses or comparative LA table.
#'
#' @param data A dataframe.
#' @param number_column A string of the column name with numbers that may need
#' suppressing.
#' @param limit An integer. Default is 10.
#'
#' @return A dataframe.
small_number_suppression <- function(data, number_column, limit = 10) {
  suppressed <- data |>
    dplyr::mutate(!!rlang::sym(number_column) := ifelse(
      !!rlang::sym(number_column) <= limit,
      glue::glue("<={limit}"),
      !!rlang::sym(number_column)
    ))
  
  count <- suppressed |>
    dplyr::filter(stringr::str_detect(!!rlang::sym(number_column), "<=")) |>
    nrow()
  
  suppressed <- if (count == 0) {
    suppressed
  } else if (count == 1) {
    suppressed |>
      dplyr::select(-perc)
  } else if (count > 1) {
    suppressed |>
      dplyr::mutate(perc := ifelse(stringr::str_detect(!!rlang::sym(number_column), "<="), "-", perc))
  }
  
  return(suppressed)
}

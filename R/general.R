# Functions used for general tasks, like creating tables.

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
    options = list(orderClasses = TRUE, columnDefs = factor_defs),
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
      !!rlang::sym(number_column) <= limit & !!rlang::sym(number_column) != 0,
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

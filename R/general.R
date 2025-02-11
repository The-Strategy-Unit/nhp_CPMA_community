# Functions used for general tasks, like creating tables.

#' Formats a string into a title for tables and plots.
#'
#' @param col_name A string.
#'
#' @return A string.
format_as_title <- function(col_name) {
  title <- col_name |>
    stringr::str_replace("_", " ") |>
    stringr::str_to_title() |>
    stringr::str_replace_all(c("Imd19" = "IMD",
                               "Percentage" = "Perc"))
  
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
      max_row = 20,
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

#' Formats percentage data into a table.
#'
#' @param data A dataframe of percentage data.
#'
#' @return A table.
#' 
#' @examples
#' \dontrun{
#' targets::tar_read(perc_episodes_frail_age) |> 
#'   get_table_perc()} 
get_table_perc <- function(data) {
  
  table <- data |>
    dplyr::rename_with(~format_as_title(.)) |>
    dplyr::rename("Percentage" = "Perc") |>
    get_table() |>
    flextable::delete_part(part = "footer")
  
  return(table)
}
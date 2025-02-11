# Functions used for general tasks, like creating tables. These functions are 
# mostly used in the quarto report, not targets.

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
    dplyr::rename_with(~ stringr::str_replace(., "_", " ") |>
                         stringr::str_to_title()) |>
    dplyr::rename("Percentage" = "Perc") |>
    get_table() |>
    flextable::delete_part(part = "footer")
  
  return(table)
}
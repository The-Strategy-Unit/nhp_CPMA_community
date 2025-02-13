# Functions used to complete the descriptive analyses.

#' Get the percentage of mitigable episodes by a group.
#' 
#' Given a `group` (for example, `sex` or `age`), this function will get the 
#' table from Databricks, filter to the mitigator or set of mitigators using 
#' the `condition` provided and calculate the number and percentage of mitigable 
#' episodes by group.
#'
#' @param group A string of the group that the table is for.
#' @param condition An string containing the expression needed to filter for a 
#' mitigator or set of mitigators. 
#' @param connection The Databricks connection.
#'
#' @return A dataframe of the number and percentage of mitigable episodes by 
#' group.
get_perc_episodes_by_group <- function(group, condition, connection = sc) {
  col_name <- get_col_name_from_group(group)
  
  summary <- dplyr::tbl(connection,
                        dbplyr::in_catalog(
                          "strategyunit",
                          "default",
                          paste0("sl_af_describing_mitigators_final_2324_", 
                                 group)
                        )) |>
    dplyr::filter(!!rlang::parse_expr(condition),
                  !is.na(!!rlang::sym(col_name)) # exclude NULLs
                  ) |>
    dplyr::summarise(episodes = sum(episodes), .by = {{col_name}}) |>
    sparklyr::collect() |>
    dplyr::mutate(perc = janitor::round_half_up(episodes * 100 /
                                                  sum(episodes), 2)) |>
    dplyr::arrange(dplyr::across(1))
  
  return(summary)
}

#' Plot the percentage of mitigable episodes by a group.
#'
#' @param data The output of `get_perc_episodes_by_group()`.
#' @param group The group that the data is split by.
#'
#' @return A plot.
get_perc_episodes_by_group_plot <- function(data, col_name) {
  col_name_title <- col_name |>
    format_as_title()
  
  plot <- data |>
    ggplot2::ggplot(ggplot2::aes(!!rlang::sym(col_name), 
                                 perc, 
                                 fill = !!rlang::sym(col_name))) +
    ggplot2::geom_col() +
    StrategyUnitTheme::su_theme() +
    StrategyUnitTheme::scale_fill_su() +
    ggplot2::labs(x = col_name_title, 
                  y = "Percentage of mitigable episodes", 
                  fill = col_name_title)
  
  return(plot)
}

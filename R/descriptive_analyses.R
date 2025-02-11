#' Get the percentage of mitigable episodes by a group.
#' 
#' Given a `category` (for example, `sex` or `age`), this function will get the 
#' table from Databricks, filter to the mitigator or set of mitigators using 
#' the `condition` provided and calculate the number and percentage of mitigable 
#' episodes by group.
#'
#' @param category A string of the category that the table is for.
#' @param condition An string containing the expression needed to filter for a 
#' mitigator or set of mitigators. 
#' @param connection The Databricks connection.
#'
#' @return A dataframe of the number and percentage of mitigable episodes by 
#' group.
get_perc_episodes_by_group <- function(category, condition, connection = sc) {
  group <- if (category == "age") {
    "age_range"
  } else if (category == "ethnicity") {
    "Ethnic_Category"
  } else {
    category
  }
  
  summary <- dplyr::tbl(connection,
                        dbplyr::in_catalog(
                          "strategyunit",
                          "default",
                          paste0("sl_af_describing_mitigators_final_2223_", 
                                 category)
                        )) |>
    dplyr::filter(!!rlang::parse_expr(condition)) |>
    dplyr::summarise(episodes = sum(episodes), .by = {{group}}) |>
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
get_perc_episodes_by_group_plot <- function(data, group) {
  group_title <- group |>
    stringr::str_replace("_", " ") |>
    stringr::str_to_title()
  
  plot <- data |>
    ggplot2::ggplot(ggplot2::aes(!!rlang::sym(group), 
                                 perc, 
                                 fill = !!rlang::sym(group))) +
    ggplot2::geom_col() +
    StrategyUnitTheme::su_theme() +
    StrategyUnitTheme::scale_fill_su() +
    ggplot2::labs(x = group_title, 
                  y = "Percentage of mitigable episodes", 
                  fill = group_title)
  
  return(plot)
}
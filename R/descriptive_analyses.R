# Functions used to complete the descriptive analyses.

#' Get the percentage of mitigable spells for a mitigator.
#'
#' @param activity_type Either `"emergency"` or `"elective"`.
#' @param condition An string containing the expression needed to filter for a 
#' mitigator or set of mitigators. 
#' @param totals A dataframe containing the total beddays and episodes for 2023-24.
#' @param connection The Databricks connection.
#'
#' @return A dataframe.
get_perc_spells_beddays <- function(activity_type,
                                    condition,
                                    totals = total_beddays_episodes,
                                    connection = sc) {
  totals <- totals |>
    dplyr::filter(type == activity_type) |>
    dplyr::select(metric, total)
  
  mitigator_totals <- dplyr::tbl(
    connection,
    dbplyr::in_catalog(
      "strategyunit",
      "default",
      "sl_af_describing_mitigators_final_2324_sex"
    )
  ) |>
    dplyr::filter(!!rlang::parse_expr(condition)) |>
    dplyr::summarise(episodes = sum(episodes),
                     beddays = sum(beddays)) |>
    sparklyr::collect() |>
    tidyr::pivot_longer(
      cols = c(episodes, beddays),
      names_to = "metric",
      values_to = "number"
    )
  
  summary <- mitigator_totals |>
    dplyr::left_join(totals, "metric") |>
    dplyr::mutate(
      perc = janitor::round_half_up(number * 100 / total, 2),
      metric = metric |>
        stringr::str_replace("episodes", "Spells") |>
        stringr::str_to_sentence()
    ) |>
    dplyr::arrange(dplyr::across(1))
  
  return(summary)
}

#' Get the percentage of mitigable spells for a mitigator by a group.
#' 
#' Given a `group` (for example, `sex` or `age`), this function will get the 
#' table from Databricks, filter to the mitigator or set of mitigators using 
#' the `condition` provided and calculate the number and percentage of mitigable 
#' spells by group.
#'
#' @param group A string of the group that the table is for.
#' @param condition An string containing the expression needed to filter for a 
#' mitigator or set of mitigators. 
#' @param connection The Databricks connection.
#'
#' @return A dataframe of the number and percentage of mitigable spells by 
#' group.
get_perc_spells_by_group <- function(group, condition, connection = sc) {
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
    dplyr::rename(spells = episodes) |> 
    # Although the column is called episodes, each row is the last episode in a 
    # spell. So renaming as spells here to avoid confusion later.
    dplyr::summarise(spells = sum(spells), 
                     .by = {{col_name}}) |>
    sparklyr::collect() |>
    dplyr::mutate(perc = janitor::round_half_up(spells * 100 /
                                                  sum(spells), 2),
                  dplyr::across(1, ~stringr::str_to_title(.))
                  ) |>
    order_levels_of_factors() |>
    dplyr::arrange(dplyr::across(1))
  
  return(summary)
}

#' Plot the percentage of mitigable spells for a mitigator by a group.
#'
#' @param data The output of `get_perc_spells_by_group()`.
#' @param group The group that the data is split by.
#'
#' @return A plot.
get_perc_spells_by_group_plot <- function(data, col_name) {
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
                  y = "Percentage of mitigable spells", 
                  fill = col_name_title)
  
  return(plot)
}



get_rates_per_pop <- function(group, condition, population, connection = sc) {
  
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
    dplyr::rename(spells = episodes) |> 
    # Although the column is called episodes, each row is the last episode in a 
    # spell. So renaming as spells here to avoid confusion later.
    dplyr::summarise(spells = sum(spells), 
                     .by = {{col_name}}) |>
    sparklyr::collect() |>
    dplyr::left_join(population, {{col_name}}) |>
    PHEindicatormethods::phe_rate(
      x = spells,
      n = pop,
      confidence = 0.95,
      multiplier = 100000
    ) |>
    order_levels_of_factors() |>
    dplyr::arrange(dplyr::across(1)) |>
    dplyr::mutate(value = janitor::round_half_up(value, 2),
                  dplyr::across(1, ~stringr::str_to_title(.))) |>
    dplyr::select({{col_name}}, spells, pop, value, lowercl, uppercl)
  
  return(summary)
}

get_rates_per_pop_plot <- function(data, col_name) {
  col_name_title <- col_name |>
    format_as_title()
  
  plot <- data |>
    ggplot2::ggplot(ggplot2::aes(!!rlang::sym(col_name),
                                 value, 
                                 fill = !!rlang::sym(col_name))) +
    ggplot2::geom_col() +
    StrategyUnitTheme::su_theme() +
    StrategyUnitTheme::scale_fill_su() +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = lowercl, ymax = uppercl), 
                           position = "dodge", 
                           width = 0.25)  +
    ggplot2::labs(x = col_name_title, 
                  y = "Rate per 100,000 population", 
                  fill = col_name_title)
  
  return(plot)
}

get_rates_per_pop_table <- function(data){
  table <- data |>
    dplyr::rename_with(~format_as_title(.)) |>
    get_table() |>
    flextable::delete_part(part = "footer")
  
  return(table)
}
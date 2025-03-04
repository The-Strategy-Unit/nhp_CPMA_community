# Functions used to complete the descriptive analyses.

# Percentage breakdowns --------------------------------------------------------
#' Get an overview of mitigable activity for a mitigator.
#'
#' That is, get the number of mitigable admissions and beddays for a mitigator
#' in 2023-24, get the total number of all emergency or elective admissions and
#' calculate the percentage.
#'
#' @param treatment_type Either `"emergency"` or `"elective"`.
#' @param condition A string containing the expression needed to filter for a
#' mitigator or set of mitigators.
#' @param totals A dataframe containing the total beddays and episodes for 2023-24.
#' @param connection The Databricks connection.
#'
#' @return A dataframe.
get_overview_of_mitigator <- function(treatment_type,
                                      condition,
                                      totals,
                                      connection = sc) {
  totals <- totals |>
    dplyr::filter(type == treatment_type) |>
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
        stringr::str_replace("episodes", "admissions") |>
        stringr::str_to_sentence()
    ) |>
    dplyr::arrange(dplyr::across(1))
  
  return(summary)
}

#' Get the number of mitigable admissions for a mitigator by a group.
#'
#' Given a `group` (for example, `sex` or `age`), this function will get the
#' table from Databricks, filter to the mitigator or set of mitigators using
#' the `condition` provided and calculate the number and percentage of mitigable
#' admissions by group.
#'
#' @param group A string of the group that the table is for.
#' @param condition A string containing the expression needed to filter for a
#' mitigator or set of mitigators.
#' @param activity_type Either `"admissions"` or `"beddays"`.
#' @param connection The Databricks connection.
#'
#' @return A dataframe of the number of mitigable admissions by group.
get_number_by_group <- function(group,
                                condition,
                                activity_type,
                                connection = sc) {
  col_name <- get_col_name_from_group(group)
  
  summary <- dplyr::tbl(connection,
                        dbplyr::in_catalog(
                          "strategyunit",
                          "default",
                          paste0("sl_af_describing_mitigators_final_2324_", group)
                        )) |>
    dplyr::filter(!!rlang::parse_expr(condition), 
                  !is.na(!!rlang::sym(col_name)) # exclude NULLs
                  ) |>
                  dplyr::rename(admissions = episodes) |>
                    # Although the column is called episodes, each row is the 
                    # last episode in a spell. So renaming as admissions here to
                    # avoid confusion later.
                    dplyr::summarise(number = sum(!!rlang::sym(activity_type)), 
                                     .by = {{col_name}}) |>
                    sparklyr::collect()
                  
                  return(summary)
                  }

#' Get the percentage of mitigable admissions for a mitigator by a group.
#'
#' Given a `group` (for example, `sex` or `age`), this function will use 
#' `get_number_by_group()` to get the table from Databricks, filter to the 
#' mitigator or set of mitigators using the `condition` provided and calculate 
#' the number and percentage of mitigable admissions by group.
#'
#' @param group A string of the group that the table is for.
#' @param condition A string containing the expression needed to filter for a
#' mitigator or set of mitigators.
#' @param activity_type Either `"admissions"` or `"beddays"`.
#' @param connection The Databricks connection.
#'
#' @return A dataframe of the number and percentage of mitigable admissions by 
#' group.
get_perc_by_group <- function(group,
                              condition,
                              activity_type,
                              connection = sc) {
  summary <- get_number_by_group(group, 
                                 condition, 
                                 activity_type, 
                                 connection = sc) |>
    dplyr::mutate(
      perc = janitor::round_half_up(number * 100 /
                                      sum(number), 2),
      dplyr::across(1, ~ stringr::str_to_title(.))
    ) |>
    order_levels_of_factors() |>
    dplyr::arrange(dplyr::across(1)) |>
    dplyr::rename(!!rlang::sym(activity_type) := number)
  
  return(summary)
}

#' Plot the percentage of mitigable admissions for a mitigator by a group.
#'
#' @param data The output of `get_perc_admissions_by_group()`.
#' @param col_name The col_name that the data is split by.
#' @param activity_type Either `"admissions"` or `"beddays"`.
#'
#' @return A plot.
get_perc_by_group_plot <- function(data, col_name, activity_type) {
  col_name_title <- col_name |>
    format_as_title()
  
  plot <- data |>
    order_levels_of_factors() |>
    ggplot2::ggplot(ggplot2::aes(!!rlang::sym(col_name), perc, fill = 'bars_color')) +
    ggplot2::geom_col() +
    ggplot2::scale_fill_manual(values = c('bars_color' = "#f9bf07"),
                               guide = 'none') +
    StrategyUnitTheme::su_theme() +
    ggplot2::labs(x = col_name_title,
                  y = glue::glue("Percentage of mitigable {activity_type}"))
  
  return(plot)
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
    dplyr::rename_with( ~ format_as_title(.)) |>
    dplyr::rename("Percentage" = "Perc") |>
    get_table() |>
    flextable::delete_part(part = "footer")
  
  return(table)
}

# Rates per 100,000 ------------------------------------------------------------
#' Get rates per population of admissions per group.
#'
#' @param group A string of the group that the table is for.
#' @param condition A string containing the expression needed to filter for a
#' mitigator or set of mitigators.
#' @param population A dataframe of the population by group.
#' @param activity_type Either `"admissions"` or `"beddays"`.
#' @param connection The Databricks connection.
#'
#' @return
get_rates_by_group <- function(group,
                               condition,
                               population,
                               activity_type,
                               connection = sc) {
  col_name <- get_col_name_from_group(group)
  
  summary <- get_number_by_group(group, 
                                 condition, 
                                 activity_type, 
                                 connection = sc) |>
    dplyr::left_join(population, {{col_name}}) |>
    PHEindicatormethods::phe_rate(
      x = number,
      n = pop,
      confidence = 0.95,
      multiplier = 100000
    ) |>
    order_levels_of_factors() |>
    dplyr::arrange(dplyr::across(1)) |>
    dplyr::mutate(value = janitor::round_half_up(value, 2),
                  dplyr::across(1, ~ stringr::str_to_title(.))) |>
    dplyr::select({{col_name}}, number, pop, value, lowercl, uppercl)
  
  return(summary)
}

#' Plot the rates per population of admissions per group.
#'
#' @param data The output of `get_rates_per_pop()`.
#' @param col_name The col_name that the data is split by.
#'
#' @return A plot.
get_rates_by_group_plot <- function(data, col_name) {
  col_name_title <- col_name |>
    format_as_title()
  
  plot <- data |>
    order_levels_of_factors() |>
    ggplot2::ggplot(ggplot2::aes(!!rlang::sym(col_name), value, fill = 'bars_color')) +
    ggplot2::geom_col() +
    ggplot2::scale_fill_manual(values = c('bars_color' = "#f9bf07"),
                               guide = 'none') +
    StrategyUnitTheme::su_theme() +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = lowercl, ymax = uppercl),
                           position = "dodge",
                           width = 0.25)  +
    ggplot2::labs(x = col_name_title, y = "Rate per 100,000 population")
  
  return(plot)
}

#' Formats rates per 100,000 data into a table.
#'
#' @param data A dataframe of rates per 100,000 data.
#'
#' @return A table.
#'
#' @examples
#' \dontrun{
#' targets::tar_read(rates_per_pop_age_frail) |>
#' get_rates_per_pop_table()}
get_rates_by_group_table <- function(data) {
  table <- data |>
    dplyr::rename_with( ~ format_as_title(.)) |>
    get_table() |>
    flextable::delete_part(part = "footer")
  
  return(table)
}

# Top ten specialties ----------------------------------------------------------
#' Get the top ten specialties for a mitigator.
#'
#' @param condition A string containing the expression needed to filter for a
#' mitigator or set of mitigators.
#' @param key The specialty key.
#' @param activity_type Either `"admissions"` or `"beddays"`.
#'
#' @return A dataframe.
get_top_ten_specialties <- function(condition, key, activity_type) {
  get_perc_by_group("mainspef", condition, activity_type) |>
    dplyr::left_join(key, by = c("mainspef" = "dd_code")) |>
    dplyr::arrange(desc(perc)) |>
    dplyr::slice(1:10) |>
    dplyr::select(specialty, {{activity_type}}, perc)
}

# Length of Stay ---------------------------------------------------------------

#' Get the percentage of mitigable admissions for a mitigator by a group.
#'
#' Using `get_perc_by_group` with `group = "los`, this function will get the
#' table from Databricks, filter to the mitigator or set of mitigators using
#' the `condition` provided and calculate the number and percentage of mitigable
#' admissions by length of stay (LOS).
#'
#' @param condition A string containing the expression needed to filter for a
#' mitigator or set of mitigators.
#' @param activity_type Either `"admissions"` or `"beddays"`.
#' @param connection The Databricks connection.
#'
#' @return A dataframe of the number and percentage of mitigable admissions by
#' LOS.
get_perc_by_los <- function(condition, activity_type, connection = sc) {
  data_weeks <- get_perc_by_group("los", 
                                  condition, 
                                  activity_type, 
                                  connection) |>
    dplyr::mutate(
      weeks = dplyr::case_when(
        los_range == "0" ~ "1",
        los_range == "1" ~ "1",
        los_range == "2" ~ "1",
        los_range == "3" ~ "1",
        los_range == "4-7" ~ "1",
        los_range == "8-14" ~ "2",
        los_range == "15-21" ~ "3",
        los_range == "22+" ~ "3+",
        .default = "error"
      )
    )
  
  summary <- data_weeks |>
    dplyr::summarise(number_weeks = sum(!!rlang::sym(activity_type)), 
                     .by = weeks) |>
    dplyr::mutate(perc_weeks = janitor::round_half_up(number_weeks * 100 /
                                                        sum(number_weeks), 
                                                      2)) |>
    dplyr::right_join(data_weeks, "weeks") |>
    dplyr::select(los_range,
                  !!rlang::sym(activity_type),
                  perc,
                  weeks,
                  number_weeks,
                  perc_weeks)
  
  return(summary)
}

#' Plot the percentage of mitigable activity for a mitigator by LOS.
#'
#' @param data The output of `get_perc_by_group()`.
#' @param activity_type Either `"admissions"` or `"beddays"`.
#'
#' @return A plot.
get_perc_by_los_plot <- function(data, activity_type) {
  plot <- data |>
    ggplot2::ggplot(ggplot2::aes(weeks, perc, fill = los_range)) +
    ggplot2::geom_col() +
    StrategyUnitTheme::scale_fill_su() +
    StrategyUnitTheme::su_theme() +
    ggplot2::labs(
      fill = "Length of stay (days)",
      x = "Length of stay (weeks)",
      y = glue::glue("Percentage of mitagable {activity_type}")
    )
  
  return(plot)
}

#' Formats the output of `get_perc_by_los` into a table.
#'
#' @param data The output of `get_perc_admissions_by_group()`.
#' @param activity_type Either `"admissions"` or `"beddays"`.
#'
#' @return A table.
get_perc_by_los_table <- function(data, activity_type) {
  table <- data |>
    dplyr::select(los_range, !!rlang::sym(activity_type), perc) |>
    get_table_perc()
  
  return(table)
}

# Functions used to complete the descriptive analyses.

# Percentage breakdowns --------------------------------------------------------
#' Get an overview of mitigable activity for a mitigator or mechanism.
#'
#' That is, get the number of mitigable admissions and beddays for a mitigator 
#' or mechanism in 2023-24, get the total number of all emergency or elective 
#' admissions and calculate the percentage.
#'
#' @param mitigator The mitigator or mechanism.
#' @param totals A dataframe containing the total beddays and episodes for 2023-24.
#' @param connection The Databricks connection.
#' @param treatment_lookup A dataframe of the mitigators and mechanisms and their treatment types.
#'
#' @return A dataframe.
get_overview_of_mitigator <- function(mitigator,
                                      totals,
                                      treatment_lookup,
                                      connection = sc) {
  
  treatment <- treatment_lookup |> 
    dplyr::filter(mitigator_or_mechanism == mitigator) |>
    dplyr::pull(treatment_type)
  
  if(treatment == "both") {
    treatment_title <- ""
  } else {
    treatment_title <- paste0(" ", treatment)
  }
  
  totals <- totals |>
    dplyr::filter(treatment_type == treatment) |>
    dplyr::summarise(total = sum(total), .by = activity_type) |>
    dplyr::select(activity_type, total)
  
  mitigator_totals <- dplyr::tbl(
    connection,
    dbplyr::in_catalog(
      "strategyunit",
      "default",
      get_table_name("sex")
    )
  ) |>
    filter_to_mitigator_or_mechanism(mitigator) |>
    dplyr::summarise(admissions = sum(episodes),
                     beddays =sum(beddays)) |>
    sparklyr::collect() 
  
  summary <- mitigator_totals |>
    tidyr::pivot_longer(
      cols = c(admissions, beddays),
      names_to = "activity_type",
      values_to = "number"
    ) |>
    dplyr::left_join(totals, "activity_type") |>
    dplyr::mutate(
      perc = janitor::round_half_up(number * 100 / total, 2),
      activity_type = activity_type |>
        stringr::str_to_sentence()
    ) |>
    dplyr::arrange(dplyr::across(1)) |>
    dplyr::rename(!!rlang::sym(glue::glue("Total{treatment_title} activity")) := total,
                  "Mitigable activity" = number)
  
  return(summary)
}

#' Get the number of mitigable admissions for a mitigator or mechanism by a 
#' group.
#'
#' Given a `group` (for example, `sex` or `age`), this function will get the
#' table from Databricks, filter to the mitigator or mechanism using and 
#' calculate the number and percentage of mitigable admissions by group.
#'
#' @param group A string of the group that the table is for.
#' @param activity_type Either `"admissions"` or `"beddays"`.
#' @param connection The Databricks connection.
#' @param mitigator The mitigator or mechanism.
#'
#' @return A dataframe of the number of mitigable admissions by group.
get_number_by_group <- function(mitigator,
                                group,
                                activity_type,
                                connection = sc) {
  col_name <- get_col_name_from_group(group)
  
  summary <- dplyr::tbl(connection,
                        dbplyr::in_catalog(
                          "strategyunit",
                          "default",
                          get_table_name(group)
                        )) |>
    filter_to_mitigator_or_mechanism(mitigator) |>
    dplyr::filter(!is.na(!!rlang::sym(col_name))) |> # exclude NULLs
    dplyr::rename(admissions = episodes) |>
    format_diagnoses_codes(group) |>
      # Although the column is called episodes, each row is the last episode in 
      # a spell. So renaming as admissions here to avoid confusion later.
    dplyr::summarise(number = sum(!!rlang::sym(activity_type)), 
                     .by = {{col_name}}) |>
    sparklyr::collect()
  
  if(group == "age"){
    summary <- summary |>
      dplyr::filter(!!rlang::sym(col_name) != "NA")
  }
  
    return(summary)
  }

#' Get the percentage of mitigable admissions for a mitigator or mechanism by a 
#' group.
#'
#' Given a `group` (for example, `sex` or `age`), this function will use 
#' `get_number_by_group()` to get the table from Databricks, filter to the 
#' mitigator or mechanism and calculate the number and percentage of mitigable 
#' admissions by group.
#'
#' @param group A string of the group that the table is for.
#' @param activity_type Either `"admissions"` or `"beddays"`.
#' @param connection The Databricks connection.
#' @param mitigator The mitigator or mechanism.
#'
#' @return A dataframe of the number and percentage of mitigable admissions by 
#' group.
get_perc_by_group <- function(mitigator,
                              group,
                              activity_type,
                              connection = sc) {
  summary <- get_number_by_group(mitigator,
                                 group,
                                 activity_type, 
                                 connection = sc) |>
    dplyr::mutate(
      perc = janitor::round_half_up(number * 100 /
                                      sum(number), 2),
      dplyr::across(1, ~ stringr::str_to_title(.))
    ) |>
    order_levels_of_factors() |>
    dplyr::arrange(dplyr::across(1)) |>
    dplyr::mutate(number = janitor::round_half_up(number,0))  |>   
    dplyr::rename(!!rlang::sym(activity_type) := number) 
  
  return(summary)
}

#' Plot the percentage of mitigable admissions for a mitigator or mechanism by a
#' group.
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
  
  if("beddays" %in% colnames(data)){
    data <- data |>
      small_number_suppression("beddays")
  }
  
  else if("admissions" %in% colnames(data)){
    data <- data |>
      small_number_suppression("admissions")
  }
  
  table <- data |>
    dplyr::rename_with( ~ format_as_title(.)) |> 
    get_table() |>
    flextable::delete_part(part = "footer")|>
    align(part="all", align="left")
  
  
  return(table)
}

# Rates per 100,000 ------------------------------------------------------------
#' Get rates per population of admissions per group for a mitigator or 
#' mechanism.
#'
#' @param group A string of the group that the table is for.
#' @param population A dataframe of the population by group.
#' @param activity_type Either `"admissions"` or `"beddays"`.
#' @param connection The Databricks connection.
#' @param mitigator The mitigator or mechanism.
#'
#' @return
get_rates_by_group <- function(mitigator,
                               group,
                               population,
                               activity_type,
                               connection = sc) {
  col_name <- get_col_name_from_group(group)
  
  summary <- get_number_by_group(mitigator, 
                                 group,
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
    rowwise()|>
    dplyr::mutate(number= janitor::round_half_up(number,0),
                  pop= janitor::round_half_up(pop,0),
                  value=ifelse(min(lowercl)<100, janitor::round_half_up(value,2), janitor::round_half_up(value,0) ),
                  lowercl=ifelse(min(lowercl)<100, janitor::round_half_up(lowercl,2), janitor::round_half_up(lowercl,0) ),
                  uppercl=ifelse(min(lowercl)<100, janitor::round_half_up(uppercl,2), janitor::round_half_up(uppercl,0) ))|>
    dplyr::mutate(dplyr::across(1, ~ stringr::str_to_title(.))) |>
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
    mutate(number=ifelse(number<=10, "<=10", as.character(number)),
           value=ifelse(number<=10, "-", as.character(value)),
           lowercl=ifelse(number<=10, "-", as.character(lowercl)),
           uppercl=ifelse(number<=10, "-", as.character(uppercl)))|>
    dplyr::rename_with( ~ format_as_title(.)) |>
    get_table() |>
    flextable::delete_part(part = "footer")|>
    align(part="all", align="left")
  
  
  return(table)
}

# Top ten specialties/diagnoses ------------------------------------------------
#' Get the top ten specialties/diagnoses for a mitigator or mechanism.
#'
#' @param activity_type Either `"admissions"` or `"beddays"`.
#' @param mitigator The mitigator or mechanism.
#' @param group Either `"tretspef"` for specialty or `"diagnosis"` for diagnosis.
#' @param key Key for specialty or diagnosis.
#'
#' @return A dataframe.
get_top_ten <- function(mitigator, activity_type, group, key) {
  
  if(group == "tretspef"){
    column_title <- "specialty"
    join <- group
  } else {
    column_title <- "description"
    join <- "primary_diagnosis"
  }
  
  key <- key |>
    dplyr::rename(dplyr::any_of(c("tretspef" = "dd_code",
                                  "primary_diagnosis" = "code")))
  
  top_ten <- get_perc_by_group(mitigator, group, activity_type) |>
    dplyr::left_join(key, join) |>
    dplyr::arrange(desc(perc)) |>
    dplyr::slice(1:10) |>
    dplyr::select({{column_title}}, {{activity_type}}, perc)
  
  return(top_ten)
}

#' Plot the top ten specialties/diagnoses for a mitigator or mechanism.
#'
#' @param data The output of `get_top_ten_specialties()`.
#' @param activity_type Either `"admissions"` or `"beddays"`.
#' @param group Either `"tretspef"` for specialty or `"diagnosis"` for diagnosis.
#'
#' @return A plot.
get_top_ten_plot <- function(data, activity_type, group) {
  
  if(group == "diagnosis"){
    group <- "description"
    y_title <- "Primary Diagnosis"
  } else {
    y_title <- "Specialty"
  }
  
  plot <- data |>
    ggplot2::ggplot(ggplot2::aes(perc, 
                                 reorder(!!rlang::sym(group), perc),
                                 fill = 'bars_color')) + 
    ggplot2::geom_col() +
    ggplot2::scale_fill_manual(values = c('bars_color' = "#f9bf07"), 
                               guide = 'none') +
    StrategyUnitTheme::su_theme() +
    ggplot2::labs(x = glue::glue("Percentage of mitigable {activity_type}"), 
                  y = y_title)
  
  return(plot)
}

# Length of Stay ---------------------------------------------------------------

#' Get the percentage of mitigable admissions for a mitigator or mechanism by a 
#' group.
#'
#' Using `get_perc_by_group` with `group = "los`, this function will get the
#' table from Databricks, filter to the mitigator  or mechanism and calculate 
#' the number and percentage of mitigable admissions by length of stay (LOS).
#'
#' @param connection The Databricks connection.
#' @param mitigator The mitigator or mechanism.
#'
#' @return A dataframe of the number and percentage of mitigable admissions by
#' LOS.
get_perc_by_los <- function(mitigator, connection = sc) {
  data_weeks <- get_perc_by_group(mitigator,
                                  "los",  
                                  "admissions",
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
    dplyr::summarise(number_weeks = sum(admissions), 
                     .by = weeks) |>
    dplyr::mutate(perc_weeks = janitor::round_half_up(number_weeks * 100 /
                                                        sum(number_weeks), 
                                                      2)) |>
    dplyr::right_join(data_weeks, "weeks") |>
    dplyr::select(los_range,
                  admissions,
                  perc,
                  weeks,
                  number_weeks,
                  perc_weeks)
  
  return(summary)
}

#' Plot the percentage of mitigable activity for a mitigator or mechanism by 
#' LOS.
#'
#' @param data The output of `get_perc_by_group()`.
#'
#' @return A plot.
get_perc_by_los_plot <- function(data) {
  plot <- data |>
    ggplot2::ggplot(ggplot2::aes(los_range, perc, fill = 'bars_color')) +
    ggplot2::geom_col() +
    ggplot2::scale_fill_manual(values = c('bars_color' = "#f9bf07"),
                               guide = 'none')  +
    StrategyUnitTheme::su_theme() +
    ggplot2::labs(
      x = "Length of stay range (days)",
      y = glue::glue("Percentage of mitigable admissions")
    ) +
    ggplot2::scale_fill_manual(values = c('bars_color' = "#f9bf07"),
                               guide = 'none')
  
  return(plot)
}

#' Formats the output of `get_perc_by_los()` into a table.
#'
#' @param data The output of `get_perc_by_los()`.
#'
#' @return A table.
get_perc_by_los_table <- function(data) {
  table <- data |>
    dplyr::select(los_range, admissions, perc) |>
    get_table_perc()
  
  return(table)
}

#' Get the percentage LOS over time.
#'
#' @param data The object `los_over_time`.
#' @param mitigator The mitigator or mechanism.
get_perc_by_los_trends <- function(data, mitigator) {
  los_over_time <- data |>
    filter_to_mitigator_or_mechanism(mitigator) |>
    dplyr::mutate(
      los_range2 = dplyr::case_when(
        los_range %in% c("2", "3", "4-7") ~ "2-7",
        los_range %in% c("15-21", "22+") ~ "15+",
        .default = los_range
      )
    ) |>
    dplyr::summarise(episodes = sum(episodes),
                     .by = c(year, los_range2)) |>
    dplyr::mutate(perc = janitor::round_half_up(episodes * 100 / sum(episodes), 2),
                  .by = year) |>
    order_levels_of_factors()
  
  return(los_over_time)
  
}

#' Plot the number or percentage LOS over time.
#'
#' @param data The output of `get_perc_los_over_time()`.
#'
#' @returns A plot.
get_perc_by_los_trends_plot <- function(data) {
  
  max_number <- data |>
    dplyr::summarise(max = max(perc)) |>
    dplyr::pull(max)
  
  plot <- data |>
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(
      x = year,
      y = perc,
      group = los_range2,
      col = los_range2
    ),
    size = 1) +
    ggplot2::geom_rect(
      ggplot2::aes(NULL, NULL, xmin = "2019/20", xmax = "2021/22"),
      ymin = 0,
      ymax = max_number * 1.1,
      fill = "#686f73",
      size = 0.5,
      alpha = 0.01
    ) +
    ggplot2::annotate(
      "text",
      x = "2020/21",
      y = max_number * 1.08,
      label = "COVID-19 pandemic",
      size = 2.7
    ) +
    StrategyUnitTheme::su_theme() +
    StrategyUnitTheme::scale_colour_su() +
    ggplot2::labs(x = "", 
                  y = "Percentage",
                  col = "LOS range") +
    ggplot2::scale_y_continuous(labels = scales::label_comma())
  
  return(plot)
}

#' Formats the output of `get_perc_by_los_trends()` into a table.
#'
#' @param data The output of `get_perc_by_los_trends()`.
#'
#' @return A table.
get_perc_by_los_trends_table <- function(data) {
  table <- data |>
    dplyr::arrange(year, los_range2) |>
    dplyr::select(year, los_range = los_range2, admissions = episodes, perc) |>
    get_table_perc() |>
    flextable::hline(i = 5, border = flextable::fp_border_default()) |>
    flextable::hline(i = 10, border = flextable::fp_border_default()) |>
    flextable::hline(i = 15, border = flextable::fp_border_default()) |>
    flextable::hline(i = 20, border = flextable::fp_border_default()) |>
    flextable::hline(i = 25, border = flextable::fp_border_default()) 
  
  return(table)
}

#' Get the total number of beddays and admissions by emergency, elective and 
#' both.
#'
#' @param connection The Databricks connection.
#'
#' @return A dataframe.
get_total_beddays_admissions <- function(connection) {
  data_each <- dplyr::tbl(
    connection,
    dbplyr::in_catalog(
      "strategyunit",
      "default",
      get_table_name("sex")
    )
  ) |>
    dplyr::select(dplyr::starts_with("total")) |>
    dplyr::distinct() |>
    dplyr::summarise(dplyr::across(dplyr::everything(), ~ sum(.))) |>
    sparklyr::collect() |>
    tidyr::pivot_longer(
      cols = dplyr::everything(),
      names_to = "activity_treatment",
      values_to = "total"
    ) |>
    tidyr::separate_wider_delim(activity_treatment,
                                "_",
                                names = c("a", "activity_type", "treatment_type")) |>
    dplyr::mutate(activity_type = stringr::str_replace(activity_type, "episodes", "admissions"))
  
  data_both <- data_each |>
    dplyr::summarise(total = sum(total), .by = activity_type) |>
    dplyr::mutate(treatment_type = "both",
                  a = "total") |>
    dplyr::select(a, activity_type, treatment_type, total)
  
  data <- data_each |>
    rbind(data_both)
  
  return(data)
}

# Functions used to complete the comparative analyses.

#' This function is to extract a value (number, percentage or standardised rate)
#' for England as a whole, so the value can be plotted in the outputs of
#' `get_summary_by_icb_bar()`. Depending on the `metric`, only certain other
#' parameters need to be supplied.
#'
#' @param metric Either `"total_count"`, `"perc"` or `"rate"`.
#' @param data_number Either `summary_MITIGATOR_elderly_high_icb_admissions` or ``summary_MITIGATOR_elderly_high_icb_beddays`.
#' @param activity Needed if `metric == "perc`. Either `"admissions"` or `"beddays"`.
#' @param treatment Needed if `metric == "perc`. Either `"emergency"` or `"elective"`.
#' @param data_rate Either `"england_age_sex_standardised_rates_episodes"` or `"england_age_sex_standardised_rates_beddays"`.
#' @param cohort A string for the mitigator cohort. 
#'
#' @return A number.
get_england_value <- function(metric,
                              data_number = NULL,
                              data_perc = total_beddays_admissions,
                              activity = NULL,
                              treatment = NULL,
                              data_rate = NULL,
                              cohort = NULL) {
  if (metric == "total_count") {
    england_value <- data_number |>
      dplyr::summarise(mean = mean(total_count)) |>
      dplyr::pull()
  }
  
  if (metric == "perc") {
    numerator <- data_rate |>
      dplyr::filter(year == "2023/24", cohorts == cohort)  |>
      dplyr::pull(total_count)
    
    denominator <- data_perc |>
      dplyr::filter(activity_type == activity, treatment_type == treatment) |>
      dplyr::pull(total)
    
    england_value <- numerator * 100 / denominator
  }
  
  if (metric == "value") {
    england_value <- data_rate |>
      dplyr::filter(year == "2023/24", cohorts == cohort) |>
      dplyr::pull(!!rlang::sym(metric))
  }
  
  return(england_value)
}

#' Formats the metric shorthand into a metric that can be used in a plot.
#' 
#' Similar to `format_metric_for_captions()` but a shorter name, usually for plots.
#'
#' @param metric 
#'
#' @return A string.
get_metric_title <- function(metric) {
  title <- if(metric == "total_count") {
    "Number"
  } else if (metric == "perc") {
    "Percentage"
  } else if (metric == "value") {
    "Standardised Rate"
  }
  
  return(title)
}

#' Gets the ICB tooltip for the map.
#' 
#' Used inside `get_summary_by_icb_map()` to format the tooltip text.
#'
#' @param data A dataframe.
#' @param metric Either `"total_count"`, `"perc"` or `"rate"`.
#'
#' @return A dataframe.
get_icb_tooltip <- function(data, metric) {
  metric_title <- get_metric_title(metric)
  
  wrangled <- data |>
    dplyr::rename(metric_colour = !!rlang::sym(metric)) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      metric_colour_label = ifelse(metric == "perc",
                                 glue::glue("{metric_colour}%"),
                                 prettyNum(metric_colour, big.mark = ",")),
      icb_name = stringr::str_remove(icb_2024_name,
                                     " Integrated Care Board"),
      ICB = glue::glue("{icb_name} \n {metric_title}: {metric_colour_label}")) |>
    dplyr::ungroup() 
  
  return(wrangled)
}

#' Creates a note for the plots in `get_summary_by_icb_bar()`.
#'
#' @param metric_title The output of `get_metric_title()`.
#' @param england_value The output of `get_england_value`.
#'
#' @return A string.
get_note_on_dashed_line <- function(metric_title, england_value) {
  
  symbol <- ifelse(metric_title == "Percentage", "%", "")
  
  note <- glue::glue("Dashed line is the {stringr::str_to_lower(metric_title)} for England: {janitor::round_half_up(england_value, 2)}{symbol}.")
  
  return(note)
}

#' Gets the number of mitigable admissions/beddays, the percentage over all emergency/elective and
#' the standardised rates per 100,000 population for a mitigator.
#'
#' @param data Either `"icb_age_sex_standardised_rates_episodes"` or `"icb_age_sex_standardised_rates_beddays"`.
#' @param cohort A string for the mitigator cohort.
#' @param total The `total_beddays_admissions_by_icb` object.
#' @param activity_type Either `"admissions"` or `"beddays"`.
#' @param treatment_type Either `"emergency"` or `"elective"`.
#'
#' @return A dataframe.
get_summary_by_icb <- function(data, cohort, total, activity_type, treatment_type) {
  
  if(activity_type == "admissions") {
    activity_type <- "episodes"
  }
  
  denominator <- glue::glue("total_{activity_type}_{treatment_type}")
  
  wrangled <- data |>
    dplyr::filter(cohorts == cohort, 
                  year == "2023/24") |>
    dplyr::left_join(total, "icb") |>
    dplyr::mutate(perc = janitor::round_half_up(total_count * 100 / !!rlang::sym(denominator), 2))
  
  
  return(wrangled)
}

#' Plots a bar chart of the number, percentage or standardised rates by ICB.
#'
#' @param data The output of `get_summary_by_icb()`.
#' @param metric Either `"total_count"`, `"perc"` or `"rate"`.
#' @param cohort A string for the mitigator cohort. 
#' @param england_value The output of `get_england_value`.
#'
#' @return A plot.
get_summary_by_icb_bar <- function(data, metric, cohort, england_value) {
  
  metric_title <- get_metric_title(metric)
  
  symbol <- ifelse(metric == "perc", "%", "")
  
  max_value <- data |>
    dplyr::summarise(max = max(!!rlang::sym(metric))) |>
    dplyr::pull()
  
  plot <- data |>
    dplyr::mutate(metric_colour = janitor::round_half_up(!!rlang::sym(metric), 2)) |>
    ggplot2::ggplot() +
    ggplot2::aes(x = metric_colour,
                 y = reorder(icb_2024_name, metric_colour),
                 fill = 'bars_color') +
    ggplot2::geom_col() +
    ggplot2::geom_vline(xintercept = england_value, linetype = "dashed") +
    ggplot2::geom_text(ggplot2::aes(label = paste0(metric_colour, symbol)), 
                       hjust = -0.05, 
                       size = 2.3) +
    ggplot2::scale_fill_manual(values = c('bars_color' = "#f9bf07"), 
                               guide = 'none') +
    ggplot2::scale_x_continuous(limits = c(0, (max_value * 1.2))) +
    ggplot2::labs(x = metric_title, 
                  y = "",
                  caption = get_note_on_dashed_line(metric_title, england_value)) +
    StrategyUnitTheme::su_theme() +
    ggplot2::theme(legend.position = "none")
  
  return(plot)
  
}

#' Plots a map of the number, percentage or standardised rates by ICB.
#'
#' @param data The output of `get_summary_by_icb()`.
#' @param boundaries The ICB shapefile.
#' @param metric Either `"total_count"`, `"perc"` or `"rate"`.
#'
#' @return A map.
get_summary_by_icb_map <- function(data, boundaries, metric) {
  
  metric_title <- get_metric_title(metric)
  
  plot <- boundaries |>
    dplyr::left_join(data, by = c("icb23cd" = "icb_2024_code")) |>
    get_icb_tooltip(metric) |>
    ggplot2::ggplot(ggplot2::aes(fill = metric_colour, 
                                 tooltip = ICB)) +
    ggplot2::geom_sf() +
    ggplot2::theme_void() +
    ggplot2::labs(metric = metric_title)
    
  plot <- plotly::ggplotly(plot, tooltip = "tooltip") |>
      plotly::style(hoveron = "metrics")
  
  return(plot)
}

get_summary_by_icb_table <- function(data, activity_type, treatment_type) {
  
  table <- data |> 
    dplyr::select(icb = icb_2024_name,
                  !!rlang::sym(glue::glue("mitigable {activity_type}")) := total_count,
                  glue::glue("total_{activity_type}_{treatment_type}"),
                  perc,
                  total_pop,
                  standardised_rate = value) |>
    dplyr::arrange(icb) |>
    dplyr::mutate(dplyr::across(is.numeric, ~prettyNum(., big.mark = ","))) |>
    dplyr::rename_with( ~ format_as_title(.)) |>
    create_dt()
  
  return(table)
}


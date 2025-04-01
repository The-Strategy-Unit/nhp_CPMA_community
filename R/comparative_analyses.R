# Functions used to complete the comparative analyses.

#' This function is to extract a value (number, percentage or standardised rate)
#' for England as a whole, so the value can be plotted in the outputs of
#' `get_summary_by_icb_bar()`. Depending on the `metric`, only certain other
#' parameters need to be supplied.
#'
#' @param metric Either `"total_count"`, `"perc"` or `"rate"`.
#' @param data A dataframe used to calculate the number, perc or rate.
#' @param activity Needed if `metric == "perc`. Either `"admissions"` or `"beddays"`.
#' @param treatment Needed if `metric == "perc`. Either `"emergency"` or `"elective"`.
#' @param cohort A string for the mitigator cohort.
#'
#' @return A number.
get_england_value <- function(metric,
                              data,
                              activity = NULL,
                              treatment = NULL,
                              cohort = NULL) {
  if (metric == "total_count") {
    england_value <- data |>
      dplyr::summarise(mean = mean(total_count)) |>
      dplyr::pull()
  }
  
  if (metric == "perc") {
    england_value <- data |>
      dplyr::filter(stringr::str_to_lower(activity_type) == activity) |>
      dplyr::pull(perc)
  }
  
  if (metric == "value") {
    england_value <- data |>
      dplyr::filter(year == "2023/24", cohorts == cohort) |>
      dplyr::pull(!!rlang::sym(metric))
  }
  
  if(metric == "perc_total_mitigation"){
    if (activity == "admissions") {
      activity <- "episodes"
    }
    
    denominator <- glue::glue("total_{activity}")
    
    england_value <- data |>
      summarise(number = sum(!!rlang::sym(activity)), 
                               total_number = sum(!!rlang::sym(denominator))) |>
      mutate(perc = janitor::round_half_up(number * 100 / total_number, 2)) |>
      dplyr::pull(perc)
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
  title <- if (metric == "total_count") {
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
      metric_colour_label = ifelse(
        metric == "perc",
        glue::glue("{metric_colour}%"),
        prettyNum(metric_colour, big.mark = ",")
      ),
      icb_name = stringr::str_remove(icb_2024_name, " Integrated Care Board"),
      ICB = glue::glue("{icb_name} \n {metric_title}: {metric_colour_label}")
    ) |>
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
  metric <- ifelse(metric_title == "Number",
                   "average number",
                   stringr::str_to_lower(metric_title))
  
  symbol <- ifelse(metric_title == "Percentage", "%", "")
  
  note <- glue::glue(
    "Dashed line is the {metric} for England 2023/24: {janitor::round_half_up(england_value, 2)}{symbol}."
  )
  
  return(note)
}

#' Gets the number of mitigable admissions/beddays, the percentage over all 
#' emergency/elective and the standardised rates per 100,000 population for a 
#' mitigator/mechanism by ICB/LA.
#'
#' @param data Either `"icb_age_sex_standardised_rates_episodes"` or `"icb_age_sex_standardised_rates_beddays"` or LA equivalents.
#' @param total The `total_beddays_admissions_by_icb` object.
#' @param activity_type Either `"admissions"` or `"beddays"`.
#' @param geography Either `"icb"` or `"ladcode23"`.
#' @param mitigator The mitigator or mechanism.
#' @param treatment_lookup A dataframe of the mitigators and mechanisms and their treatment types.
#'
#' @return A dataframe.
get_summary_by_geography <- function(data,
                                     mitigator,
                                     total,
                                     activity_type,
                                     geography,
                                     treatment_lookup = NULL) {
  if (activity_type == "admissions") {
    activity_type <- "episodes"
  }
  
  treatment_type <- if(mitigator == "all") {
    "both"
  } else {
    treatment_lookup |> 
      dplyr::filter(mitigator_or_mechanism == mitigator) |>
      dplyr::pull(treatment_type)
  }
  
  denominator <- glue::glue("total_{activity_type}_{treatment_type}")
  
  wrangled <- data |>
    dplyr::rename(dplyr::any_of(c(ladcode23 = "resladst_ons"))) |>
    dplyr::filter(cohorts == mitigator, year == "2023/24") |>
    dplyr::left_join(total, geography) |>
    dplyr::mutate(perc = janitor::round_half_up(total_count * 100 / 
                                                  !!rlang::sym(denominator), 
                                                2),
                  dplyr::across(dplyr::any_of(c("icb_2024_name")),
                                ~simplify_icb_name(.)))
  
  return(wrangled)
}

#' Format the summary by ICB / LA as a table.
#'
#' @param data The output of `get_summary_by_geography()`.
#' @param activity_type Either `"admissions"` or `"beddays"`.
#' @param treatment_type Either `"emergency"` or `"elective"`.
get_summary_by_geography_table <- function(data, activity_type, treatment_type) {
  table <- data |>
    dplyr::select(
      dplyr::any_of(c(
        "icb" = "icb_2024_name", "local_authority" = "laname23"
      )),
      !!rlang::sym(glue::glue("mitigable {activity_type}")) := total_count,
      glue::glue("total_{activity_type}_{treatment_type}"),
      perc,
      total_pop,
      standardised_rate = value
    ) |>
    dplyr::arrange(dplyr::pick(1)) |>
    dplyr::mutate(dplyr::across(is.numeric, 
                                ~ prettyNum(., big.mark = ","))) |>
    dplyr::rename_with(~ format_as_title(.)) |>
    create_dt()
  
  return(table)
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
    dplyr::mutate(metric_colour = janitor::round_half_up(!!rlang::sym(metric), 
                                                         2)) |>
    ggplot2::ggplot() +
    ggplot2::aes(
      x = metric_colour,
      y = reorder(icb_2024_name, metric_colour),
      fill = 'bars_color'
    ) +
    ggplot2::geom_col() +
    ggplot2::geom_vline(xintercept = england_value, linetype = "dashed") +
    ggplot2::geom_text(ggplot2::aes(label = paste0(metric_colour, symbol)),
                       hjust = -0.05,
                       size = 2.3) +
    ggplot2::scale_fill_manual(values = c('bars_color' = "#f9bf07"),
                               guide = 'none') +
    ggplot2::scale_x_continuous(limits = c(0, (max_value * 1.2))) +
    ggplot2::labs(
      x = metric_title,
      y = "",
      caption = get_note_on_dashed_line(metric_title, england_value)
    ) +
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
    dplyr::rename(icb_2024_name = icb23nm) |>
    dplyr::left_join(data, by = "icb_2024_name") |>
    get_icb_tooltip(metric) |>
    ggplot2::ggplot(ggplot2::aes(fill = metric_colour, tooltip = ICB)) +
    ggplot2::geom_sf() +
    ggplot2::theme_void() +
    ggplot2::labs(fill = metric_title)
  
  plot <- plotly::ggplotly(plot, tooltip = "tooltip") |>
    plotly::style(hoveron = "metrics")
  
  return(plot)
}

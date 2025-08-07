# LOS over time

# For descriptive_analyses.R ---------------------------------------------------
#' Get the percentage LOS over time.
#'
#' @param data The object `los_over_time`.
#' @param mitigator The mitigator or mechanism.
get_perc_los_over_time <- function(data, mitigator) {

  los_over_time <- data |>
    filter_to_mitigator_or_mechanism(mitigator) |>
    dplyr::mutate(los_range2 = dplyr::case_when(
      los_range %in% c("0", "1") ~ "0-1",
      los_range %in% c("2", "3", "4-7") ~ "2-7",
      los_range %in% c("15-21", "22+") ~ "15+",
      .default = los_range)) |>
    dplyr::summarise(episodes = sum(episodes),
                     .by = c(year, los_range2)) |>
    dplyr::mutate(
      perc = janitor::round_half_up(episodes * 100 / sum(episodes), 2),
      .by = year
    ) |>
    order_levels_of_factors() 
  
  return(los_over_time)
  
}

#' Plot the number or percentage LOS over time.
#'
#' @param data The output of `get_perc_los_over_time()`.
#' @param metric Either `"number"` or `"perc"`.
#'
#' @returns A plot.
get_perc_los_over_time_plot <- function(data, metric) {
  if(metric == "number") {
    column <- "episodes"
  } else {
    column <- metric
  }
  
  max_number <- data |>
    dplyr::summarise(max = max(!!rlang::sym(column))) |>
    dplyr::pull(max)
  
  plot <- data |>
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(
      x = year,
      y = !!rlang::sym(column),
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
                  y = format_as_title(metric)) +
    ggplot2::scale_y_continuous(labels = scales::label_comma())
  
  return(plot)
}

# In targets -------------------------------------------------------------------
sc <- sparklyr::spark_connect(
master     = Sys.getenv("DATABRICKS_HOST"),
cluster_id = Sys.getenv("DATABRICKS_CLUSTER_ID"),
token      = Sys.getenv("DATABRICKS_TOKEN"),
method     = "databricks_connect",
envname    = Sys.getenv("DATABRICKS_ENVNAME")
)

connection <- sc
los_over_time <- dplyr::tbl(
  connection,
  dbplyr::in_catalog(
    "strategyunit",
    "default",
    "SL_AF_describing_mitigators_los_over_time"
  )
) |>
  dplyr::filter(fyear >= "201819") |>
  sparklyr::collect() |>
  dplyr::mutate(year = paste0( # change to add_year_col()
    stringr::str_sub(fyear, 1, 4),
    "/",
    stringr::str_sub(fyear, 5, 6)
  )) |>
  dplyr::select(-fyear)


d <- get_perc_los_over_time(los_over_time, "redirection_substitution") 
d

# In child-dir -----------------------------------------------------------------
get_perc_los_over_time_plot(d, "number")
get_perc_los_over_time_plot(d, "perc")


# For plots_of_mitigators_side_by_side.R ---------------------------------------
get_los__over_time_plots <- function(cohorts = mitigators_and_mechanisms,
                          summary_table = mitigator_summary_table) {
  new_cohorts <- cohorts[check_include_admissions(cohorts, summary_table)
                         & !check_if_zero_los_mitigator(cohorts)]
  
  plots <- purrr::map(
    new_cohorts,
    ~ targets::tar_read_raw(glue::glue("perc_los_over_time_{.}")) |> # or whatever target called
      get_perc_los_over_time_plot() +
      ggplot2::labs(title = .) +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_blank(),
                     axis.text.y = ggplot2::element_blank(),
                     axis.ticks.y = ggplot2::element_blank())
  )
  
  final <- patchwork::wrap_plots(plots) +
    patchwork::plot_annotation(
      glue::glue("Percentage of admissions by LOS range over time"))
  
  return(final)
}


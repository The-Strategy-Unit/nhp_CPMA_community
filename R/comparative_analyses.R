get_perc_by_icb <- function(data, total, activity_type, treatment_type) {
  
  denominator <- glue::glue("total_{activity_type}_{treatment_type}")
  
  wrangled <- data |>
    dplyr::left_join(total, "icb") |>
    dplyr::mutate(perc = janitor::round_half_up(total_count * 100 / !!rlang::sym(denominator), 2))
  
  return(wrangled)
}

get_standardised_rate_by_icb <- function(data, standard_pop) {
  
  wrangled <- data |>
    dplyr::left_join(standard_pop, 
                     by = c("age_range", "sex")) |>
    dplyr::group_by(icb_2024_code, icb, icb_2024_name) |>
    PHEindicatormethods::calculate_dsr(x = activity, # observed number of events
                                       n = icb_population, # non-standard pops for each stratum
                                       stdpop = pop) |> # standard populations for England for each stratum
    dplyr::ungroup() |>
    dplyr::mutate(value = janitor::round_half_up(value, 2))
  
  return(wrangled)
  
}

get_summary_by_icb <- function(data, cohort, icb_pop, standard_pop, total, activity_type) {
  
  wrangled <- data |>
    dplyr::filter(cohorts == cohort, 
                  fyear == "202324") |>
    dplyr::left_join(icb_pop,
                     by = c("icb" = "icb24cdh", 
                            "year" = "fyear", 
                            "age_range", "sex"))  |>
    dplyr::filter(!is.na(icb), 
                  age_range != "NA") |>
    dplyr::rename(activity = {{activity_type}}) |>
    get_standardised_rate_by_icb(standard_pop) |>
    get_perc_by_icb(total, activity_type, "emergency")
  
  return(wrangled)
}

get_summary_by_icb_plot <- function(data, boundaries, fill) {
  
  fill_title <- if(fill == "total_count") {
    "Number"
  } else if (fill == "perc") {
    "Percentage"
  } else if (fill == "value") {
    "Standardised Rate"
  }
  
  plot <- boundaries |>
    dplyr::left_join(data, by = c("icb23cd" = "icb_2024_code")) |>
    ggplot2::ggplot() +
    ggplot2::geom_sf(ggplot2::aes(fill = !!rlang::sym(fill))) +
    ggplot2::theme_void() +
    ggplot2::labs(fill = fill_title)
  
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
  

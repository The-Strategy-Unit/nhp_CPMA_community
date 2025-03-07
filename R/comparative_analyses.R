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

get_summary_by_icb_plot <- function(data, boundaries, fill) {
  
  fill_title <- get_fill_title(fill)
  
  plot <- boundaries |>
    dplyr::left_join(data, by = c("icb23cd" = "icb_2024_code")) |>
    get_icb_tooltip(fill) |>
    ggplot2::ggplot(ggplot2::aes(fill = fill_colour, 
                                 tooltip = ICB)) +
    ggplot2::geom_sf() +
    ggplot2::theme_void() +
    ggplot2::labs(fill = fill_title)
    
  plot <- plotly::ggplotly(plot, tooltip = "tooltip") |>
      plotly::style(hoveron = "fills")
  
  return(plot)
}

get_fill_title <- function(fill) {
  title <- if(fill == "total_count") {
    "Number"
  } else if (fill == "perc") {
    "Percentage"
  } else if (fill == "value") {
    "Standardised Rate"
  }
  
  return(title)
}

get_icb_tooltip <- function(data, fill) {
  fill_title <- get_fill_title(fill)
  
  wrangled <- data |>
    dplyr::rename(fill_colour = !!rlang::sym(fill)) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      fill_colour_label = ifelse(fill == "perc",
                                 glue::glue("{fill_colour}%"),
                                 prettyNum(fill_colour, big.mark = ",")),
      icb_name = stringr::str_remove(icb_2024_name,
                                     " Integrated Care Board"),
      ICB = glue::glue("{icb_name} \n {fill_title}: {fill_colour_label}")) |>
    dplyr::ungroup() 
  
  return(wrangled)
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


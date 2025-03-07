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

get_summary_by_icb_map <- function(data, boundaries, fill) {
  
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

get_england_value <- function(fill,
                              data_number = NULL,
                              data_perc = NULL,
                              activity = NULL,
                              treatment = NULL,
                              data_rate = NULL,
                              cohort = NULL) {
  if (fill == "total_count") {
    england_value <- data_number |>
      dplyr::summarise(mean = mean(total_count)) |>
      dplyr::pull()
  }
  
  if (fill == "perc") {
    numerator <- data_rate |>
      dplyr::filter(year == "2023/24", cohorts == cohort)  |>
      dplyr::pull(total_count)
    
    denominator <- data_perc |>
      dplyr::filter(activity_type == activity, treatment_type == treatment) |>
      dplyr::pull(total)
    
    england_value <- numerator * 100 / denominator
  }
  
  if (fill == "value") {
    england_value <- data_rate |>
      dplyr::filter(year == "2023/24", cohorts == cohort) |>
      dplyr::pull(!!rlang::sym(fill))
  }
  
  return(england_value)
}


get_summary_by_icb_bar <- function(data, fill, cohort, england_value) {
  
  fill_title <- get_fill_title(fill)
  
  symbol <- ifelse(fill == "perc", "%", "")
  
  max_value <- data |>
    dplyr::summarise(max = max(!!rlang::sym(fill))) |>
    dplyr::pull()
  
  plot <- data |>
    dplyr::mutate(fill_colour = janitor::round_half_up(!!rlang::sym(fill), 2)) |>
    ggplot2::ggplot() +
    ggplot2::aes(x = fill_colour,
                 y = reorder(icb_2024_name, fill_colour),
                 fill = 'bars_color') +
    ggplot2::geom_col() +
    ggplot2::geom_vline(xintercept = england_value, linetype = "dashed") +
    ggplot2::geom_text(ggplot2::aes(label = paste0(fill_colour, symbol)), 
                       hjust = -0.05, 
                       size = 2.3) +
    ggplot2::scale_fill_manual(values = c('bars_color' = "#f9bf07"), 
                               guide = 'none') +
    ggplot2::scale_x_continuous(limits = c(0, (max_value * 1.2))) +
    ggplot2::labs(x = fill_title, 
                  y = "",
                  caption = get_note_on_dashed_line(fill_title, england_value)) +
    StrategyUnitTheme::su_theme() +
    ggplot2::theme(legend.position = "none")
  
  return(plot)
  
}

get_note_on_dashed_line <- function(fill_title, england_value) {
  
  symbol <- ifelse(fill_title == "Percentage", "%", "")
  
  caption <- glue::glue("Dashed line is the {stringr::str_to_lower(fill_title)} for England: {janitor::round_half_up(england_value, 2)}{symbol}.")
  
  return(caption)
}

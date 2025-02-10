get_perc_episodes_by_group <- function(category, 
                                       cond,
                                       connection = sc) {
  
  group <- if(category == "age") {
    "age_range"
  } else {
    category
  }
  
  summary <- dplyr::tbl(
    connection,
    dbplyr::in_catalog("strategyunit", 
                       "default", 
                       paste0("sl_af_describing_mitigators_final_2223_", 
                              category))
  ) |>
    dplyr::filter(!!rlang::parse_expr(cond)) |>
    dplyr::summarise(episodes = sum(episodes), 
                     .by = {{group}}) |>
    sparklyr::collect() |>
    dplyr::mutate(perc = janitor::round_half_up(episodes * 100 / 
                                                  sum(episodes), 
                                                2)) |>
    dplyr::arrange(dplyr::across(1))
  
  return(summary)
}

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
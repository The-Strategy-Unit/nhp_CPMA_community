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

## LOS percentage breakdowns ---------------------------------------------------
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

## LOS Trends ------------------------------------------------------------------
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
    rename_los_for_eol_care(mitigator) |>
    order_levels_of_factors()
  
  return(los_over_time)
  
}

#' Plot the number or percentage LOS over time.
#'
#' @param data The output of `get_perc_los_over_time()`.
#'
#' @returns A plot.
get_perc_by_los_trends_plot <- function(data) {
  
  los_range_column <- if("los_range3" %in% names(data)) {
    "los_range3"
  } else {
    "los_range2"
  }
  
  max_number <- data |>
    dplyr::summarise(max = max(perc)) |>
    dplyr::pull(max)
  
  plot <- data |>
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(
      x = year,
      y = perc,
      group = !!rlang::sym(los_range_column),
      col = !!rlang::sym(los_range_column)
    ),
    size = 1) 
  
  plot <- plot |>
    add_covid_box_to_plot(max_number) +
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
  
  los_range_column <- if("los_range3" %in% names(data)) {
    "los_range3"
  } else {
    "los_range2"
  }
  
  number_unique_los <- data |>
    dplyr::select(!!rlang::sym(los_range_column)) |>
    unique() |>
    nrow()
  
  number_years <- data |>
    dplyr::select(year) |>
    unique() |>
    nrow()
  
  table <- data |>
    dplyr::arrange(year, !!rlang::sym(los_range_column)) |>
    dplyr::select(year, los_range = !!rlang::sym(los_range_column), admissions = episodes, perc) |>
    get_table_perc()
  
  for(j in 1:number_years) {
    table <- table |>
      flextable::hline(i = (j * number_unique_los), border = flextable::fp_border_default())
  }
  
  return(table)
}

#' For eol_care mitigators, add los_range3 column.
#'
#' @param data A dataframe with a column for los_range2.
#' @param cohort The mitigator / mechanism.
#'
#' @returns A dataframe.
rename_los_for_eol_care <- function(data, cohort) {
  if (cohort == "eol_care_2_days") {
    data <- data |>
      dplyr::mutate(los_range3 = stringr::str_replace(los_range2, "2-7", "2"))
  } else if (cohort == "eol_care_3_to_14_days") {
    data <- data |>
      dplyr::mutate(los_range3 = stringr::str_replace(los_range2, "2-7", "3-7"))
  }
  
  return(data)
}

## Average LOS -----------------------------------------------------------------
#' A plot of the average LOS over time.
#'
#' @param data A dataframe of the average LOS over time.
#'
#' @returns A plot.
get_avg_los_england_plot <- function(data){
  max_number <- data |>
    dplyr::summarise(max = max(avg_los)) |>
    dplyr::pull(max)
  
  plot <- data |>
    ggplot2::ggplot() +
    ggplot2::geom_line(ggplot2::aes(
      x = year,
      y = avg_los,
      group = mitigator
    ),
    size = 1) 
  
  plot <- plot |>
    add_covid_box_to_plot(max_number) +
    StrategyUnitTheme::su_theme() +
    StrategyUnitTheme::scale_colour_su() +
    ggplot2::labs(x = "", 
                  y = "Average Length of Stay") +
    ggplot2::scale_y_continuous(labels = scales::label_comma())
  
  return(plot)
}

#' Plotly plot of the average LOS over time for each ICB.
#'
#' @param data A dataframe of average LOS over time for each ICB.
#'
#' @returns A plot.
get_avg_los_icb_plot <- function(data) {
  
  axis_title <- "Average Length of Stay"
  
  fig <- data |>
    arrange(year) |>
    group_by(icb_2024_name) |>
    highlight_key( ~ icb_2024_name) |>
    plot_ly(
      x = ~ year,
      y = ~ avg_los,
      type = 'scatter',
      mode = 'lines',
      text =  ~ icb_2024_name,
      line = list(color = "#686f73"),
      width = 660,
      height = 300,
      hovertemplate = paste("ICB: %{text}<br>", "Year: %{x}<br>", "Value: %{y}")
    ) |>
    highlight( ~ icb_2024_name,
               on = "plotly_click",
               off = "plotly_doubleclick",
               dynamic = FALSE) |>
    layout(
      shapes = list(
        list(
          type = "rect",
          fillcolor = "#686f73",
          line = list(color = "#686f73"),
          opacity = 0.1,
          x0 = "2019/20",
          x1 = "2021/22",
          xref = "x",
          y0 = 0,
          y1 = max(data$avg_los) * 1.1,
          yref = "y"
        )
      ),
      xaxis = list(
        title = "",
        showticklabels = TRUE,
        showline = TRUE,
        showgrid = F ,
        linewidth = 1.6
      ),
      yaxis = list(
        title = axis_title,
        rangemode = "tozero",
        showline = TRUE,
        showgrid = F ,
        linewidth = 1.6 ,
        zeroline = FALSE,
        tickformat = "digits",
        anchor = "free",
        shift = 100
      ),
      annotations =
        list(
          x = "2020/21",
          y = max(data$avg_los) * 1.08,
          text = "COVID-19 pandemic",
          showarrow = F,
          xref = 'x',
          yref = 'y'
        )
    )
  
  fig2 <- fig |>
    layout(
      annotations = list(
        x = 1,
        y = -0.16,
        text = "footnote",
        showarrow = F,
        xref = 'paper',
        yref = 'paper'
      )
    )
  
  return(fig2)
}

# Total beddays and admissions -------------------------------------------------
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

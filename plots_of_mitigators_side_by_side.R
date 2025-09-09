# To see all the plots from a section for all mitigators and mechanisms at 
# side by side. Intended for QA and to pull out insights. If we think some may
# be useful to share, should double check the methodology and neaten up.
 
# There is a lot of setup below. In the plots section near the bottom, arguments
# in functions can be changed to look at different plots.

# Libraries and source files ---------------------------------------------------
source("R/general.R")
source("R/descriptive_analyses.R")
source("R/comparative_analyses.R")
source("R/Functions for cohort overlap.R")
source("R/Functions for trend analysis.R")
source("R/manipulating_mitigators_and_mechanisms.R")

library("dplyr")
library("ggplot2")
library("StrategyUnitTheme")
library("tidyr")
library("scales")
library("ComplexUpset")
library("patchwork")
library("targets")

get_treatment_type <- function(mitigator, lookup) {
  treatment_type <- lookup |>
    dplyr::filter(mitigator_or_mechanism == mitigator) |>
    dplyr::pull(treatment_type)
  
  return(treatment_type)
}

# Mitigator details ------------------------------------------------------------
# This is a copy of what's at the top of the targets pipeline
source("R/manipulating_mitigators_and_mechanisms.R")

mitigator_summary_table <-
  readxl::read_excel("reference/summary_mitigators_table.xlsx") |>
  dplyr::mutate(mechanism = snakecase::to_snake_case(mechanism))

mitigators <- mitigator_summary_table |>
  dplyr::select(mitigator_or_mechanism = mitigator_code,
                treatment_type = type_of_admission)

mechanisms <- mitigator_summary_table |>
  dplyr::summarise(treatment_type = paste(unique(type_of_admission), 
                                          collapse = ', '),
                   .by = mechanism) |>
  dplyr::mutate(treatment_type = ifelse(
    stringr::str_detect(treatment_type, "emergency & elective"),
    "both",
    treatment_type
  )) |>
  dplyr::rename(mitigator_or_mechanism = mechanism)

mitigators_and_mechanisms_treatment_lookup <- mitigators |>
  rbind(mechanisms) |>
  dplyr::mutate(treatment_type = stringr::str_replace(treatment_type, 
                                                      "emergency & elective",
                                                      "both"))

mitigators_and_mechanisms <- mitigators_and_mechanisms_treatment_lookup |>
  dplyr::pull(mitigator_or_mechanism)

# Patient characteristic -------------------------------------------------------
get_patient_characteristic_plot <- function(cohort,
                                            activity_type,
                                            patient_characteristic,
                                            metric) {
  patient_characteristic_col_name <- get_col_name_from_group(patient_characteristic)
  
  if (metric == "rate") {
    data <- targets::tar_read_raw(glue::glue(
      "rates_{patient_characteristic}_{cohort}_{activity_type}"
    ))
    
    plot <- data |>
      get_rates_by_group_plot(patient_characteristic_col_name)
  } else if (metric == "perc") {
    data <- targets::tar_read_raw(glue::glue(
      "perc_{patient_characteristic}_{cohort}_{activity_type}"
    ))
    
    plot <- data |>
      get_perc_by_group_plot(patient_characteristic_col_name, activity_type)
  }
  
  plot <- plot +
    ggplot2::labs(title = cohort) +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank())
  
  return(plot)
}

get_patient_characteristic_plots <- function(
    activity_type,
    patient_characteristic,
    metric,
    cohorts = mitigators_and_mechanisms,
    summary_table = mitigator_summary_table) {
  metric_title <- if (metric == "perc") {
    "Percentage"
  } else if (metric == "rate") {
    "Rates per 100,000 population"
  }
  
  new_cohorts <- if (activity_type == "beddays") {
    cohorts[check_include_beddays(cohorts)]
  } else if (activity_type == "admissions") {
    cohorts[check_include_admissions(cohorts, summary_table)]
  }
  
  plots <- purrr::map(
    new_cohorts,
    ~ get_patient_characteristic_plot(
      .,
      activity_type = activity_type,
      patient_characteristic = patient_characteristic,
      metric = metric
    )
  )
  
  final <- patchwork::wrap_plots(plots) +
    patchwork::plot_annotation(glue::glue(
      "{metric_title} of {activity_type} by {patient_characteristic}"
    ))
  
  return(final)
}

# LOS --------------------------------------------------------------------------
get_los_plots <- function(cohorts = mitigators_and_mechanisms,
                          summary_table = mitigator_summary_table) {
  new_cohorts <- cohorts[check_include_admissions(cohorts, summary_table)
                         & !check_if_zero_los_mitigator(cohorts)]
  
  plots <- purrr::map(
    new_cohorts,
    ~ targets::tar_read_raw(glue::glue("perc_los_{.}")) |>
      get_perc_by_los_plot() +
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
      glue::glue("Percentage of admissions by LOS range"))
  
  return(final)
}

get_los_trends_plots <- function(cohorts = mitigators_and_mechanisms,
                                 summary_table = mitigator_summary_table) {
  new_cohorts <- cohorts[check_include_admissions(cohorts, summary_table)
                         & !check_if_zero_los_mitigator(cohorts)]
  
  plots <- purrr::map(
    new_cohorts,
    ~ targets::tar_read_raw(glue::glue("perc_los_trends_{.}")) |> # or whatever target called
      get_perc_by_los_trends_plot("perc") +
      ggplot2::labs(title = .) +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_blank(),
                     axis.text.y = ggplot2::element_blank(),
                     axis.ticks.y = ggplot2::element_blank(),
                     legend.position = "none")
  )
  
  final <- patchwork::wrap_plots(plots) +
    patchwork::plot_annotation(
      glue::glue("Percentage of admissions by LOS range over time"))
  
  return(final)
}

# Top ten specialty/diagnosis --------------------------------------------------
get_top_ten_plots <- function(activity_type,
                              group,
                              cohorts = mitigators_and_mechanisms,
                              summary_table = mitigator_summary_table) {
  new_cohorts <- if (activity_type == "beddays") {
    cohorts[check_include_beddays(cohorts)]
  } else if (activity_type == "admissions") {
    cohorts[check_include_admissions(cohorts, summary_table)]
  }
  
  plots <- purrr::map(
    new_cohorts,
    ~ targets::tar_read_raw(glue::glue("{group}_{.}_{activity_type}")) |>
      get_top_ten_plot(activity_type, group) +
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
      glue::glue("Percentage of admissions by Top Ten {stringr::str_to_title(group)}"))
  
  return(final)
}

# Number within other cohorts --------------------------------------------------

get_number_within_plot <- function(cohort,
                                   activity_type,
                                   mitigator_summary_table) {
  if (check_if_mechanism(cohort, mitigator_summary_table)) {
    mechanism <- if (cohort == "redirection_substitution") {
      "redirection"
    } else if (cohort == "efficiencies_relocation") {
      "relocation"
    } else {
      cohort
    }
    
    overlap_numbers <- targets::tar_read_raw(
      glue::glue("cohort_overlap_numbers_{mechanism}"))
    
    plot <- plotting_barchart_number_of_cohorts_for_mechanism_group(
      overlap_numbers, 
      episodes) +
      ggplot2::labs(title = cohort) +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_blank(),
                     axis.text.y = ggplot2::element_blank(),
                     axis.ticks.y = ggplot2::element_blank())
  } else {
    overlap_numbers <- targets::tar_read(total_cohort_numbers_2324) |>
      filter(!!rlang::sym(cohort) == 1)
    
    plot <- plotting_barchart_number_of_cohorts(overlap_numbers, episodes) +
      ggplot2::labs(title = cohort) +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_blank(),
                     axis.text.y = ggplot2::element_blank(),
                     axis.ticks.y = ggplot2::element_blank())
  }
  
  return(plot)
}

get_number_within_plots <- function(activity_type,
                                    cohorts = mitigators_and_mechanisms,
                                    summary_table = mitigator_summary_table) {
  new_cohorts <- if (activity_type == "beddays") {
    cohorts[check_include_beddays(cohorts)]
  } else if (activity_type == "admissions") {
    cohorts[check_include_admissions(cohorts, summary_table)]
  }
  
  activity_type <- rename_admissions_as_episodes(activity_type)
  
  plots <- purrr::map(
    new_cohorts,
    ~ get_number_within_plot(
      cohort = .,
      activity_type = activity_type,
      mitigator_summary_table = summary_table
    )
  )
  
  final <- patchwork::wrap_plots(plots) +
    patchwork::plot_annotation(
      glue::glue("Number within multiple other cohorts ({activity_type})"))
  
  return(final)
}

# Most common overlaps ---------------------------------------------------------
# Method above doesn't work since not grobs
#
# get_most_common_plot <- function(cohort,
#                                    activity_type,
#                                    mitigator_summary_table) {
#   if(check_if_mechanism(cohort, mitigator_summary_table)) {
#     mechanism <- if (cohort == "redirection_substitution") {
#       "redirection"
#     } else if (cohort == "efficiencies_relocation") {
#       "relocation"
#     } else {
#       cohort
#     }
#
#     overlap_numbers <- targets::tar_read_raw(glue::glue("cohort_overlap_numbers_{mechanism}"))
#
#     plot_upset_plot_mechanism_group(overlap_numbers,
#                                     mitigator_summary_table,
#                                     episodes) +
#       ggplot2::labs(title = cohort) +
#       ggplot2::theme(axis.title.x = ggplot2::element_blank(),
# axis.text.x = ggplot2::element_blank(),
# axis.ticks.x = ggplot2::element_blank(),
# axis.title.y = ggplot2::element_blank(),
# axis.text.y = ggplot2::element_blank(),
# axis.ticks.y = ggplot2::element_blank())
#   } else {
#     overlap_numbers <- targets::tar_read(total_cohort_numbers_2324) |>
#       filter(!!rlang::sym(cohort) == 1)
#
#     plot <- plot_upset_plot(overlap_numbers,
#                             mitigator_summary_table,
#                             episodes) +
#       ggplot2::labs(title = cohort) +
#       ggplot2::theme(axis.title.x = ggplot2::element_blank(),
# axis.text.x = ggplot2::element_blank(),
# axis.ticks.x = ggplot2::element_blank(),
# axis.title.y = ggplot2::element_blank(),
# axis.text.y = ggplot2::element_blank(),
# axis.ticks.y = ggplot2::element_blank())
#   }
#
#   return(plot)
# }
#
# get_most_common_plots <- function(activity_type,
#                                   cohorts = mitigators_and_mechanisms,
#                                   summary_table = mitigator_summary_table) {
#   new_cohorts <- if (activity_type == "beddays") {
#     cohorts[check_include_beddays(cohorts)]
#   } else if (activity_type == "admissions") {
#     cohorts[check_include_admissions(cohorts, summary_table)]
#   }
#
#   activity_type <- rename_admissions_as_episodes(activity_type)
#
#   plots <- purrr::map(
#     new_cohorts,
#     ~ get_most_common_plot(cohort = .,
#                            activity_type = activity_type,
#                            mitigator_summary_table = summary_table)
#   )
#
#   final <- patchwork::wrap_plots(plots) +
#     patchwork::plot_annotation(glue::glue(
#       "Most common cohort combinations ({activity_type})"
#     ))
#
#   return(final)
# }

# Comparative ------------------------------------------------------------------
## Maps ------------------------------------------------------------------------
icb_23_shp <- sf::st_read(
  "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Integrated_Care_Boards_April_2023_EN_BGC/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson"
) |>
  janitor::clean_names() |>
  dplyr::mutate(icb23nm = simplify_icb_name(icb23nm))

get_icb_map <- function(cohort,
                        activity_type,
                        metric,
                        mitigator_summary_table,
                        shapefile = icb_23_shp) {
  map <- targets::tar_read_raw(
    glue::glue("summary_icb_{activity_type}_{cohort}")) |>
    get_summary_by_icb_map(icb_23_shp, metric) +
    ggplot2::labs(title = cohort) +
    ggplot2::theme(legend.position = "none")
  
  return(map)
  
}

get_icb_maps <- function(activity_type,
                         metric,
                         cohorts = mitigators_and_mechanisms,
                         summary_table = mitigator_summary_table) {
  new_cohorts <- if (activity_type == "beddays") {
    cohorts[check_include_beddays(cohorts)]
  } else if (activity_type == "admissions") {
    cohorts[check_include_admissions(cohorts, summary_table)]
  }
  
  metric_title <- get_metric_title(metric)
  
  plots <- purrr::map(
    new_cohorts,
    ~ get_icb_map(
      cohort = .,
      activity_type = activity_type,
      metric = metric,
      mitigator_summary_table = summary_table
    )
  )
  
  final <- patchwork::wrap_plots(plots) +
    patchwork::plot_annotation(
      glue::glue("{metric_title} of mitigable {activity_type}"))
  
  return(final)
}

## Bars ------------------------------------------------------------------------
get_icb_bar <- function(cohort,
                        activity_type,
                        mitigator_summary_table,
                        shapefile = icb_23_shp) {
  england_value <- get_england_value("total_count",
                                     data = targets::tar_read_raw(glue::glue(
    "summary_icb_{activity_type}_{cohort}"
  )))
  
  plot <- targets::tar_read_raw(
    glue::glue("summary_icb_{activity_type}_{cohort}")) |>
    get_summary_by_icb_bar("total_count", cohort, england_value) +
    ggplot2::labs(title = cohort) +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank())
  
  return(plot)
  
}

get_icb_bars <- function(activity_type,
                         cohorts = mitigators_and_mechanisms,
                         summary_table = mitigator_summary_table) {
  new_cohorts <- if (activity_type == "beddays") {
    cohorts[check_include_beddays(cohorts)]
  } else if (activity_type == "admissions") {
    cohorts[check_include_admissions(cohorts, summary_table)]
  }
  
  metric_title <- get_metric_title("total_count")
  
  plots <- purrr::map(
    new_cohorts,
    ~ get_icb_bar(
      cohort = .,
      activity_type = activity_type,
      mitigator_summary_table = summary_table
    )
  )
  
  final <- patchwork::wrap_plots(plots) +
    patchwork::plot_annotation(
      glue::glue("{metric_title} of mitigable {activity_type}"))
  
  return(final)
}

# England trends ---------------------------------------------------------------
numbers_over_time <- tar_read(numbers_over_time) |>
  filter(year != "2014/15")

denominator_over_time <- tar_read(denominator_over_time)

icb_population_data <- tar_read(icb_population_data)

## Number ----------------------------------------------------------------------
get_number_england_trend <- function(cohort,
                                     activity_type,
                                     mitigator_summary_table,
                                     denominator,
                                     numbers,
                                     treatment_lookup) {
  activity_type2 <- rename_admissions_as_episodes(activity_type)
  
  number_percentage_over_time <- data_number_percentage_over_time_icb(
    denominator,
    numbers,
    cohort,
    get_treatment_type(cohort, treatment_lookup))
  
  plot <- number_percentage_over_time |>
    plot_of_number_over_time(!!rlang::sym(activity_type2)) +
    ggplot2::labs(title = cohort) +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank())
  
  return(plot)
  
}

get_number_england_trends <- function(
    activity_type,
    cohorts = mitigators_and_mechanisms,
    summary_table = mitigator_summary_table,
    denominator = denominator_over_time,
    numbers = numbers_over_time,
    treatment_lookup = mitigators_and_mechanisms_treatment_lookup) {
  new_cohorts <- if (activity_type == "beddays") {
    cohorts[check_include_beddays(cohorts)]
  } else if (activity_type == "admissions") {
    cohorts[check_include_admissions(cohorts, summary_table)]
  }
  
  plots <- purrr::map(
    new_cohorts,
    ~ get_number_england_trend(
      cohort = .,
      activity_type = activity_type,
      mitigator_summary_table = summary_table,
      denominator = denominator,
      numbers = numbers,
      treatment_lookup = treatment_lookup
    )
  )
  
  final <- patchwork::wrap_plots(plots) +
    patchwork::plot_annotation(glue::glue("Number over time ({activity_type})"))
  
  return(final)
}

## Perc ------------------------------------------------------------------------
get_perc_england_trend <- function(cohort,
                                   activity_type,
                                   mitigator_summary_table,
                                   denominator,
                                   numbers,
                                   treatment_lookup) {
  activity_type2 <- rename_admissions_as_episodes(activity_type)
  
  number_percentage_over_time <- data_number_percentage_over_time_icb(
    denominator,
    numbers,
    cohort,
    get_treatment_type(cohort, treatment_lookup))
  
  plot <- number_percentage_over_time |>
    filter(!is.na(percentage_episodes)) |>
    plot_of_percentage_over_time(
      !!rlang::sym(activity_type2), 
      !!rlang::sym(glue::glue("total_{activity_type2}"))) +
    ggplot2::labs(title = cohort) +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank())
  
  return(plot)
  
}

get_perc_england_trends <- function(
    activity_type,
    cohorts = mitigators_and_mechanisms,
    summary_table = mitigator_summary_table,
    denominator = denominator_over_time,
    numbers = numbers_over_time,
    treatment_lookup = mitigators_and_mechanisms_treatment_lookup) {
  new_cohorts <- if (activity_type == "beddays") {
    cohorts[check_include_beddays(cohorts)]
  } else if (activity_type == "admissions") {
    cohorts[check_include_admissions(cohorts, summary_table)]
  }
  
  plots <- purrr::map(
    new_cohorts,
    ~ get_perc_england_trend(
      cohort = .,
      activity_type = activity_type,
      mitigator_summary_table = summary_table,
      denominator = denominator,
      numbers = numbers,
      treatment_lookup = treatment_lookup
    )
  )
  
  final <- patchwork::wrap_plots(plots) +
    patchwork::plot_annotation(glue::glue("Percentage over time ({activity_type})"))
  
  return(final)
}

## SR --------------------------------------------------------------------------
get_rates_england_trends <- function(
    activity_type,
    cohorts = mitigators_and_mechanisms,
    summary_table = mitigator_summary_table) {
  new_cohorts <- if (activity_type == "beddays") {
    cohorts[check_include_beddays(cohorts)]
  } else if (activity_type == "admissions") {
    cohorts[check_include_admissions(cohorts, summary_table)]
  }
  
  activity_type2 <- rename_admissions_as_episodes(activity_type)
  
  plots <- purrr::map(
    new_cohorts,
    ~ tar_read_raw(glue::glue("england_age_sex_standardised_rates_{activity_type2}")) |>
      filter(cohorts == .) |>
      plot_of_standardised_rates_over_time() +
      ggplot2::labs(title = .,
                    caption = "")  +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_blank(),
                     axis.text.y = ggplot2::element_blank(),
                     axis.ticks.y = ggplot2::element_blank())
  )
  
  final <- patchwork::wrap_plots(plots) +
    patchwork::plot_annotation(glue::glue("Standardised rates over time ({activity_type})"))
  
  return(final)
}

# ICB rates versus percentage change plots -------------------------------------
get_rates_versus_perc_change_plots <- function(
    activity_type,
    cohorts = mitigators_and_mechanisms,
    summary_table = mitigator_summary_table) {
  new_cohorts <- if (activity_type == "beddays") {
    cohorts[check_include_beddays(cohorts)]
  } else if (activity_type == "admissions") {
    cohorts[check_include_admissions(cohorts, summary_table)]
  }
  
  activity_type2 <- rename_admissions_as_episodes(activity_type)
  
  plots <- purrr::map(
    new_cohorts,
    ~ tar_read_raw(glue::glue("icb_age_sex_standardised_rates_{activity_type2}")) |>
      filter(cohorts == .) |>
      plotting_total_activity_vs_percentage_change_ggplot("icb_2024_name") +
      ggplot2::labs(title = .)  +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_blank(),
                     axis.text.y = ggplot2::element_blank(),
                     axis.ticks.y = ggplot2::element_blank())
  )
  
  final <- patchwork::wrap_plots(plots) +
    patchwork::plot_annotation(glue::glue("Change in standardised rate ({activity_type})"))
  
  return(final)
}

# Table of percentage change numbers-----------------------------------------------------

numbers_over_time<-tar_read(numbers_over_time)
denominator_over_time<-tar_read(denominator_over_time)


number_percentage_data<-numbers_over_time|>
  left_join(denominator_over_time, by=c("age_range", "sex", "year", "icb", "icb_2024_name"))|>
  mutate(total_episodes_all=total_episodes_emergency+total_episodes_elective)|>
  mutate(total_beddays_all=total_beddays_emergency+total_beddays_elective)|>
  left_join(mitigators, by=c("cohorts"="mitigator_or_mechanism"))|>
  mutate(total_episodes_all=ifelse(treatment_type!="emergency"|is.na(treatment_type),total_episodes_all, total_episodes_emergency ))|>
  mutate(total_beddays_all=ifelse(treatment_type!="emergency"|is.na(treatment_type), total_beddays_all, total_beddays_emergency ))|>
  summarise(episodes=sum(episodes, na.rm=TRUE),
            beddays=sum(beddays, na.rm=TRUE),
            total_episodes=sum(total_episodes_all, na.rm=TRUE),
            total_beddays=sum(total_beddays_all, na.rm=TRUE),
            .by=c(cohorts, year))|>
  filter(year!="2014/15")|>
  mutate(percentage_episodes=(episodes/total_episodes)*100,
         percentage_beddays=(beddays/total_beddays)*100)|>
  as.data.frame()|>
  filter(year=="2023/24"| year=="2018/19")|>
  select(-total_episodes, -total_beddays)|>
  pivot_longer(  cols = c(episodes, beddays, percentage_episodes, percentage_beddays),
                 names_to = "activity",
                 values_to = "value")|>
  pivot_wider(names_from=year, values_from=value)|>
  mutate(change=janitor::round_half_up(((`2023/24`-`2018/19`)/`2018/19`)*100,1))|>
  select(-`2023/24`, -`2018/19`)|>
  pivot_wider(names_from=activity, values_from=change)

england_age_sex_standardised_rates_episodes <- tar_read(england_age_sex_standardised_rates_episodes)|>
  mutate(activity_type="episode_SR")

england_age_sex_standardised_rates_beddays <- tar_read(england_age_sex_standardised_rates_beddays)|>
  mutate(activity_type="bedday_SR")

SR_data<-rbind(england_age_sex_standardised_rates_episodes, england_age_sex_standardised_rates_beddays) |>
  filter(year=="2023/24"| year=="2018/19")|>
  select(year, cohorts, value, activity_type)|>
  pivot_wider(names_from=year, values_from=value)|>
  mutate(change=janitor::round_half_up(((`2023/24`-`2018/19`)/`2018/19`)*100,1))  |>
  select(-`2023/24`, -`2018/19`)|>
  pivot_wider(names_from=activity_type, values_from=change)

table_percent_change_over_time_england<-number_percentage_data |>
  left_join(SR_data, by=c("cohorts"))

# Plots ------------------------------------------------------------------------
# Below are some examples. All arguments can be changed as stated in comments 
# next to them. LOS is admissions only, but otherwise all functions can be used 
# for admissions/beddays.
get_patient_characteristic_plots("beddays", 
                                 "ethnicity", # age/ethnicity/imd/sex
                                 "rate") # rate/perc

get_los_plots() # Only does admissions.

get_los_trends_plots()

get_top_ten_plots("admissions", 
                  "specialty") # specialty/diagnosis

get_number_within_plots("admissions")

# Note: maps are slow and small
get_icb_maps("beddays", 
             "value") # total_count/perc/value

get_icb_bars("admissions") # have not done perc or SR, because not sure if this 
  # is useful since lots of text and would need extra work for england values

get_number_england_trends("admissions")

get_perc_england_trends("beddays")

get_rates_england_trends("beddays")

get_rates_versus_perc_change_plots("admissions")

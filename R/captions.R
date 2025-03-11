# Functions used to generate captions for outputs.

#' Gets the denominator of the percentage for the caption.
#' 
#' If the metric is a percentage, creates the phrase for the denominator of the percentage.
#'
#' @param metric A string.
#' @param activity_type Either `"admissions"` or `"beddays"`.
#' @param treatment_type  Either `"emergency"` or `"elective"`.
#'
#' @return A string.
format_denominator_for_captions <- function(metric, activity_type, treatment_type) {
  denominator <- if(metric == "perc") {
    glue::glue(" by {treatment_type} {activity_type}")
  } else {
    ""
  }
  
  return(denominator)
}

#' Formats the group name to a phrase more suitable for captions.
#'
#' @param group The group the data is split by: `"age"`, `"ethnicity"`, `"imd"` or `"sex"`.
#'
#' @return A string.
format_group_name_for_caption <- function(group) {
  if (group == "ethnicity") {
    group_formatted <- "ethnic category"
  } else if (group == "imd") {
    group_formatted <- "Index of Multiple Deprivation (IMD) decile"
  } else if (group == "los") {
    group_formatted <- "Length of Stay (LOS) range"
  } else {
    group_formatted <- group
  }
  
  return(group_formatted)
  
}

#' Formats the metric shorthand into a metric that can be used in the caption.
#'
#' @param metric Either `"number"`, `"perc"`, `"rate"` or` "SR"`.
#'
#' @return A string.
format_metric_for_captions <- function(metric) {
  renamed <- if (metric == "perc") {
    "Percentage"
  } else if(metric == "SR") {
    "Age and sex standardised rates per 100,000 population"
  } else if (metric == "rate") {
    "Rates per 100,000 population"
  } else { 
    stringr::str_to_title(metric)
  }
  
  return(renamed)
}

#' Get a caption for the tables of number/percentage/rates of mitigable admissions/beddays by mitigator by ICB.
#'
#' @param cohort A string for the mitigator cohort.
#' @param metric Either `"number"`, `"perc"` or` "SR"`.
#' @param activity_type Either `"admissions"` or `"beddays"`.
#' @param treatment_type  Either `"emergency"` or `"elective"`.
#'
#' @return A string.
get_caption_by_icb <- function(cohort, metric, activity_type, treatment_type = NULL) {
  
  denominator <- format_denominator_for_captions(metric, activity_type, treatment_type)
  
  metric <- format_metric_for_captions(metric)
  
  caption <- glue::glue("{metric} of mitigable {activity_type}{denominator} for the {cohort} cohort for ICBs in 2023/24.")
  
  return(caption)
}

#' Get a caption for the tables of number, percentage and rates of mitigable admissions/beddays by mitigator by ICB.
#'
#' @param cohort A string for the mitigator cohort.
#' @param activity_type Either `"admissions"` or `"beddays"`.
#' @param treatment_type Either `"emergency"` or `"elective"`.
#'
#' @return A string.
get_caption_by_icb_table <- function(cohort, activity_type, treatment_type) {
  caption <- glue::glue("The number of mitigable {activity_type}, the percentage by {treatment_type} admissions and the age and sex standardised rates per 100,000 population for the {cohort} cohort in 2023-24.")
  
  return(caption)
  
}

#' Get a caption for the plots/tables of percentage/rates of mitigable admissions/beddays by group .
#'
#' @param metric Either `"perc"` or` "rate"`.
#' @param cohort A string for the mitigator cohort.
#' @param activity_type Either `"admissions"` or `"beddays"`.
#' @param group The group the data is split by: `"age"`, `"ethnicity"`, `"imd"` or `"sex"`.
#'
#' @return A string.
get_caption_by_group <- function(metric, cohort, activity_type, group, treatment_type = NULL) {
  
  denominator <- format_denominator_for_captions(metric, activity_type, treatment_type)
  
  group_formatted <- format_group_name_for_caption(group)
  
  metric <- format_metric_for_captions(metric)
  
  caption <- glue::glue(
    "{metric} of mitigable {activity_type}{denominator} for the {cohort} cohort in 2023/24 by {group_formatted}."
  )
  return(caption)
}

#' Get caption for the overview table by mitigator.
#'
#' @param cohort A string for the mitigator cohort.
#' @param treatment_type Either `"emergency"` or `"elective"`.
#'
#' @return A string.
get_caption_overview <- function(cohort, treatment_type) {
  caption <- glue::glue("Percentage of mitigable admissions and beddays for the {cohort} cohort by {treatment_type} activity in 2023/24.")
  
  return(caption)
}

#' Get caption for the top ten specialties by mitigator tables and plots.
#'
#' @param cohort A string for the mitigator cohort.
#' @param activity_type Either `"admissions"` or `"beddays"`.
#'
#' @return A string.
get_caption_top_ten_specialties <- function(cohort, activity_type) {
  caption <- glue::glue("Top ten specialties of mitigable {activity_type} for the {cohort} cohort in 2023-24.")
  
  return(caption)
}


# Functions used to generate captions for outputs.

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

#' Get a caption for the plots/tables of percentage/rates of mitigable admissions/beddays by group .
#'
#' @param metric Either `"Percentage"` or` "Rates per 100,000 population"`.
#' @param cohort A string for the mitigator cohort.
#' @param type Either `"admissions"` or `"beddays"`.
#' @param group The group the data is split by: `"age"`, `"ethnicity"`, `"imd"` or `"sex"`.
#'
#' @return A string.
get_caption_by_group <- function(metric, cohort, type, group) {
  group_formatted <- format_group_name_for_caption(group)
  
  caption <- glue::glue(
    "{metric} of mitigable {type} for the {cohort} cohort in 2023/24 by {group_formatted}."
  )
  return(caption)
}

#' Get caption for the overview table by mitigator.
#'
#' @param cohort A string for the mitigator cohort.
#' @param activity_type Either `"emergency"` or `"elective"`.
#'
#' @return A string.
get_caption_overview <- function(cohort, activity_type) {
  caption <- glue::glue("Percentage of mitigable admissions and beddays for the {cohort} cohort by {activity_type} activity in 2023/24.")
  
  return(caption)
}

#' Get caption for the top ten specialties by mitigator tables and plots.
#'
#' @param cohort A string for the mitigator cohort.
#' @param type Either `"admissions"` or `"beddays"`.
#'
#' @return A string.
get_caption_top_ten_specialties <- function(cohort, type) {
  caption <- glue::glue("Top ten specialties of mitigable {type} for the {cohort} cohort in 2023-24.")
  
  return(caption)
}
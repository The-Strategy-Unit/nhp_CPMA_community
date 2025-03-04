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
#' @param metric Either `"perc"` or` "rate"`.
#' @param cohort A string for the mitigator cohort.
#' @param activity_type Either `"admissions"` or `"beddays"`.
#' @param group The group the data is split by: `"age"`, `"ethnicity"`, `"imd"` or `"sex"`.
#'
#' @return A string.
get_caption_by_group <- function(metric, cohort, activity_type, group, treatment_type = NULL) {
  
  by <- format_by_for_captions(metric, activity_type, treatment_type)
  
  group_formatted <- format_group_name_for_caption(group)
  
  metric <- format_metric_for_captions(metric)
  
  caption <- glue::glue(
    "{metric} of mitigable {activity_type}{by} for the {cohort} cohort in 2023/24 by {group_formatted}."
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



# test targets and qmd - shapefile
# then commit above first  correctiin types



#' Title
#'
#' @param cohort A string for the mitigator cohort.
#' @param metric Either `"number"`, `"perc"` or` "SR"`.
#' @param activity_type Either `"admissions"` or `"beddays"`.
#' @param treatment_type 
#'
#' @return
get_caption_by_icb <- function(cohort, metric, activity_type, treatment_type = NULL) {
  
  by <- format_by_for_captions(metric, activity_type, treatment_type)
  
  metric <- format_metric_for_captions(metric)
  
  caption <- glue::glue("{metric} of mitigable {activity_type}{by} for the {cohort} cohort for ICBs in 2023/24.")
  
  return(caption)
}


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

format_by_for_captions <- function(metric, activity_type, treatment_type) {
  by <- if(metric == "perc") {
    glue::glue(" by {treatment_type} {activity_type}")
  } else {
    ""
  }
  
  return(by)
}
# Functions used to generate captions for outputs.

# General formatting -----------------------------------------------------------
#' Gets the denominator of the percentage for the caption.
#' 
#' If the metric is a percentage, creates the phrase for the denominator of the percentage.
#'
#' @param metric A string.
#' @param activity_type Either `"admissions"` or `"beddays"`.
#' @param treatment_type  For example `"emergency"` or `"elective"`.
#'
#' @return A string.
format_denominator_for_caption <- function(metric, activity_type, treatment_type) {
  denominator <- if(metric == "perc") {
    glue::glue(" by {treatment_type} {activity_type}")
  } else {
    ""
  }
  
  return(denominator)
}

#' Formats the geography so can be used in a caption.
#'
#' @param geography Either `"icb"`,`"la"` or other.
#'
#' @return A string.
format_geography_for_caption <- function(geography) {
  
  caption <- if(geography == "icb") {
    "Integrated Care Board"
  } else if (geography == "la") {
    "Local Authority"
  } else {
    geography
  }
  
  return(caption)
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
  } else if (group == "diagnosis"){
    group_formatted <- "primary diagnoses"
  } else {
    group_formatted <- group
  }
  
  return(group_formatted)
  
}

#' Formats the metric shorthand into a metric that can be used in the caption.
#'
#' @param metric Either `"number"`, `"perc"`, `"rate"`, `"prop"` or` "SR"`.
#' @param treatment_type For example `"emergency"` or `"elective"`.
#' @param activity_type Either `"admissions"` or `"beddays"`.
#'
#' @return A string.
format_metric_for_caption <- function(metric, 
                                      activity_type = NULL, 
                                      treatment_type = NULL) {
  renamed <- if (metric == "perc") {
    "Percentage"
  } else if(metric == "SR") {
    "Age and sex standardised rates per 100,000 population"
  } else if (metric == "rate") {
    "Crude rate per 100,000 population"
  } else if (metric == "prop"){
    glue::glue("Proportion of {format_treatment_for_caption(treatment_type)} {activity_type}")
  } else { 
    stringr::str_to_title(metric)
  }
  
  return(renamed)
}

format_treatment_for_caption <- function(treatment_type) {
  caption <- if(treatment_type == "both") {
    "all (elective and emergency)"
  } else {
    treatment_type
  }
  
  return(caption)
}

# Comparative ------------------------------------------------------------------
#' Get a caption for the tables of number/percentage/rates of mitigable admissions/beddays by mitigator by ICB.
#'
#' @param cohort A string for the mitigator cohort.
#' @param metric Either `"number"`, `"perc"` or` "SR"`.
#' @param activity_type Either `"admissions"` or `"beddays"`.
#' @param treatment_type  For example `"emergency"` or `"elective"`.
#'
#' @return A string.
get_caption_by_geography <- function(cohort, metric, activity_type, treatment_type = NULL) {
  
  if(metric == "perc"){
    treatment_type <- format_treatment_for_caption(treatment_type)
  }
  
  denominator <- format_denominator_for_caption(metric, activity_type, treatment_type)
  
  metric <- format_metric_for_caption(metric)
  
  if(cohort == "All Mitigation") {
    cohort <- ""
    all <- "all "
  } else {
    cohort <- glue::glue(" for {cohort}")
    all <- ""
  }
  
  caption <- glue::glue("{metric} of {all}mitigable {activity_type}{denominator}{cohort} for ICBs in 2023/24.")
  
  return(caption)
}

#' Get a caption for the tables of number, percentage and rates of mitigable admissions/beddays by mitigator by ICB.
#'
#' @param cohort A string for the mitigator cohort.
#' @param activity_type Either `"admissions"` or `"beddays"`.
#' @param treatment_type For example `"emergency"` or `"elective"`.
#' @param geography Either `"icb"` or `"la"`.
#'
#' @return A string.
get_caption_by_geography_table <- function(cohort, 
                                           activity_type, 
                                           treatment_type,
                                           geography,
                                           number_suppressed = 0) {
  
  treatment_type <- format_treatment_for_caption(treatment_type)
  
  geography <- format_geography_for_caption(geography)
  
  if(cohort == "All Mitigation") {
    cohort <- ""
    all <- "all "
  } else {
    cohort <- glue::glue(" for {cohort}")
    all <- ""
  }
  
  caption <- if(number_suppressed == 1) {
    glue::glue("The number of {all}mitigable {activity_type} and the age and sex standardised rates per 100,000 population{cohort} by {geography} in 2023-24. The column for the percentage by {treatment_type} {activity_type} has been withheld due to small number suppression.")
  } else {
  glue::glue("The number of {all}mitigable {activity_type}, the percentage by {treatment_type} {activity_type} and the age and sex standardised rates per 100,000 population{cohort} by {geography} in 2023-24.")
  } 
   
  return(caption)
  
}

# Descriptive ------------------------------------------------------------------
#' Get a caption for the plots/tables of percentage/rates of mitigable admissions/beddays by group .
#'
#' @param metric Either `"perc"` or` "rate"`.
#' @param cohort A string for the mitigator cohort.
#' @param activity_type Either `"admissions"` or `"beddays"`.
#' @param group The group the data is split by: `"age"`, `"ethnicity"`, `"imd"` or `"sex"`.
#'
#' @return A string.
get_caption_by_group <- function(metric, cohort, activity_type, group, treatment_type = NULL) {
  
  if(metric == "perc"){
    treatment_type <- format_treatment_for_caption(treatment_type)
  }
  
  denominator <- format_denominator_for_caption(metric, activity_type, treatment_type)
  
  group_formatted <- format_group_name_for_caption(group)
  
  metric <- format_metric_for_caption(metric)
  
  caption <- glue::glue(
    "{metric} of mitigable {activity_type}{denominator} for {cohort} in 2023/24 by {group_formatted}."
  )
  return(caption)
}

#' Get caption for the overview table by mitigator.
#'
#' @param cohort A string for the mitigator cohort.
#' @param treatment_type For example `"emergency"` or `"elective"`.
#'
#' @return A string.
get_caption_overview <- function(cohort, treatment_type) {
  
  treatment_type <- format_treatment_for_caption(treatment_type)
  
  caption <- glue::glue("Percentage of mitigable admissions and beddays for {cohort} by {treatment_type} activity in 2023/24.")
  
  return(caption)
}

#' Get caption for the top ten specialties by mitigator tables and plots.
#'
#' @param cohort A string for the mitigator cohort.
#' @param activity_type Either `"admissions"` or `"beddays"`.
#' @param group Either `"specialties"` or `"diagnoses"`.
#'
#' @return A string.
get_caption_top_ten <- function(cohort, activity_type, group) {
  group <- format_group_name_for_caption(group)
  
  caption <- glue::glue("Top ten {group} of mitigable {activity_type} for {cohort} in 2023-24.")
  
  return(caption)
}

# Overlap ----------------------------------------------------------------------
#' Get caption for the number of cohorts charts.
#'
#' @param cohort A string for the mitigator cohort.
#' @param activity_type Either `"admissions"` or `"beddays"`.
#'
#' @return A string.
get_caption_number_of_cohorts <- function(cohort, activity_type){
  caption <- glue::glue("The number and percentage of mitigator cohorts that the {cohort} {activity_type} are a part of (2023/24).")
  
  return(caption)
}

#' Get caption for the summary of overlaps charts.
#'
#' @param cohort A string for the mitigator cohort.
#' @param activity_type Either `"admissions"` or `"beddays"`.
#'
#' @return A string.
get_caption_summary_of_overlaps <- function(cohort, activity_type){
  caption <- glue::glue("The number and proportion of {cohort} {activity_type} that are included in each of the other cohorts (2023/24).")
  
  return(caption)
}

#' Get caption for the upset plots.
#'
#' @param cohort A string for the mitigator cohort.
#' @param activity_type Either `"admissions"` or `"beddays"`.
#'
#' @return A string.
get_caption_upset_plots <- function(cohort, activity_type){
  caption <- glue::glue("The 15 most common mitigator cohort overlaps for {cohort} {activity_type} (2023/24).")
  
  return(caption)
}

# Trends -----------------------------------------------------------------------

#' Get caption for the total activity vs percentage change plots.
#'
#' @param cohort A string for the mitigator cohort.
#' @param activity_type Either `"admissions"` or `"beddays"`.
#'
#' @return A string.
get_caption_activity_vs_perc_change <- function(cohort,
                                                cohort_group,
                                                activity_type) {
  caption <- glue::glue("The standardised rate of {activity_type} for the {cohort} {cohort_group} (in 2018/19) versus the percentage change in {activity_type} between 2018/19 and 2023/24 for ICBs.")
  
  return(caption)
}

#' Get caption for the local authority trends tables.
#'
#' @param cohort A string for the mitigator cohort.
#' @param activity_type Either `"admissions"` or `"beddays"`.
#'
#' @return A string.
get_caption_la_trends <- function(cohort,
                                  activity_type) {
  caption <- glue::glue("Table of the age and sex standardised rates of {cohort} {activity_type} per 100,000 population by Local Authority over the last 5 years, including the percentage change between 2018/19 and 2023/24.")
  
  return(caption)
}

#' Get caption for the trends plots.
#'
#' @param cohort A string for the mitigator cohort.
#' @param activity_type Either `"admissions"` or `"beddays"`.
#' @param metric Either `"number"`, `"perc"` or` "SR"`.
#' @param geography Either `"England"`, `"icb"` or `"la"`.
#'
#' @return A string.
get_caption_trends <- function(cohort, metric, cohort_group, activity_type, geography) {
  
  metric <- format_metric_for_caption(metric) |>
    stringr::str_to_lower()
  
  geography <- if(geography == "England") {
    "in England"
  } else {
    glue::glue("by {format_geography_for_caption(geography)}")
  }
  
  caption <- glue::glue("The {metric} of {activity_type} for the {cohort} {cohort_group} {geography} over the last 9 years.")
  
  return(caption)
}

#' Get caption for trends percentage change plots.
#'
#' @param cohort A string for the mitigator cohort.
#' @param metric Either `"number"`, `"prop"` or` "SR"`.
#' @param activity_type Either `"admissions"` or `"beddays"`.
#'
#' @return A string.
get_caption_trends_percentage_change <- function(cohort, 
                                                 metric,
                                                 cohort_group,
                                                 activity_type, 
                                                 treatment_type = NULL) {
  metric1 <- format_metric_for_caption(metric, activity_type, treatment_type) |>
    stringr::str_to_lower()
  
  if(metric=="prop"){
  caption <- glue::glue("The percentage change in the {metric1} for the {cohort} {cohort_group} by ICB between 2018/19 and 2023/24.")
  }else{
    caption <- glue::glue("The percentage change in the {metric1} of {activity_type} for the {cohort} {cohort_group} by ICB between 2018/19 and 2023/24.")
  }
    
    
  return(caption)
}


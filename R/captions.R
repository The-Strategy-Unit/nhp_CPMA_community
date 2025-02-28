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
  } else {
    group_formatted <- group
  }
  
  return(group_formatted)
  
}

#' Get a caption for the plots/tables of percentage/rates of mitigable admissions/beddays by group .
#'
#' @param metric Either `"Percentage"` or` "Rates per 100,000 population"`.
#' @param cohort The mitigator.
#' @param activity_type Either `"admissions"` or `"beddays"`.
#' @param group The group the data is split by: `"age"`, `"ethnicity"`, `"imd"` or `"sex"`.
#'
#' @return A string.
get_caption_by_group <- function(metric, cohort, activity_type, group) {
  group_formatted <- format_group_name_for_caption(group)
  
  caption <- glue::glue(
    "{metric} of mitigable {activity_type} for the {cohort} cohort in 2023-24 by {group_formatted}."
  )
  return(caption)
}

# Functions to manipulate mitigator and mechanisms.

#' Checks if the mitigator or mechanism is the efficiency mechanism or one of 
#' its mitigators.
#'
#' @param mitigator The mitigator or mechanism.
#' @param summary_table The `mitigator_summary_table`.
#'
#' @return Boolean.
check_if_efficiency_mitigator <- function(mitigator, summary_table){
  
  efficiency_mitigators <- summary_table |>
    dplyr::filter(mechanism == "efficiencies") |>
    dplyr::pull(mitigator_code) |>
    c("efficiencies")
  
  check <- mitigator %in% efficiency_mitigators
  
  return(check)
}

#' Check if a mitigator is a mechanism.
#'
#' @param mitigator The mitigator or mechanism.
#' @param summary_table The `mitigator_summary_table`.
#'
#' @return Boolean.
check_if_mechanism <- function(mitigator, summary_table) {
  mechanisms <- summary_table |>
    dplyr::pull(mechanism) |>
    unique()
  
  check <- mitigator %in% mechanisms
  
  return(check)
    
}

#' Checks if the mitigator or mechanism is a zero length of stay mitigator.
#'
#' @param mitigator The mitigator or mechanism.
#'
#' @return Boolean.
check_if_zero_los_mitigator <- function(mitigator){
  
  check <- startsWith(mitigator, "zero_los_no_procedure")
  
  return(check)
}

#' Check if admissions are applicable for the mitigator.
#' 
#' This uses `check_if_efficiency_mitigator()` but also allows for other 
#' criteria to be added.
#'
#' @param mitigator The mitigator or mechanism.
#' @param summary_table The `mitigator_summary_table`.
#'
#' @return Boolean.
check_include_admissions <- function(mitigator, summary_table) {
  check <- !check_if_efficiency_mitigator(mitigator, summary_table)
  
  return(check)
}

#' Check if beddays are applicable for the mitigator.
#'
#' This uses `check_if_zero_los_mitigator()` but also allows for other criteria 
#' to be added.
#' 
#' @param mitigator The mitigator or mechanism.
#'
#' @return Boolean.
check_include_beddays <- function(mitigator) {
  check <- !check_if_zero_los_mitigator(mitigator)
  
  return(check)
}

#' Filter a dataframe to data for a mitigator or mechanism.
#'
#' @param data A dataframe.
#' @param mitigator The mitigator or mechanism.
#'
#' @return A dataframe.
filter_to_mitigator_or_mechanism <- function(data, mitigator) {
  filtered <- data |>
    mutate_mechanism_columns() |>
    dplyr::filter(!!rlang::sym(mitigator) == 1)
  
  return(filtered)
}


#' Get a table of the mitigators in a mechanism.
#'
#' @param cohort The mechanism.
#' @param data Output of `get_mitigators_totals()`.
#'
#' @return A table.
get_mitigators_in_a_mechanism_table <- function(data, cohort) {
  
  table <- data |>
    dplyr::filter(mechanism == cohort) |>
    dplyr::select(-mechanism) |>
    get_table() |>
    flextable::set_header_labels(
      mitigator_name = "Mitigator",
      type_of_admission = "Type of admission",
      episodes = "Admissions",
      beddays = "Beddays"
    ) |>
    flextable::delete_part(part = "footer")
  
  return(table)
}

#' Get the total number admissions and beddays for each mitigator.
#'
#' @param summary_table  The `mitigator_summary_table`.
#' @param numbers The target `numbers_over_time`.
#'
#' @return A table.
get_mitigators_totals <- function(summary_table, numbers) {
  totals <- summary_table |>
    dplyr::left_join(
      numbers |>
        dplyr::filter(year == "2023/24") |>
        dplyr::summarise(
          episodes = sum(episodes),
          beddays = sum(beddays),
          .by = cohorts
        ),
      by = c("mitigator_code" = "cohorts")
    ) |>
    dplyr::mutate(
      episodes = ifelse(
        mechanism == "Efficiencies",
        NA,
        janitor::round_half_up(episodes, 0)
      ),
      beddays = janitor::round_half_up(beddays, 0)
    ) |>
    dplyr::select(mechanism, mitigator_name, type_of_admission, episodes, beddays)
  
  return(totals)
}

#' Add columns flagging if a row is part of each mechanism.
#'
#' @param data A dataframe with columns for each mitigator. 
#'
#' @return A dataframe.
mutate_mechanism_columns <- function(data) {
  wrangled <- data |>
    dplyr::mutate(
      prevention = ifelse(
        alcohol_partially_attributable_acute == 1 |
          alcohol_partially_attributable_chronic == 1 |
          alcohol_wholly_attributable == 1 |
          obesity_related_admissions == 1 |
          smoking == 1 |
          raid_ae == 1 |
          intentional_self_harm == 1 |
          medically_unexplained_related_admissions == 1,
        1,
        0
      ),
      redirection_substitution = ifelse(
        ambulatory_care_conditions_acute == 1 |
          ambulatory_care_conditions_chronic == 1 |
          ambulatory_care_conditions_vaccine_preventable == 1 |
          eol_care_2_days == 1 |
          eol_care_3_to_14_days == 1 |
          falls_related_admissions == 1 |
          frail_elderly_high == 1 |
          frail_elderly_intermediate == 1 |
          medicines_related_admissions_explicit == 1 |
          medicines_related_admissions_implicit_anti_diabetics == 1 |
          medicines_related_admissions_implicit_benzodiasepines == 1 |
          medicines_related_admissions_implicit_diurectics == 1 |
          medicines_related_admissions_implicit_nsaids == 1 |
          readmission_within_28_days == 1 |
          zero_los_no_procedure_adult == 1 |
          zero_los_no_procedure_child == 1,
        1,
        0
      ),
      efficiencies_relocation = ifelse(
        virtual_wards_activity_avoidance_ari == 1 |
          virtual_wards_activity_avoidance_heart_failure == 1,
        1,
        0
      ),
      efficiencies = ifelse(
        emergency_elderly == 1 |
          stroke_early_supported_discharge == 1 |
          raid_ip == 1,
        1,
        0
      )
    )
  
  return(wrangled)
}

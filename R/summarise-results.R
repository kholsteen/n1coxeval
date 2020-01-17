#' Summarize results
#' @param data tbl of saved results, slightly modified by `summarise_results_wrapper`
#' @param sim (boolean) TRUE = simulated data, FALSE = case study data
#' @param multivariable (boolean) TRUE = multivariable data, FALSE = univariable data
#' @param rowwise (boolean) TRUE = just return the rowwise summarized data
#' @param row_errors (boolean) TRUE = just return the error classifications for each row
#' @keywords internal
summarise_results <- function(data,
                              sim = TRUE,
                              multivariable = FALSE,
                              rowwise = FALSE,
                              row_errors = FALSE) {

  if (sim) {
    group_by_vars <- c("p", "n.events", "ub", "prior_normal", "beta.true",
                     "beta.1.true",
                     "wb.shape", "freq", "var.types", "var")
  } else {
    group_by_vars <- c("id", "n.events", "ub", "prior_normal", "var.types", "var")
  }

  data_ungrouped <- data %>%
    dplyr::rowwise() %>%
    rowwise_summary() %>%
    dplyr::mutate(var.types = unlist(.data$var.types)) %>%
    dplyr::ungroup()

  if (rowwise) { return(data_ungrouped)}

  if (row_errors) {
    error_rowwise <- data_ungrouped %>%
      dplyr::group_by_at(c("p", "id", "n.events",
                           "beta.true", "var.types")) %>%
      dplyr::summarise(
        est_issue_mle_conv = sum(.data$ub == 1000 & .data$est_issue == 1),
        est_issue_mle_se_g200 = sum(.data$ub == 1000 & .data$est_issue == 2),
        est_issue_mle_overflow = sum(.data$ub == 1000 & .data$est_issue == 3),
        est_issue_mle_beta_na = sum(.data$ub == 1000 & .data$est_issue == 4),
        est_issue_mle_none = sum(.data$ub == 1000 & .data$est_issue == 0),
        est_issue_coxphf = sum(.data$ub == 900 & .data$est_issue == 5),
        est_issue_coxphf_none = sum(.data$ub == 900 & .data$est_issue == 0),
      )
    return(error_rowwise)
  }

  res.smry <- data_ungrouped %>%
    dplyr::group_by(!!! rlang::syms(group_by_vars)) %>%
    reps_summary()

  #### Summarize results from multivariable ####
    if (multivariable) {

      res.smry %>% multivariable_summary()

    } else {
      res.smry %>%
        dplyr::ungroup()
    }
}












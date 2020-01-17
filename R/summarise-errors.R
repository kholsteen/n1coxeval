#' Summarise Errors
#' @keywords internal
#' @param results_dir results_dir directory of saved RDS results from estimation step
#' @param mv (boolean) TRUE = multivariable simulation, FALSE = univariable
#' @param sim (boolean) TRUE = simulated data, FALSE = case study

summarise_errors <- function(results_dir, mv, sim) {

  res_errors_only <- summarise_results_wrapper( results_dir,
                                                rowwise = FALSE,
                                                mv = mv,
                                                sim = sim,
                                                row_errors = TRUE)


  errors <- res_errors_only %>%
    dplyr::group_by_at(c("p", "var.types", "n.events", "beta.true")) %>%
    dplyr::summarise_at(dplyr::vars(dplyr::starts_with("est_issue")), sum) %>%
    dplyr::filter(.data$est_issue_mle_none < 1000) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      Variable = dplyr::case_when(
        var.types == "b" ~ "Dichotomous",
        TRUE ~ "Continuous"
      )) %>%
    dplyr::select(.data$Variable, .data$n.events, .data$beta.true,
                  `MLE: Convergence Warning` = .data$est_issue_mle_conv,
                  `MLE: SE > 200, No Warning` = .data$est_issue_mle_se_g200,
                  `MLE: Numeric Overflow Error` = .data$est_issue_mle_overflow,
                  `MLE: Estimate NA, No Warning` = .data$est_issue_mle_beta_na,
                  `MLE: No Problems` = .data$est_issue_mle_none,
                  `Firth: Convergence Warning` = .data$est_issue_coxphf,
                  `Firth: No Problems` = .data$est_issue_coxphf_none)
  errors
}

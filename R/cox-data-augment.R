#' Cox Regression with option of semi-Bayesian estimation through augmented data
#' @description Estimates either standard Cox regression (MLE), Firth method, or
#' approximate Bayesian estimation given an upper 95% confidence limit for
#' the prior hazard ratio.
#' @param t0_var (string) variable name for observation start time
#' @param t1_var (string) variable name for observation end time
#' @param y_var (string) variable name for event indicator (1/0)
#' @param x_var (string) variable name for predictor
#' @param prior_hr_ub_95 (numeric) upper 95% confidence limit for prior hazard
#' ratio. If infinite, will estimate with MLE method. If NA, will use Firth
#' method.
#' @param data dataset for estimation
#' @param normal (Boolean) use a normal distribution? If FALSE, then use a log-F distribution
#' for the log-hazard ratio
#' @param return_as (Boolean) return parameter values `A`
#' (number of cases represented by each prior data record) and
#' `S` (scale factor)?
#' If TRUE, then return them along with the primary results and the dataset.
#' @references Sullivan SG, Greenland S. Bayesian regression in SAS software. Int J Epidemiol. 2013;42:308-317.
#' @keywords internal
#' @importFrom rlang :=
#'
cox_data_augment <- function(t0_var,
                             t1_var,
                             y_var,
                             x_var,
                             prior_hr_ub_95 = Inf,
                             data = NULL,
                             normal = 0,
                             return_as = FALSE
                              ) {

  ## If prior is NA, then use FIRTH
  if (!is.na(prior_hr_ub_95)) {

    if (!is.infinite(prior_hr_ub_95)) {

      a = 2/((log(prior_hr_ub_95)/1.96)^2)

      if (normal) {
        ## Normal distribution
        if (a <= 3) {
          s = 10
          a = (s^2)*a
        } else {
          s = 1
          a = a + 1/2
        }
      } else {
        ## F distribution
        s = 1
      }

      max_t1_data <-  max(data %>% dplyr::pull(t1_var))

      data_prior <- tibble::tibble(
        wt__ = a,
        !!t0_var := rep(max_t1_data + c(1, 2), each = 2),
        !!t1_var := rep(max_t1_data + c(3, 4), each = 2),
        !!x_var := c(1, 0, 1, 0)/s,
        !!y_var := c(0, 1, 1, 0)
      )

      ## Cox with only the prior to confirm the empirical upper bound
      sc <-  try(suppressWarnings(
        survival::coxph(stats::as.formula(
        paste0("survival::Surv(", t0_var, ", ", t1_var, ", ", y_var, ") ~ ", x_var)),
        data = data_prior, ties = "efron",
        weights = wt__)), silent = TRUE)

      empir_ub <- exp(stats::confint(sc, parm = x_var, level = 0.95))[2]

      stopifnot(abs(empir_ub - prior_hr_ub_95)/prior_hr_ub_95 < 0.01)

      data <- data_prior  %>%
        dplyr::bind_rows(data %>% dplyr::mutate(wt__ = 1))

    ## Frequentist case; Prior is infinite
    }  else {
      a = NULL
      s = NULL
      data <- data %>% dplyr::mutate(wt__ = 1)
    }
    ## Cox with the prior and observed data to generate the final estimate
    rc <- try(
      survival::coxph(stats::as.formula(
      paste0("survival::Surv(", t0_var, ", ", t1_var, ", ", y_var, ") ~ ", x_var)),
      data = data, ties = "efron",
      weights = wt__), silent = TRUE)

  } else {
    a = NA
    s = NA

    rc <- try(
      coxphf::coxphf(stats::as.formula(
       paste0("survival::Surv(", t0_var, ", ", t1_var, ", ",
              y_var, ") ~ ", x_var)),
       data = data,
       pl = TRUE,
       firth = TRUE,
       maxit = 1000,
       maxstep = 0.05
       ), silent = TRUE)

  }

  if (!return_as) {
    return(rc)
  } else if (return_as) {
    return(list(model = rc, A = a, S = s, data = data))
  }

}







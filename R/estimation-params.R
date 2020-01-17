#' Create table of model estimation parameters.
#' @description Model estimation parameters include the number of events,
#' the 95% prior upper bound on the hazard ratio, and whether the
#' prior distribution is normal or log-F.
#' @export
#' @param mv (Boolean) TRUE if multivariable, FALSE if univariable
#' @param sim (Boolean) TRUE if using simulated data, FALSE if real data
#' @return tbl with columns specifying estimation parameters:
#' \describe{
#'   \item{n.events}{number of events (cumulative) to include in estimation}
#'   \item{prior_hr_ub}{prior upper bound on hazard ratio. For proper normal or log-F priors on the log-hazard ratio, this is numeric. `Inf` indicates maximum partial likelihood estimation and `NA` indicates Firth estimation.}
#'   \item{prior_normal}{Boolean indicating whether the prior on the log-hazard ratio is normal (1) or log-F (0)}
#' }

estimation_params <- function(sim, mv) {

  if (mv & sim ) { n.events = c(6, 8, 10, 12, 20, 40, 60, 80)
  } else if (sim) { n.events  = c(6, 8, 10, 12, 20, 40, 60, 80)
  } else { n.events = c(5, 10, 15, 20) }

  if (mv) { prior_hr_ub = Inf
  } else { prior_hr_ub = c(Inf, NA, 648, 40, 16) }

  expand.grid(
    n.events = n.events,
    prior_hr_ub = prior_hr_ub,
    prior_normal = 0,
    stringsAsFactors = FALSE
  ) %>% tibble::as_tibble() %>%
  dplyr::filter(!(.data$prior_normal == 1 &
                    (is.infinite(prior_hr_ub) | is.na(prior_hr_ub))))
}


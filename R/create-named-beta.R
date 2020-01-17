#' Create named vector of true coefficients
#' @export
#' @param sim (boolean) TRUE = simulated data, FALSE = case study data
#' @param param_row (integer) which row of the parameter dataset to use
#' (only relevant for `sim = TRUE`)
#' @return named numeric vector of coefficients

create_named_beta <- function(sim = TRUE, param_row = NA) {

  #### Specify the parameter vector, X vars, # events, and beta ####
  if (sim) {
    x <- param_row
    xvars = paste0("X", stringr::str_pad( c(1:(x$n.vars)),
                                          pad = "0", width = 3))
    beta <- x$beta[[1]]

  } else {
    x <- NA
    xvars <- case_study_vars()
    beta <- rep(NA, length(xvars))
  }
  names(beta) <- xvars

  beta
}


#' Variable names for case study
#' @keywords internal
case_study_vars <- function() {
  c("stress_global_lag1",
    "sleep_hours_lag1",
    "caff_count_dif_lag1")
}

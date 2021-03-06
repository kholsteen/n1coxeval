#' Generate outcomes for the proportional hazards model
#' @param rates (numeric vector) piecewise exponential event rates per time unit,
#' generated by `generate_pred_wrapper`
#' @param g.inv.t.min (numeric) minimum allowed event time, transformed by `generate_time_transform`
#' @param g.inv.t.max (numeric) maximum allowed event time, transformed by `generate_time_transform`
#' @param g.inv.t (numeric vector) time units corresponding to piecewise exponential rates, transformed by `generate_time_transform`
#' @references Hendry DJ. Data generation for the Cox proportional hazards model with time-dependent covariates: A method for medical researchers. Stat Med. 2014;33:436-454.
#'
#' @keywords internal
generate_outcomes <- function(rates, g.inv.t.min, g.inv.t.max, g.inv.t) {

  # M is the difference between the CDF of truncation bounds
  #"Therefore, for any given bounds of truncation {a,b} we can let M = KY(b)/KY(a)"
  d <- msm::ppexp(q = g.inv.t.max, rate = rates, t = g.inv.t) -
    msm::ppexp(q = g.inv.t.min, rate = rates, t = g.inv.t)
  M <- 1/d
  r <- 10
  j = 0
  repeat{
    # Random draws from piecewise exponential on transformed time scale
    ## Generate Y from k
    y <- msm::rpexp(n = r, rate = rates, t = g.inv.t)
    # Hendry p. 440
    t0 <- M * ((k(x = y,
                  lower_bound = as.numeric(g.inv.t.min),
                  upper_bound = as.numeric(g.inv.t.max),
                  pexp_rates = rates,
                  pexp_t = g.inv.t) / d / msm::dpexp(y, rates, g.inv.t)))

    j = j + 1
    if (j == 10000) {
      stop("Hit 10000...")
      # Perhaps want to build in a workaround when this occurs;
      # like deleting the first day of data
    }
    u <- runif(r)
    y <- y[u <= t0][1]
    if (!is.na(y)) break
  }
  y
}

#' Accept-reject function
#' @keywords internal
#' @description Returns piecewise exponential density of `x` if `x` inside bounds,
#' otherwise returns zero.
#' @param x value of simulated piecewise exponential
#' @param lower_bound minimum allowed event time, transformed by `generate_time_transform`
#' @param upper_bound maximum allowed event time, transformed by `generate_time_transform`
#' @param pexp_rates piecewise exponential rates
#' @param pexp_t time units corresponding to piecewise exponential rates, transformed by `generate_time_transform`
#' @references Hendry DJ. Data generation for the Cox proportional hazards model with time-dependent covariates: A method for medical researchers. Stat Med. 2014;33:436-454.

k <- function(x, lower_bound, upper_bound, pexp_rates, pexp_t){
  ifelse(x <= lower_bound | x >= upper_bound, 0, msm::dpexp(x, pexp_rates, pexp_t))
}




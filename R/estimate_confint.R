#' Profile Likelihood Confidence Interval
#' @description Firth model returns confidence interval from original estimation;
#' other models (estimated with `coxph`) have profile CI separately estimated.
#' @keywords internal
#' @param mod Model object (of class `coxph` or `coxphf`)
#' @param varname (string) Predictor variable
#' @param data (tbl) dataset used in estimation; needed for profile likelihood
#' interval with `coxph`.
#' @importFrom stats coef
estimate_confint <- function(mod, varname, data = NA) {
  if (length(mod) == 1) {
    if (class(mod) == "list") {
      mod <- mod[[1]]
    }
  }
  if (inherits(mod, "coxphf")) {

    se <- sqrt(mod$var[varname, varname])

    res <- c(coef(mod)[varname],
             se,
             NA, NA,
             NA, NA,
             NA, NA,
             log(mod$ci.lower),
             log(mod$ci.upper))

  } else if (inherits(mod, "coxph")) {

    stopifnot(!is.null(names(summary(mod))))

    se <- summary(mod)$coefficients[varname, "se(coef)"]
    ci <- profile_ci(mod, parm = varname, CI = 0.95, data = data)

    res <- c(coef(mod)[varname],
             se,
             NA, NA,
             NA, NA,
             NA, NA,
             ci)

  } else {
    stop("`mod` must inherit from class `coxph` or `coxphf`")
  }
  res <- as.data.frame(t(res))
  colnames(res) <- c("beta", "se", "lb80", "ub80", "lb85", "ub85",
                     "lb90", "ub90","lb95", "ub95")
  return(res)
}

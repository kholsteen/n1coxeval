#' @name profile_ci
#' @keywords internal
#' @title Profile likelihood for coefficients in a \code{coxph} model
#' @description Profile likelihood for coefficients in a \code{coxph} model
#' @param x A \code{coxph} model.
#' @param parm (string) name of variable for which to evaluate CI.
#' @param CI nominal coverage level of \bold{C}onfidence \bold{I}nterval.
#' @param data (tbl) Dataset used to estimate the original model
#' @references Much of this code was taken from \code{survMisc::profLik}.

profile_ci <- function(x,
                       parm,
                       CI = 0.95,
                       data) {

  if (!inherits(x, "coxph")) stop ("Only applies to objects of class coxph")


  if (CI <= 0 | CI >=1 )  stop("CI must be strictly between 0 and 1")

  predictor_var_names <- names(stats::model.frame(x))[!grepl( ("Surv|weights"),
                                             names(stats::model.frame(x)) )]

  which_var = which(predictor_var_names == parm)
  estimated_param <- stats::coef(x)[which_var]
  maxloglik = max_log_lik(x)

  start_intervals <- list(
    c(estimated_param - 0.01, estimated_param),
    c(estimated_param, 0.01 + estimated_param)
  )

  upper_bound <- list(FALSE, TRUE)

  ci_limits <- purrr::pmap_dbl(list(upper =upper_bound,
                                    start_interval = start_intervals),

                                find_zero,

                                f = f,
                                predictor_var_names = predictor_var_names,
                                which_var = which_var,
                                model_obj = x,
                                maxloglik = maxloglik,
                                CI = CI,
                                data = data)

  ci_limits

}

#' Find zeros of a concave function
#' @param f Function
#' @param upper (Boolean) Search x-values above (TRUE) or below (FALSE)
#' the `start_interval`
#' @param start_interval (numeric, 2-component vector) initial interval of x-values
#' in which to search for the zero. If no zero-crossing, then this function will
#' search additional x-values in the direction specified by `upper`
#' @note This method assumes a concave function.
#' @keywords internal

find_zero <- function(f, upper = TRUE, start_interval, ...) {

  if (upper) {
    cross_dir = "downX"
    backup_value = Inf
  } else {
    cross_dir = "upX"
    backup_value = -Inf
  }

  root_obj <- try(stats::uniroot(f,
              extendInt = cross_dir,
              interval = start_interval,
              ...), silent = TRUE)


  if (inherits(root_obj, "try-error")) {
    backup_value
  } else {
    root_obj$root
  }
}

#' Difference between profile log-likelihood and CI cutoff value.
#' @description The zeros of this function are the confidence limits for
#' the estimated parameter.
#' @keywords internal
#' @param initial_estimate initial estimate
#' @param predictor_var_names (character vector) names of predictor variables
#' @param which_var (integer) index (into `predictor_var_names` of the variable
#' for which to create the confidence interval
#' @param model_obj original model object
#' @param maxloglik maximum log-likelihood from original model
#' @param CI (numeric) nominal confidence interval coverage level, strictly
#' between 0 and 1
#' @references Much of this code was taken from
#' Patterson, David. "Profile Likelihood Confidence Intervals for GLM's."
#' David Patterson, Math, U. of Montana.
#' http://www.math.umt.edu/patterson/ProfileLikelihoodCI.pdf Accessed 2020-01-09.

f <- function(initial_estimate, predictor_var_names,
              which_var, model_obj, maxloglik, CI, data){

  ci_cutoff <- maxloglik - qchisq(CI,1)/2

  ### Set up formula to use predictors except the `which_var`,
  ### and use `which_var` as an offset
  rhs0 <- paste0(predictor_var_names[-which_var], collapse = "+")
  offst <- paste0("offset(", initial_estimate, "*",
                  predictor_var_names[which_var], ")")
  rhs_offst <- paste0(rhs0, offst, collapse = '+')
  formula_updated <- stats::as.formula(paste0(".~", rhs_offst))

  ## This fails when the offset leads to an infinite risk score.
  model_updated <- try(stats::update(model_obj,
                                     formula = formula_updated),
                       silent = TRUE)

  if (inherits(model_updated, "try-error")) {
    NA
  } else {

    ## Difference between log-likelihood for the reduced model
    ## (profile likelihood) and the cutoff value for the CI.
    ## Where this exceeds zero, the initial estimate is in the confidence interval.
    max_log_lik(model_updated) - ci_cutoff
  }
}

max_log_lik <- function(model_obj) {
  model_obj$loglik[length(model_obj$loglik)]
}








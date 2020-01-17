#' Wrapper for Cox Model estimation
#' @param data (tbl) simluated or case study dataset from `generate_data_wrapper`
#' @param n (integer) number of events to include (will use the first `n` events)
#' @param u prior 95-percent upper bound on hazard ratio.
#' @param r (Boolean) use a normally distributed (TRUE) or F-distributed (FALSE)
#' prior on the hazard ratios?
#' @param var (string) predictor variable name
#' @param error_dir output directory - really just to save errors
#' @param scale_x (Boolean) scale X to have a standard deviation of 1 (TRUE)
#' or leave the current standard deviation (FALSE). (Note: X is always centered,
#' the option here is just whether to modify the standard deviation)
#' @importFrom stats coef confint median pnorm power
#' qchisq quantile rnorm runif sd uniroot var
#' @export
#' @return tibble with confidence intervals and details for specified model

cox_model_wrapper <- function(data, n, u, r, var, error_dir, scale_x = FALSE) {

  data_cox <- data %>%
    dplyr::filter(data$mig.num <= n) %>%
    dplyr::mutate_at(var, scale, scale = scale_x)

  error <- ""

  if (nrow(unique(data_cox[, var])) == 1) {
    error <- "Only 1 unique value"
    md <- NA
    class(md) <- "try-error"
    attributes(md)$condition = error

  } else {

    md <- tryCatch(cox_data_augment(prior_hr_ub_95 = u,
                                        data = data_cox,
                                        t0_var = "t0",
                                        t1_var = "t1",
                                        y_var = "mig.ind",
                                        x_var = var,
                                        normal = r,
                                        return_as = TRUE),
                   warning = function(w) {
                     toreturn <- suppressWarnings(cox_data_augment(prior_hr_ub_95 = u,
                                               data = data_cox,
                                               t0_var = "t0",
                                               t1_var = "t1",
                                               y_var = "mig.ind",
                                               x_var = var,
                                               normal = r,
                                               return_as = TRUE))
                     toreturn$warning = w
                     return(toreturn)
                   },
                   error = function(e) { e })
  }

  if (!(inherits(md, "error") | inherits(md, "try-error"))) {


    if (!(inherits(md$model, "error") | inherits(md$model, "try-error"))) {
      ### Model OK, can estimate confidence interval
      ci <- tibble::as_tibble(estimate_confint(md$model, varname = var,
                                                        data = md$data))

    } else {

      ### Model had fundamental error, cannot estimate CI
      error <- as.character(attributes(md$model)$condition[1])

      ci <- tibble::tibble(beta = NA,
                              se = NA,
                             lb80 = NA,
                             ub80 = NA,
                             lb85 = NA,
                             ub85 = NA,
                             lb90 = NA,
                             ub90 = NA,
                             lb95 = NA,
                             ub95 = NA)

      saveRDS(data_cox,
              file.path(error_dir, paste0("model-try-error-",
                                        paste(var, n, u, r, sep = "-"), "-",
                                        dplyr::first(data_cox$p), ".rds")))

    }

    if (is.null(md$A)) { md$A = NA}
    if (is.null(md$S)) { md$S = NA}

    if ("warning" %in% names(md)) { error = paste(md$warning, collapse = " + ")}

    if ("concordance" %in% names(md$model)) {
      discordant <- md$model$concordance[["discordant"]]
      concordant <- md$model$concordance[["concordant"]]
    } else {
      discordant <- NA
      concordant <- NA
    }

    if ("nevent" %in% names(md$model)) {
      n.events.in.fit <- md$model$nevent
    } else {
      n.events.in.fit <- NA
    }

    if (inherits(md$model, "coxph") & !inherits(md$model, "coxphf")) {
      fitd <- try(survival::coxph.detail(md$model))

      if (!inherits(fitd, "try-error")) {

        n_times_with_events <- length(fitd$time)
        n_times_with_ties <- sum(fitd$nevent > 1)
        n_tied_events <- sum(fitd$nevent[which(fitd$nevent > 1)])
        med_event_time <- stats::median(rep(fitd$time, times = fitd$nevent))

      } else { # try-error
        n_times_with_events <- NA
        n_times_with_ties = NA
        n_tied_events = NA
        med_event_time = NA
      }

    } else { # wrong type of model to use coxph.detail

      n_times_with_events <- NA
      n_times_with_ties = NA
      n_tied_events = NA
      med_event_time = NA
    }

    ### Save final results to return
    output <- ci %>% dplyr::mutate(
      var = var,
      ub = u,
      prior_normal = r,
      a = md$A,
      s = md$S,
      a.over.s2 = md$A/(md$S^2),
      id = dplyr::first(data_cox$id),
      p = dplyr::first(data_cox$p),
      n.events = n,
      n.events.in.fit = n.events.in.fit,
      error = error,
      discordant = discordant,
      concordant = concordant,
      n_times_with_events = n_times_with_events,
      n_times_with_ties = n_times_with_ties,
      n_tied_events = n_tied_events,
      med_event_time = med_event_time)

  } else {

    ### Model wrapper had fundamental error, cannot estimate anything
    error <- as.character(md)
    output <- tibble::tibble(beta = NA,
                             se = NA,
                             lb80 = NA,
                             ub80 = NA,
                             lb85 = NA,
                             ub85 = NA,
                             lb90 = NA,
                             ub90 = NA,
                             lb95 = NA,
                             ub95 = NA,
                             var = var,
                             ub = u,
                             prior_normal = r,
                             a = NA,
                             s = NA,
                             a.over.s2 = NA,
                             id = dplyr::first(data_cox$id),
                             p = dplyr::first(data_cox$p),
                             n.events = n,
                             error = error)

    saveRDS(data_cox %>% dplyr::filter(.data$mig.num <= n),
            file.path(error_dir, paste0("data-try-error-",
                                      paste(var, n, u, r, sep = "-"), "-",
                                      dplyr::first(data_cox$p), ".rds")))


  }

  output %>%
    dplyr::mutate(var = var,
                  nrow = nrow(data_cox))

}

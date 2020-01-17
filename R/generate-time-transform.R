#' Generate the time transformation g() and g^-1()
#' @param weibull_shape (numeric) Weibull distribution shape parameter; shape > 1
#' means hazard is increasing, 1 = constant, and <1 is decreasing.
#' @param beta (numeric vector) Coefficient vector, used to generate the weibull
#' scale parameter
#' @param median_event_time (integer) target median days between events
#' (time-to-event)
#' @param x.list (list of tbls) Output from generate_pred_wrapper, need when
#' use_weibull = TRUE, to generate the weibull scale parameter
#' @param verbose (Boolean) print calculated Weibull scale (TRUE) or not (FALSE)
#' @description As described in Hendry (2013) and Montez-Rath (2017),
#' generate a function `g` and its inverse. The inverse represents the process
#' of interest as the cumulative baseline hazard.
#' @return list with two components: functions `g.inv` and `g`
#' @keywords internal
#' @references Hendry DJ. Data generation for the Cox proportional hazards model with time-dependent covariates: A method for medical researchers. Stat Med. 2014;33:436-454.
#'
#' Montez-Rath ME, Kapphahn K, Mathur MB, Mitani AA, Hendry DJ, Desai M. Guidelines for generating right-censored outcomes from a Cox model extended to accommodate time-varying covariates. J Mod Appl Stat Methods. 2017;16:86-106.
#'
#' @note Relying on the Weibull scale calculation from Montez-Rath (2017) did
#' not work well for our parameters. Figured out approximate method for this
#' specific case but future versions will need to update and systematize to
#' make the generated median event times reliable in other parameter settings.
generate_time_transform <- function(
                          weibull_shape = 2,
                          beta = NULL,
                          median_event_time = NULL,
                          x.list = NULL,
                          verbose = FALSE) {

  stopifnot(!is.null(x.list))

  xbind <- dplyr::bind_rows(x.list) %>% dplyr::select(-.data$rate) %>% data.matrix()

  lp.all <- exp(xbind %*% beta)

  ########################### NOTE ##########################################
  # Hacked weibull scale to get target median event time because
  # just relying on the mean beta*Z did not work well for our parameters.
  # Future versions will need to update
  # and systematize to make this usable in other parameter settings.

  if (weibull_shape != 2) {
    warning("Time transform is set up for Weibull shape of 2. With a different shape, median event times may not match target.")
  }

  if (length(unique(xbind)) == 2) {
    maxx <- if (length(beta) == 1 && beta == 0) {  max(xbind %*% log(1.5))
      } else { max(xbind %*% beta) }
    wb.scale <- (log(2)/(exp(0.5*maxx)))*((median_event_time-0.5)^(-weibull_shape))
  } else {
    # print(beta)
    # print(dim(beta))
    beta_mult <- if (length(beta) == 1 && beta == 0) { log(1.75)
    } else if (length(beta) > 1 && max(beta) > 2) { beta*1.1
    } else { beta }
    pctile <- if (length(beta) == 1 ) { 0.25 } else { 0.25 }
    # print(beta_mult)
    # print(dim(beta_mult))
    wb.scale.all <- (log(2)/(exp(xbind %*% beta_mult)))*((median_event_time-0.5)^(-weibull_shape))
    # print(summary(wb.scale.all))
    wb.scale <- quantile(wb.scale.all, pctile)
  }
  ############################################################################

  if (verbose) { cat(paste0("Weibull Scale Parameter: ", round(wb.scale, 5), "\n")) }
  g.inv = function(x) { purrr::map_dbl(x, function(xi) wb.scale*(xi^weibull_shape))}
  g = function(x) { purrr::map_dbl(x, function(xi) (xi/wb.scale)^(1/weibull_shape))}

  list(g = g, g.inv = g.inv)

}

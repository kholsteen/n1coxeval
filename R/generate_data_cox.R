#' Generate data based on Cox PH with time-varying covariates
#' @description Main function to generate the dataset based on Cox PH model.
#'             Algorithm from Hendry (2014) with guidelines from Montez-Rath (2017)
#' @param n.subj  number of subjects to generate
#' @param n.vars number of predictor variables to generate
#' @param var.types types of predictor variables to generate
#' @param beta (numeric vector) true coefficient values. Length `n.vars`
#' @param rho (numeric vector) autocorrelation coefficient (unused)
#' @param freq (numeric vector) frequency of dichotomous var
#' @param max.n.days (integer)
#' @param n.events (integer) number of outcome events to generate
#' @param t.min (integer) minimum time-to-event
#' @param t.max (integer) maximum time-to-event (must be greater than `t.min`)
#' @param median.days.btwn.events (integer) target median days between events
#' (time-to-event)
#' @param wb.shape Weibull shape parameter
#' @param round_event_times (Boolean) TRUE = round event times up to the nearest
#' integer, FALSE = keep exact event times
#' @param verbose (Boolean) TRUE = print progress, FALSE = print nothing
#' @return dataset for analysis
#' @importFrom magrittr %>%
#' @keywords internal
#' @references Hendry DJ. Data generation for the Cox proportional hazards model with time-dependent covariates: A method for medical researchers. Stat Med. 2014;33:436-454.
#'
#' Montez-Rath ME, Kapphahn K, Mathur MB, Mitani AA, Hendry DJ, Desai M. Guidelines for generating right-censored outcomes from a Cox model extended to accommodate time-varying covariates. J Mod Appl Stat Methods. 2017;16:86-106.
#'
generate_data_cox <- function(n.subj = 1,
                              n.vars = 10,
                              var.types,
                              beta = rep(0, n.vars),
                              rho = rep(0, n.vars),
                              freq = rep(0.5, n.vars),
                              max.n.days = NA,
                              n.events = 50,
                              t.min = 0,
                              t.max = 60,
                              median.days.btwn.events = 7,
                              wb.shape = 1,
                              round_event_times = FALSE,
                              verbose = FALSE) {

  stopifnot(length(beta) == n.vars)
  stopifnot(t.max > t.min)

  if (verbose) { tictoc::tic("\nGenerate the predictors") }
  #### Basic setup ####
  events.per.person = rep(n.events + 1, times = n.subj)
  ##### Generate predictors ####
  ### This yields a
  time_units <- 0:t.max
  x.list <- generate_pred_wrapper(n_events = sum(events.per.person),
                                  time_units = time_units,
                                  var.types = var.types,
                                  rho = rho,
                                  freq = freq,
                                  beta = beta)
  if (verbose) { tictoc::toc() }

  if (verbose) { tictoc::tic("Generate the outcomes") }
  #### Set the time transformation ####
  g_list <- generate_time_transform(x.list = x.list,
                                    weibull_shape = wb.shape,
                                    beta = beta,
                                    median_event_time = median.days.btwn.events,
                                    verbose = verbose)
  g.inv.t <- g_list$g.inv(time_units)
  g.inv.t.max <- g_list$g.inv(t.max)
  g.inv.t.min <- g_list$g.inv(t.min)

  g_inv_event_times <- purrr::map_dbl(x.list, function(dta) {
    generate_outcomes(dta$rate, g.inv.t.min, g.inv.t.max, g.inv.t)
  })

  event_times <- g_list$g(g_inv_event_times)
  if (verbose) { tictoc::toc()}

  #### GIVEN EVENT TIMES, ORGANIZE OUTPUT DATA ####
  if (verbose) { tictoc::tic("Organize the data") }

  data <- generate_final_data( event_times = event_times,
                               events.per.person = events.per.person,
                               n.subj = n.subj,
                               x.list = x.list,
                               max_time_for_person = max.n.days,
                               round_event_times = round_event_times)
  if (verbose) { tictoc::toc() }

  data
}






#' Generate simulated or case study data for n-of-1 Cox regression analysis.
#' @export
#' @param sim (Boolean) Simulate data (TRUE) or read in case study data (FALSE)
#' @param param (tbl) 1-row tibble of parameters, output from `create_params`
#' @param x_vars (Character vector) predictor variable names to use
#' @param p label for this parameter specification (usually the row number of the
#' parameter data)
#' @param verbose (Boolean) TRUE = print progress, FALSE = print nothing
#' @return list of tibbles, one per (simulated or case study) participant.
#' Each row represents a participant-day.
#' For simulated data, each tibble has the following variables:
#' \describe{
#'   \item{id}{Study participant ID}
#'   \item{mig.num}{Next headache number to occur}
#'   \item{t0}{Observation start time, in days (always an integer)}
#'   \item{t1}{Observation end time, in days}
#'   \item{mig.ind}{Indicator for a day with new headache onset}
#'   \item{X001...X00`n`}{Predictor variables, `n` columns}
#'   \item{order}{sort order for participant-days}
#'   \item{p}{Parameter specification under which these data were generated}
#' }
#' For case study data, the variables are documented under `case_study_data.`
generate_data_wrapper <- function(sim = TRUE,
                                  param = NA,
                                  x_vars = NA,
                                  p = 1,
                                  verbose = FALSE) {

  if (sim) {
    data_list <- generate_data_cox(n.subj = param$n.subj,
                        n.events = param$n.events.max,
                        n.vars = param$n.vars,
                        var.types = unlist(param$var.types),
                        beta = param$beta[[1]],
                        rho = param$rho[[1]],
                        freq = param$freq,
                        median.days.btwn.events =
                          param$median.days.btwn.events,
                        wb.shape = param$wb.shape,
                        round_event_times = param$round_event_times,
                        verbose = verbose) %>%
      tibble::as_tibble() %>%
      dplyr::mutate(p = p) %>%
      split(.$id)

  } else {

    data_list <- case_study_data

  }

  data_list

}






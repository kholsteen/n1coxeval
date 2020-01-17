#' Organize the predictors and event times into final data set.
#' @param event_times (numeric vector) actual event times
#' @param events.per.person (numeric vector) number of events for each subject
#' (length should equal n.subj)
#' @param n.subj (integer) number of subjects)
#' @param x.list (list of tbls) list of predictor data output from `generate_pred_wrapper()`
#' @param max_time_for_person (integer, optional) maximum number of time units.
#' Each subject's follow-up time will be cut off after `max_time_for_person` (or
#' not at all , if set to NA).
#' @keywords internal
#' @references Hendry DJ. Data generation for the Cox proportional hazards model with time-dependent covariates: A method for medical researchers. Stat Med. 2014;33:436-454.
#'
generate_final_data <- function(
  event_times,
  events.per.person,
  n.subj,
  x.list,
  max_time_for_person = NA,
  round_event_times = FALSE
) {

  stopifnot(length(events.per.person) == n.subj)
  stopifnot(length(event_times) == length(x.list))

  mig.num = unlist(lapply(events.per.person, function(e0) {c(1:e0)}))
  id.per.event = rep(1:(n.subj), times = events.per.person)

  data <- purrr::imap(event_times, function(exact_event_time, i) {

    n_time_units = ceiling(exact_event_time)
    if (round_event_times) {
      event_time_final <- n_time_units
    } else {
      event_time_final <- exact_event_time
    }

    ## Returns NULL if exact_event_time > 1 is FALSE
    t1_pre <- switch(exact_event_time > 1, 1:(n_time_units - 1))

    tibble::tibble(

      id = rep(id.per.event[i], n_time_units),
      mig.num = rep(mig.num[i], n_time_units),
      t0 = 0:(n_time_units - 1),
      t1 = c(t1_pre, event_time_final) ,
      mig.ind = c(rep(0, n_time_units - 1), 1)) %>%

      dplyr::bind_cols(

        x.list[[i]][1:n_time_units, ]

      )
  }) %>% dplyr::bind_rows() %>%
    ## Filter out the day with the last extra migraine
    dplyr::group_by(.data$id) %>%
    dplyr::mutate(n.events = max(mig.num)) %>%
    dplyr::filter(!(mig.num == .data$n.events + 1 & .data$mig.ind == 1)) %>%
    dplyr::select(-n.events) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(order = dplyr::row_number()) %>%
    dplyr::select(-.data$rate)

  if (! is.na(max_time_for_person)) {

    data <- data %>%
      ##Drop if days > max:
      dplyr::group_by(.data$iter, .data$id) %>%
      dplyr::filter( dplyr::row_number(.data$id) <  max_time_for_person)
  }

  data

}

#' Migraine headache case study data.
#'
#' Person-day level dataset containing headache timing and daily measures of
#' potential trigger factors for 4 participants in a 90-day daily diary
#' study. Each person's dataset contains their at-risk days (days
#' which began headache-free).
#'
#' @format A list of 4 tibbles (one per participant), each with 10 variables:
#' \describe{
#'   \item{id}{Study participant ID}
#'   \item{day}{Day of study for participant}
#'   \item{mig.num}{Next headache number to occur}
#'   \item{mig.ind}{Indicator for a day with new headache onset}
#'   \item{t0}{Observation start time, in days (always an integer)}
#'   \item{t1}{Observation end time, in days}
#'   \item{stress_global_lag1}{Yesterday's stress rating (centered and
#'   scaled within person to have a SD of 0.5)}
#'   \item{sleep_hours_lag1}{Yesterday's hours of sleep (centered and
#'   scaled within person to have a SD of 0.5)}
#'   \item{caff_count_dif_lag1}{Change in cups of caffeinated beverage
#'   consumption between yesterday and the previous day.
#'   (centered and scaled within person to have a SD of 0.5)}
#'   \item{p}{Placeholder for parameter specification, always 1 for case study}
#' }
"case_study_data"

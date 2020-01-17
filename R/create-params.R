#' Create and save parameter table.
#' @param save_dir location to save the parameter tbl
#' @param n_subj number of subjects
#' @param n_vars number of predictor variables
#' @param freq for binary predictor variables, frequencies
#' @param var_types (string) either one character giving the type for all
#' predictor variables, or a string of length n_vars.
#' "c" = continuous, "b" = binary. If NA, then parameters will be generated for
#' both binary and continuous settings.
#' @param n_imp Number of "important" predictors (with nonzero coefficients)
#' @param beta_imp Coefficients (hazard ratios) for important predictors
#' @param wb_shape Shape of Weibull distribution for event times
#' @param max_n_events (integer) Maximum number of events to simulate for any individual.
#' @param sim (Boolean) TRUE = simulated data, FALSE = case study data
#' @param nosave (Boolean) Don't save output, just return it (TRUE) or save it
#' as rds in the save_dir (FALSE, default)
#' @param round_event_times (Boolean) TRUE = round event times up to the nearest
#' integer, FALSE = keep exact event times
#' @export
#' @return parameter table (if `nosave` = TRUE) or simply the number of rows of
#' the table (if `nosave` = FALSE). The parameter table has the following columns:
#' \describe{
#'   \item{n.subj}{(integer) Number of participants}
#'   \item{n.events.max}{(integer) Number of outcome events simulated per participant.}
#'   \item{median.days.btwn.events}{(numeric) Target median inter-event time (days)}
#'   \item{wb.shape}{(numeric) Weibull distribution shape parameter for baseline hazard}
#'   \item{round_event_times}{(Boolean) whether to round up to next integer day (TRUE) or keep exact times (FALSE)}
#'   \item{freq}{(numeric) target frequency of dichotomous exposure}
#'   \item{n.vars}{(integer) number of predictor variables}
#'   \item{n.imp}{(integer) number of 'important' predictor variables with non-zero log-HR}
#'   \item{var.types}{(string of length `n.vars`, giving types of each predictor variable ('b' = binary/dichotomous, 'c' = continuous)}
#'   \item{beta}{(numeric vector of length `n.vars`) true log-HR values}
#'   \item{rho}{(numeric) autocorrelation coefficient (unused)}
#'   \item{p}{(integer) parameter table row ID}
#'  }

create_params <- function(save_dir,
                          n_subj,
                          n_vars,
                          n_imp = 0,
                          freq = NA,
                          var_types = NA,

                          beta_imp = NA,
                          wb_shape = 2,

                          max_n_events = 80,
                          sim = TRUE,
                          nosave = FALSE,
                          round_event_times = FALSE) {

  stopifnot(n_imp <= n_vars)
  if (all(!is.na(var_types))) {
    if (!(max(nchar(var_types)) == 1 |
              (max(nchar(var_types)) == n_vars & min(nchar(var_types)) == n_vars)))
    {
      stop("`var_types` length does not match `n_vars`")
    }

  }

  if (sim) {
    ## coefficients (log-HR) for important vars.
    beta = purrr::map(beta_imp, function(b) {
      c(rep(b, times = n_imp), rep(0, times = n_vars - n_imp))
    })

    ## variable types: binary and continuous
    if (all(is.na(var_types))) {
      ## If variable type is not specified, then try both binary and continuous
      ## applied to all of the n_vars vars
      var_types_list = purrr::map_chr(c("b", "c"), function(x) {
        paste0(rep(x, n_vars), collapse = "")
      })
    } else if (max(nchar(var_types)) == 1) {
      ## Only 1 type of variable specified: apply to all of the n_vars vars
      var_types_list <- purrr::map_chr(var_types, function(v) {
        paste(rep(v, times = n_vars), collapse = "")
      })
    } else {
      ## Keep prespecified types
      var_types_list <- var_types
    }

    var_types_tbl <- tibble::tibble(var_types = var_types_list,
                                    unique_types =
                                      purrr::map_chr(var_types,unique_char),
                                    conts_only = (.data$unique_types == "c"))
    ## autocorrelation
    rho = list(rep(0, n_vars))

    ### factors intended to vary
    df.vars <- var_types_tbl %>%
      split(.$conts_only) %>%
      purrr::map_dfr(function(vt) {

        if (all(vt$conts_only)) {
          freq_cur <- NA
        } else {
          freq_cur <- freq
        }

        tibble::as_tibble(
          expand.grid(
            freq = freq_cur,
            n.vars = n_vars,
            n.imp = n_imp,
            var.types = vt$var_types,
            beta = beta,
            rho = rho,
            stringsAsFactors = FALSE
          )
        )
      })

    ## Factors intended to remain constant
    params <- expand.grid(n.subj = n_subj,
                          n.events.max = max_n_events,
                          df.vars.row = 1:nrow(df.vars),
                          median.days.btwn.events = 7,
                          wb.shape = wb_shape,
                          round_event_times = round_event_times) %>%
      dplyr::group_by_all() %>%
      dplyr::do(df.vars[.data$df.vars.row,])  %>%
      dplyr::ungroup() %>%
      dplyr::mutate(p =  dplyr::row_number()) %>%
      dplyr::select(-.data$df.vars.row)

  } else if (!sim) {
    ### Case study setup

    params <- tibble::tibble(
      n.subj = n_subj,
      n.events.max = max_n_events,
      freq = NA,
      n.vars = n_vars,
      n.imp = n_imp,
      var.types = var_types ,
      beta = NA,
      p = 1,
      rho = NA,
      wb.shape = NA
    )

  }

  if (!nosave) {
    saveRDS(params, file.path(save_dir,  "params.rds"))
    return(nrow(params))
  } else if (nosave) {
    return(params)
  }
}

unique_char <- function(x) paste(unique(strsplit(x, "")[[1]]), collapse = "")







#' Generate time-series of predictor variable
#' @param var.type (string) "b" for binary or "c" for continuous
#' @param r desired autocorrelation
#' @param f desired frequency (binary variables only)
#' @param t time vector (used for length only)
#' @keywords internal
generate_predictor <- function(var.type, r, f, t) {

  if (var.type == "b") {
    ### Block Randomization to more evenly distribute the exposures.
    t_subset <- ceiling(c(1:length(t))/20)
    if (length(t) %% 20 != 0) {
      t_subset[t_subset == ceiling(length(t)/20)] <- ceiling(length(t)/20)-1
    }

    x <- tibble::tibble(t = t_subset) %>% split(.$t) %>%
      purrr::map(function(dt) {

        t_cur <- dt %>% dplyr::pull(t)

        n_ones <- ifelse(runif(1) < 0.5,
                         ceiling(f*length(t_cur)),
                         floor(f*length(t_cur)))

        x = c(rep(1, n_ones),
              rep(0, length(t_cur) - n_ones))

        sample(size = length(t_cur), x)
      }) %>%
      purrr::reduce(`c`)
    return(x)

  } else if (var.type == "c") {
    ## Set SD = 0.5 to match the SD of binomial
    rnorm(n = length(t), mean = 0, sd = 0.5)

  }
}

#' Generate time-series of predictor variable(s): wrapper around `generate_predictor`
#' @keywords internal
#' @return list with columns for predictor variables and exponential rates (`rate`)

generate_pred_wrapper <- function(
  n_events,
  time_units,
  var.types,
  rho,
  freq,
  beta

) {

  purrr::map(c(1:n_events), function(i, t, var.types, rho, freq, beta) {

    var.types.list <- unlist(strsplit(var.types, split = ""))
    names(var.types.list) <- paste0("X", stringr::str_pad(
      c(1:length(var.types.list)), pad = "0", width = 3))

    x0 <- as.matrix(purrr::pmap_dfc(list(var.type = var.types.list,
                                         r = rho,
                                         f = freq),
                                    .f = generate_predictor,
                                    t = t))
    x <- x0 %>% tibble::as_tibble() %>%
      dplyr::mutate(rate = exp(x0 %*% as.matrix(beta)))

    x

  }, t = time_units, var.types, rho, freq, beta)

}




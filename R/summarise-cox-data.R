#' Summarise Cox data for n-of-1 power analysis
#' @keywords internal
summarise_cox_data <- function(data,
                               x_vars = NULL,
                               sim = TRUE) {

  stopifnot(sim == TRUE | !is.null(x_vars))

  ## general results
  ## Note for the real data, n_days is missing all the dropped days...
  r1 <- data %>% dplyr::summarise(
    id = dplyr::first(.data$id),
    n_days = dplyr::n()
  )

  ## realized distribution of x
  r2 <- data %>% dplyr::summarise_at(x_vars,
                                     dplyr::funs("mean" = mean,
                                          "sd" = sd),
                                     na.rm = TRUE)
  ## survival time distribution
  r3 <- data %>% dplyr::filter(mig.ind == 1) %>%
    dplyr::summarise(
      t1_list = list(round(.data$t1, 1)),
      t1_mean = mean(.data$t1),
      t1_med = median(.data$t1),
      t1_min = min(.data$t1),
      t1_max = max(.data$t1)
    )
  dplyr::bind_cols(r1, r2, r3)

}

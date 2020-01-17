#' Summarize results of model estimation.
#' @export
#' @param results_dir directory of saved RDS results from estimation step
#' @param mv (boolean) TRUE = multivariable simulation, FALSE = univariable
#' @param sim (boolean) TRUE = simulated data, FALSE = case study
#' @param rowwise (boolean) TRUE = just return the rowwise summarized data
#' @param row_errors (boolean) TRUE = just return the error classifications for each row
summarise_results_wrapper <- function(results_dir,
                                      mv, sim,
                                      rowwise = FALSE,
                                      row_errors = FALSE) {

  params <- readRDS(file.path(results_dir, "params.rds"))

  furrr::future_map_dfr(params$p, function(p, params_df) {
    result <- readRDS(file.path(results_dir, paste0("result", p, ".rds")))
    res.df <- result %>%
      dplyr::mutate(p = as.integer(p)) %>%
      dplyr::left_join(params %>% dplyr::rename(beta_all = beta), by = c("p"))

    #### Sim, no mv
    if (sim & !mv) {

      res.df <- res.df %>%
        dplyr::mutate(beta.true = .data$beta_all[[1]][1],
                      beta.1.true = .data$beta.true)  %>%
        dplyr::filter(.data$var == "X001") %>%
        dplyr::group_by_at(c("p", "id", "n.events")) %>%
        dplyr::mutate(concordant_orig = as.numeric(
              max(ifelse(is.infinite(.data$ub), .data$concordant, NA),
              na.rm = TRUE))) %>%
        dplyr::ungroup()

    ##### Sim, mv
    } else if (sim) {

      res.df <- res.df %>%
        dplyr::mutate(
          beta.1.true = purrr::map_dbl(.data$beta_all,
                                       function(b) { b[[1]][1] } ),
          var.i = as.integer(substr(.data$var, 2, 4)),
          beta.true = purrr::map2_dbl(.data$var.i, .data$beta_all,
                                     function(v, b) { b[v] } ))
    ###### Case Study
    } else {

      res.df <- res.df %>%
        dplyr::mutate(
          beta.1.true = NA,
          beta.true = NA )
    }

    res.df %>%
      summarise_results(multivariable = mv,
                        sim = sim,
                        rowwise = rowwise,
                        row_errors = row_errors)

  },
  params_df = params,
  .progress = TRUE)
}

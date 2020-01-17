#' Plot figures for supplement
#' @export
#' @param run_dir output directory for simulation & case study results

figures_supp <- function(run_dir) {

  fig_dir <- file.path(run_dir, "figures")
  results_dir <- file.path(run_dir, "sim=TRUE&mv=FALSE")

  res <- readRDS(file.path(results_dir, "result-summary.rds"))

  ### Estimation ERRORS ####
  cat("Summarizing estimation errors\n")
  errors_all <- res %>%
    dplyr::select(.data$p, .data$ub, .data$var.types, .data$n.events,
                  .data$beta.true, starts_with("n_issue")) %>%
    dplyr::filter(.data$n_issue_none < 1000)
  readr::write_csv(errors_all, file.path(fig_dir, "estimation-problems-all.csv"))

  errors <- summarise_errors(results_dir, mv = FALSE, sim = TRUE)
  readr::write_csv(errors, file.path(fig_dir, "estimation-problems.csv"))

  # RELATIVE BIAS ##
  cat("Plotting bias\n")
  jpeg(filename = file.path(fig_dir, "fig-rel-bias.jpg"),
      width = 4500, height = 6400, units = "px", res = 800)
  plot(plot_univariate(res %>% dplyr::filter(.data$beta.true > 0),
                                 y = "rel.bias", 1))
  dev.off()
  readr::write_csv(smry_univariate(res %>% dplyr::filter(.data$beta.true > 0),
                                            "rel.bias"),
                   file.path( file.path(fig_dir, "fig-rel-bias.csv")))

  #### STANDARDIZED BIAS ##
  jpeg(filename = file.path(fig_dir, "fig-std-bias.jpg"),
      width = 4500, height = 6400, units = "px", res = 800)
  plot(plot_univariate(res %>% dplyr::filter(.data$beta.true > 0),
                                 y = "std.bias", 1))
  dev.off()
  readr::write_csv(smry_univariate(res %>% dplyr::filter(.data$beta.true > 0),
                                   "std.bias"),
                   file.path( file.path(fig_dir, "fig-std-bias.csv")))

  suffix= ""

  #### COVERAGE ####
  cat("Summarizing coverage\n")
  readr::write_csv(smry_univariate(res, "ct_covers_beta_95") %>%
                     dplyr::select(ub, beta.true, dplyr::everything()) %>%
                     dplyr::arrange(ub),
                   file.path( file.path(fig_dir, "fig-coverage.csv")))

  #### MARGIN OF ERROR ###
  cat("Plotting full margin of error\n")
  jpeg(filename = file.path(fig_dir, "fig-me_half_ci_50.jpg"),
      width = 4500, height = 6400, units = "px", res = 800)
  plot_univariate(res, y = "me_half_ci_50", 4) +
    theme(strip.placement  = 'inside')
  dev.off()

  readr::write_csv(smry_univariate(res, "me_half_ci_50"),
                   file.path(file.path(fig_dir,
                                       paste0("fig-me_half_ci_50", suffix, ".csv"))))

  ### Classifications ####
  cat("Plotting classifications\n")
  filename <- "fig-univ-bin-effect-class.jpg"
  jpeg(filename = file.path(fig_dir, paste0(filename)),
       width = 9, height = 6, units = "in", res = 1000)
  plot_effect_class(res, univ = TRUE, vartype = "b")
  dev.off()

  univ_bin_class_smry <-
    summarise_univ_effect_class(res, "b")
  readr::write_csv(univ_bin_class_smry,
                   file.path(fig_dir, "univ-bin-effect-class.csv"))
  univ_cont_class_smry <-
    summarise_univ_effect_class(res, "c")
  readr::write_csv(univ_cont_class_smry,
                   file.path(fig_dir, "univ-cont-effect-class.csv"))

}

#' Plot figures for univariable results
#' @export
#' @param run_dir output directory for simulation & case study results

figures_univ <- function(run_dir) {

  fig_dir <- file.path(run_dir, "figures")
  results_dir <- file.path(run_dir, "sim=TRUE&mv=FALSE")
  res <- readRDS(file.path(results_dir, "result-summary.rds"))

  #### POWER ###
  cat("Plotting power\n")
  jpeg(filename = file.path(fig_dir, "fig-power.jpg"),
       width = 5.6, height = 6.4, units = "in", res = 1000)
  plot(plot_power(res %>% dplyr::filter(.data$beta.true > 0)))
  dev.off()
  readr::write_csv(smry_univariate(res %>% dplyr::mutate(power = .data$ct_sig_95/1000),
                                            "power"),
                   file.path( file.path(fig_dir, paste0("fig-power.csv"))))
  #### MARGIN OF ERROR SUMMARY ####

  cat("Plotting margin of error summary\n")
  jpeg(filename = file.path(fig_dir, "fig-me_half_ci_50-smry.jpg"),
       width = 6, height = 4, units = "in", res = 1000)
  plot_univariate(res %>% dplyr::filter(p %in% c(3,8)),
                            y = "me_half_ci_50", max_y = 3.5) +
    theme(strip.placement  = 'inside')
  dev.off()

  readr::write_csv(smry_univariate(res  %>%
                                     dplyr::filter(.data$p %in% c(3, 8)),
                                   "me_half_ci_50"),
                   file.path( file.path(fig_dir,
                                        paste0("fig-me_half_ci_50-smry.csv"))))


  ### CLASSIFICATIONS ####
  cat("Plotting inferential classifications\n")
  filename <- "fig-univ-cont-effect-class.jpg"
  jpeg(filename = file.path(fig_dir, paste0(filename)),
       width = 8, height = 5, units = "in", res = 1000)
  plot_effect_class(res, univ = TRUE, vartype = "c")
  dev.off()


  cat(paste("Plots and csv files saved @ ", fig_dir))
}

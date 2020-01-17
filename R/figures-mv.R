#' Plot figures for multivariable simulation results
#' @export
#' @param run_dir output directory for simulation & case study results

figures_mv <- function(run_dir) {

  res <- readRDS(file.path(run_dir, "sim=TRUE&mv=TRUE", "result-summary.rds"))
  fig_dir <- file.path(run_dir, "figures")
  filename <- "fig-multi-effect-class.jpg"
  if (!file.exists(filename)) {
    jpeg(filename = file.path(fig_dir, paste0(filename)),
         width = 6.4, height = 5.2, units = "in", res = 1000)
    plot_effect_class_multivar(res)
    dev.off()
  }

  multi_class_smry <-
    summarise_multi_effect_class(res)
  readr::write_csv(multi_class_smry, file.path(run_dir,
                                               "figures",
                                               "multi-effect-class.csv"))
}

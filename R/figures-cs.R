#' Plot figures for case study
#' @export
#' @param run_dir output directory for simulation & case study results
#' @importFrom grDevices dev.off jpeg png
#' @importFrom graphics plot
figures_cs <- function(run_dir) {

  sim = FALSE
  result_cs <- readRDS(file.path(run_dir, "sim=FALSE&mv=FALSE",
                                 "result1.rds"))
  jpeg(filename = file.path(run_dir, "figures", "fig-cs-est.jpg"),
       width = 7, height = 6, units = "in", res = 1000)
  plot(plot_cs_est(result_cs %>% rowwise_summary(),
                             vars = case_study_vars()))
  dev.off()
}

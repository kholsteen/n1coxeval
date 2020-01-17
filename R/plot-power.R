#' Plot Simulated Power
#' @param data (tbl) summarised results, output from `summarise_results_wrapper`
#' @keywords internal

plot_power <- function(data) {

  by_alpha = FALSE
  which_ub = NA

  test_null = FALSE
  if (max(data$beta.true == 0)) {
    test_null = TRUE
  }

  if (by_alpha & is.na(which_ub)) {
    which_ub = 648
  }

  dp <- data %>%
    tidyr::gather(key = "key",
                  value = "value",
                  starts_with("ct_sig_9"), starts_with("ct_sig_8"))
  ylab = "Power"

  dp <- dp %>%
    tidyr::separate(key, into = c("CI_var", "CI_Width"), sep =  -2) %>%
    dplyr::mutate(CI_Width = as.numeric(.data$CI_Width),
                  alpha = (100 - .data$CI_Width)/100,
                  power = .data$value/.data$n_reps)

  if (by_alpha) {
    gp <- ggplot(dp %>% dplyr::filter(.data$ub == which_ub),
                 aes(n.events, power, col = as.factor(alpha),
                     shape = as.factor(alpha),
                     linetype = as.factor(alpha))) +
      geom_line(aes(group = alpha)) +
      facet_grid(ub + beta.true ~ var.types + freq, labeller =  lbl,
                 switch = "y") +
      scale_color_viridis_d(end = 0.7,name = "Alpha") +
      scale_linetype_discrete(name = "Alpha") +
      scale_shape_discrete(name = "Alpha")

    varying = "Alpha"

  } else {
    gp <- ggplot(dp %>% dplyr::filter(.data$alpha == 0.05),
                 aes(as.factor(n.events), power, col = as.factor(ub),
                     shape = as.factor(ub),
                     linetype = as.factor(ub)))  +
      geom_line(aes(group = ub)) +
      facet_grid(beta.true ~ var.types + freq, labeller =  lbl) +
      plot_common_elements() +
      prior_ub_labels()
    varying = "Priors"

  }

  if (!test_null) {

    gp <- gp +
      geom_hline(aes(yintercept = 0.5), col = "red", alpha = 0.25) +
      geom_hline(aes(yintercept = 0.8), col = "orange", alpha = 0.25)
  }

  gp <- gp +
    geom_point()

  if (test_null) {
    gp <- gp + scale_y_continuous(labels = scales::percent,
                                  breaks = c(0, 0.05, 0.1, 0.15, 0.2),
                                  minor_breaks = NULL)

  } else {
    gp <- gp + scale_y_continuous(labels = scales::percent,
                                  breaks = c(0, 0.25, 0.5, 0.75, 1),
                                  minor_breaks = NULL)
  }

  gp <- gp +
    ylab(ylab) +
    xlab("Event Count") +
    theme_light() +
    theme(legend.position = "top",
        strip.text = element_text(color = "black"),
        strip.placement = "outside")

  plot(gp)

}

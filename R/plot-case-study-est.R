#' Plot case-study log-hazard ratio estimates
#' @export
#' @param data Raw result from cox regression on the case study data (just read in result1.rds)
#' @description Facet plot with 3 variable-rows and 4 participant-columns.
#' In each facet, plots the log-hazard ratio estimate and 90% confidence interval for
#' each of the priors and event counts.
#' @keywords internal
plot_cs_est <- function(data, vars,
                        mle_class = FALSE,
                        ub_display = NA) {

  loghr_lims = c(-4, 3)

  class_labels <-  c("Sig+", "Non-", "0",
                     "Non+", "Sig-", "?")

  alpha_values <- rep(c(0.9, 0.5), 3)
  names(alpha_values) <- class_labels

  fill_values <- viridisLite::viridis(length(class_labels))
  names(fill_values) <- class_labels

  ### These are cut off at 4 to make the graph easier to reac
  dp <- data %>%
    dplyr::mutate_at(.vars = c("beta", "lb95", "ub95"),
                                  function(x) { dplyr::case_when(
                                    x > loghr_lims[2] ~ Inf,
                                    x < loghr_lims[1] ~ -Inf,
                                    TRUE ~ as.numeric(x)) } ) %>%
    dplyr::mutate(ub = dplyr::case_when(
      is.na(ub) ~ 900,
      is.infinite(ub) ~ 1000,
      TRUE ~ ub
    ),

    effect_class = factor(.data$effect_class,
                          levels = c(1:6),
                          labels = class_labels)


    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(# order to make graph easier to read
                n.events_plot = .data$n.events + switch(as.character(.data$ub),
                                                  "900" = -0.8,
                                                  "16" = -0.4,
                                                  "40" = 0,
                                                  "648" = 0.4,
                                                  "1000" = 0.8)) %>%
    dplyr::filter(.data$var %in% vars)

  if (mle_class) {

    dp <- dp %>%
      dplyr::filter(.data$ub == 1000)

  }

  panels <- dp %>%
    dplyr::select(.data$id, .data$var) %>%
    dplyr::group_by_all() %>%
    dplyr::filter(dplyr::row_number() == 1) %>%
    dplyr::arrange(.data$var, .data$id) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(label = purrr::map_chr(dplyr::row_number(), ~LETTERS[.]))


    gp <- ggplot(dp) +
      geom_hline(aes(yintercept = 0))

      if (!mle_class) {

        gp <- gp +

          geom_point(aes(n.events_plot, beta, col = as.factor(ub),
                         shape = as.factor(ub)))  +
          geom_line(aes(n.events_plot, beta, col = as.factor(ub),
                    linetype = as.factor(ub),
                    group = as.factor(ub))) +
          geom_errorbar(aes(x = n.events_plot, ymin = lb95,
                            ymax = ub95, col = as.factor(ub)), width = 0.3) +
          prior_ub_labels() +
          geom_label(
            data = dp %>% dplyr::filter(ub == 1000),
            aes(x = n.events_plot- 0.8,
                y = loghr_lims[1] + 0.5,
                label = effect_class,
                fill = as.factor(effect_class),
                alpha = as.factor(effect_class)
                ), label.padding = unit(0.1, "lines"),
            #vjust = 1, hjust = 0.75,
            size = 3) +
          scale_fill_manual(values = fill_values, guide = "none") +
          scale_alpha_manual(values = alpha_values, guide = "none")

      } else {

        gp <- gp + geom_point(aes(n.events_plot, beta,
                                  col = as.factor(effect_class),
                              shape = as.factor(effect_class)),
                              #alpha = as.factor(effect_class)),
                              size = 2)  +
          geom_line(aes(n.events_plot, beta), col = "gray75") +
          geom_errorbar(aes(x = n.events_plot, ymin = lb95,
                            ymax = ub95,
                            col = as.factor(effect_class),
                            #alpha = as.factor(effect_class)
                            ),
                        width = 0.6) +
          # scale_alpha_manual(values = c("1" = 1,
          #                               "2" = 0.7,
          #                               "3" = 1,
          #                               "4" = 0.7,
          #                               "5" = 1,
          #                               "6" = 0.7)) +
          scale_color_viridis_d()


      }

    gp <- gp +
      geom_text(data = panels, aes(x = 20.5, y = 2.7, label = label)) +
      facet_grid(var~id, labeller = lbl, switch = "y") +
      scale_x_continuous(breaks = c(5, 10, 15, 20), minor_breaks = NULL) +
      expand_limits(x = c(3,22)) +
      xlab("Number of Events Included") +
      #labs(subtitle = subtitle) +



      scale_y_continuous(limits = loghr_lims,
                         breaks = c(-2,0,2),
                         sec.axis = sec_axis(trans = exp,
                                             breaks = exp(c(-2, -1, 0, 1, 2)),
                                             labels = round(exp(c(-2, -1, 0, 1, 2)), 2),
                                             name = "Hazard Ratio Scale")) +
#
#       scale_y_log10(limits = hr_lims, breaks = c(0.1, 0.3, 1, 3, 10),
#                   minor_breaks = NULL) +
      ylab("log-Hazard Ratio and 95% CI") +
     # ggtitle("Case Study Hazard Ratio Estimates") +
      theme_light() + theme(legend.position = "top",
                            strip.text = element_text(color = "black"),
                            strip.placement = "outside")
                            #strip.text.y = element_text(angle = 0))

   gp

}

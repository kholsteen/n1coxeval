#' Plot effect classifications for multivariable setting
#' @keywords internal
#' @param res Results from `summarise_results_wrapper(rowwise = FALSE)`
#' @import ggplot2

plot_effect_class_multivar <- function(res) {

  levels = c(
    "sig_pos",
    "non_neg",
    "null",
    "non_pos",
    "sig_neg",
    "unknown"
  )

  labels = c("Sig. Pos.", "Non Neg.", "Null",
             "Non Pos.", "Sig. Neg", "Unknown")

  alphas <- c(1, 0.5, 1, 0.5, 1, 0.5)



    resclass <- res %>%

      dplyr::mutate(beta_zero = as.numeric(.data$beta.true == 0)) %>%
      dplyr::group_by_at(
      c("p", "n.events", "ub", "beta.1.true", "n_vars", "beta_zero")) %>%

      dplyr::select(dplyr::group_cols(), starts_with("n_effect")) %>%
      tidyr::gather(key = "key",
                    value = "value", starts_with("n_effect")) %>%
      dplyr::mutate(
        key = stringr::str_replace_all(.data$key, "n_effect_", ""),
        key = stringr::str_replace_all(.data$key, "_all", ""),
        key = factor(.data$key, levels = levels,
                     labels = labels)
      )

  g1 <- ggplot(resclass %>% dplyr::filter(beta_zero == 0)) +
    geom_col(
      aes(x = as.factor(n.events), y = value,
          fill = key, color = key)
    ) +
    facet_wrap(beta.1.true ~ ., ncol = 1, #beta_zero ~ beta.1.true , #~ var.types + ub,
               labeller = ggplot2::labeller(
                 beta.1.true = function(x) {
                   paste0("Setting ",
                          purrr::map_chr(x, ~as.character(round(min(exp(as.numeric(.))-1, 4), 0))),
                          ": True Positive Effect (HR = ", round(exp(as.numeric(x))), ")")
               }  )) +
    scale_fill_manual("Classification",
                      values = gray_colors()) +
    scale_color_manual("Classification", values = gray_lines()) +
    scale_y_continuous(
      breaks = NULL, minor_breaks = NULL,
      sec.axis = sec_axis(
      trans = ~.,
      name = "Average Count Across Simulated Results")) +
    xlab("Event Count") +
    ylab(element_blank()) +
    guides(fill = guide_legend(nrow = 1),
           color = guide_legend(nrow = 1)) +
    #ggtitle("Classification of Univariable Effects") +
    #labs(subtitle = "With 25 candidate variables (4 True Positives and 21 True Negatives)") +
    theme_light() +
    theme(legend.position = "top",
          strip.text = element_text(color = "black"),
          strip.placement = "outside",
          axis.title.y.right = element_text(margin = margin(10,10,10,10)),
          panel.spacing = unit(5, "pt"),
          plot.margin = margin(10,0,10,10))

  resclass_null <- resclass %>% dplyr::filter(beta_zero == 1) %>%
    dplyr::group_by(n.events, key, beta_zero) %>%
    dplyr::summarise(mean = mean(value))

  g2 <- ggplot(resclass_null) +
    geom_col(
      aes(x = as.factor(n.events), y = mean,
          fill = key, color = key)
    ) +
    facet_wrap(beta_zero ~ ., ncol = 1, #beta_zero ~ beta.1.true , #~ var.types + ub,
               labeller = function(x) {
                 "True Null Effect\n(Counts Averaged over 4 Settings)"}
               #space = "free_y", scales= "free_y") +
    ) +

    scale_fill_manual("Classification",
                      values = gray_colors()) +
    scale_color_manual("Classification", values = gray_lines()) +
    scale_y_continuous(breaks = c(0:21),
                       minor_breaks = NULL,
                       expand = c(0,0.1)) +
    xlab("Event Count") +
    ylab("") + #"Average Count Across Simulated Results") +
    #ggtitle("Classification of Univariable Effects") +
    #labs(subtitle = "With 25 candidate variables (4 True Positives and 21 True Negatives)") +
    theme_light() +
    theme(legend.position = "none",
          strip.text = element_text(color = "black"),
          strip.placement = "outside",
          plot.margin = margin(10,20,10,0))

  legend  <- gtable::gtable_filter(ggplotGrob(g1), "guide-box")

  gridExtra::grid.arrange(g1 + theme(legend.position = "none"),
                          g2,
                          ncol = 2,
                          top = legend)

}


























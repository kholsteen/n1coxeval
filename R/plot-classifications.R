#' Plot effect classifications
#' @keywords internal
#' @param res Results from `summarise_results_wrapper(rowwise = FALSE)`
#' @param univ (Boolean) Univariate (TRUE) or multivariable (FALSE) results?
#' @param vartype (Character) For `univ = TRUE` only, predictor vartype = "c"
#' (Continuous) or "b" (Binary)

plot_effect_class <- function(res, univ = TRUE, vartype = "c") {

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

  ############# UNIVARIABLE #########################################
  if (univ) {

    resclass <- res %>% dplyr::group_by_at(
      c("p", "n.events", "ub", "beta.true", "var.types")) %>%
      dplyr::select(dplyr::group_cols(), starts_with("n_effect")) %>%
      tidyr::gather(key, value, starts_with("n_effect")) %>%
      dplyr::mutate(
        key = stringr::str_replace_all(key, "n_effect_", ""),
        key = factor(key, levels = levels, labels = labels
        )
      )

    # for (v in c("c", "b")) {
      v = vartype
      title = ifelse(v == "c", "Continuous Predictor", "Dichotomous Predictor")

      gp <- ggplot(resclass %>% dplyr::filter(.data$var.types == v)) +
        geom_col(
          aes(x = as.factor(n.events), y = value,
              fill = key, alpha = key)
        ) +
        facet_grid(beta.true ~ ub,
                   labeller = lbl) +
        scale_fill_viridis_d("Classification") +
        scale_alpha_manual("Classification",
                           values = alphas) +
        #ggtitle(paste0("Classification of Effects: ", title)) +

        xlab("Event Count") +
        ylab("Simulated Results Count") +

        theme_light() +
        theme(legend.position = "left",
              strip.text = element_text(color = "black"),
              strip.placement = "outside",
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank())

      plot(gp)
    #}

  ############# MULTIVARIABLE #########################################
  } else if (!univ) {

    resclass <- res %>%

      dplyr::mutate(beta_zero = as.numeric(beta.true == 0)) %>%
      dplyr::group_by_at(
      c("p", "n.events", "ub", "beta.1.true", "n_vars", "beta_zero")) %>%

      dplyr::select(dplyr::group_cols(), starts_with("n_effect")) %>%
      tidyr::gather(key = "key",
                    value = "value",
                    starts_with("n_effect")) %>%
      dplyr::mutate(
        key = stringr::str_replace_all(.data$key, "n_effect_", ""),
        key = stringr::str_replace_all(.data$key, "_all", ""),
        key = factor(.data$key, levels = levels,
                     labels = labels)
      )

    ##### SIZES PROPORTIONAL TO N VARS ######

      gp <- ggplot(resclass) +
        geom_col(
          aes(x = as.factor(n.events), y = value,
              fill = key, alpha = key)
        ) +
        facet_grid(beta_zero ~ beta.1.true , #~ var.types + ub,
                   labeller = lbl,
                   space = "free_y", scales = "free_y") +

        scale_fill_viridis_d("Classification") +
        scale_alpha_manual("Classification", values = alphas) +
        xlab("Event Count") +
        ylab("Average Count Across Simulated Results") +
        #ggtitle("Classification of Univariable Effects") +
        #labs(subtitle = "With 25 candidate variables (4 True Positives and 21 True Negatives)") +
        theme_light() +
        theme(legend.position = "left",
              strip.text = element_text(color = "black"),
              strip.placement = "outside")

      plot(gp)
  }
}




























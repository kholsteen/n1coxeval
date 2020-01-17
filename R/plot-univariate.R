#' Plot any result
#' @keywords internal
#' @param data (tbl) summarised results, output from `summarise_results_wrapper`
#' @param y (string) name of variable to plot
#' @param max_y (numeric) maximum value to plot on y-axis
#' @param hline_intercept (numeric) y-value at which to draw reference line
plot_univariate <- function(data,
                            y = "me_90",
                            max_y = 10,
                            hline_intercept = 0) {

    #### SET UP PARAMETERS ####
    m = max_y
    title = paste0("Simulated Multiplicative Error vs. Sample Size")
    if (y == "me_975") { pct = 97.5} else if (y == "me_95") { pct = 95
    } else if (y == "me_90") {pct = 90
    } else if (y == "me_rank1_median") {
      pct = 50
      title = "Multiplicative Error for 1st-Ranked HR vs. Sample Size"
      data <- data %>% dplyr::filter(var == "X001")
    } else if (y == "rel.bias") {
      title = "Relative Bias of the Mean Estimated log-HR"
      ylab = "Relative Bias"
      limits = c(NA, m)
      yscale <- scale_y_continuous(limits = limits)

    } else if (grepl(pattern = "std.bias", x = y))  {
      title = "Standardized Bias of the Mean Estimated log-HR"
      ylab = "Standardized Bias"
      limits = c(NA, m)
      yscale <- scale_y_continuous(limits = limits)
    } else if (y == "me_half_ci_50") {
      title = "Median Margin of Error (Half-Width of 95% Confidence Interval)"
      ylab = "Margin of Error\n(log-HR, Additive Scale)"
      limits = c(0, m + 0.25)
      axis2 = sec_axis(trans = exp,
                       name = "Margin of Error\n(HR, Multiplicative Scale)",
                       breaks =
                         round(exp(c(0,1, seq(from = 2, to = m + 1, by = 1))), 1))

      yscale <- scale_y_continuous(limits = limits,
                                   breaks =
                                     c(0,1, seq(from = 2, to = m + 1, by = 1)),
                                   sec.axis = axis2)
    } else {
      title = y
      ylab = y
      limits = c(NA, NA)
      yscale = scale_y_continuous()
    }

    if (!is.infinite(m)) {
      dp <- data %>% dplyr::mutate(me_toplot =
                                     dplyr::if_else(!!rlang::sym(y) > m,
                                                    as.numeric(Inf),
                                                    as.numeric(!!rlang::sym(y))))
    } else {
      dp <- data %>% dplyr::mutate(me_toplot = !!rlang::sym(y))
    }

    #### CREATE PLOT ####
    gp <- ggplot(dp,
                 aes_string("as.factor(n.events)",
                            "me_toplot",
                            col = "as.factor(ub)",
                            linetype = "as.factor(ub)",
                            shape = "as.factor(ub)")) +
      geom_point() +
      geom_hline(aes(yintercept = hline_intercept), col = "red") +
      plot_common_elements() +
      prior_ub_labels()

    gp <- gp +
      yscale +
      ylab(ylab)

    if (!is.infinite(m)) {
    } else {
      gp <- gp +  scale_y_continuous(trans = "log10")
    }

    gp <- gp + geom_line(aes(group = as.factor(ub)))

    plot(gp)
}





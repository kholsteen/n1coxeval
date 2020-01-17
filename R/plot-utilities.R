#' Translate var names to readable names
#' @keywords internal
case_study_nice_names <- function(varname) {
  dplyr::case_when(
    varname == "caff_count_dif_lag1" ~ "Change in Caffeine",
    varname == "stress_global_lag1" ~ "Stress Level",
    varname == "sleep_hours_lag1" ~ "Sleep Hours",
    TRUE ~ varname
  )
}

#' Labeller function for facet plots
#' @keywords internal
lbl <- ggplot2::labeller(beta.true = function(x) { #print(x);
  paste0("HR = ", round(exp(as.numeric(x))))},

  beta_zero = function(x) {
    ifelse(x == 0, "True Positive", "True Negative (HR = 1)")
  },

  beta.1.true = function(x) { #print(x);
    paste0("HR = ", round(exp(as.numeric(x))))},
  var.types = function(x) {
    ifelse(substr(x, 1, 1) == "b", "Dichotomous Var", "Continuous Var") },
  freq = function(x) { #if (is.na(x)) { "SD = 0.5"} else {
    ifelse(is.na(x), "SD = 0.5", paste0("Frequency = ", x))
  },
  rho = function(x) {paste0("Autocor = ", rho)},
  ub = function(x) { dplyr::case_when(x == 900 ~ "Firth",
                                      x == 1000 ~ "MLE",
                            is.infinite(x) ~ "MLE",
                            TRUE ~ paste0("Prior 95% UB = ", x))},
  n.events = function(x) { paste0(x, " Events")},
  id = function(x) { paste0("ID ", x)},
  beta_null = function(x) {
    dplyr::case_when(x == 1.25~ "-1 to 1.25 (by 0.25)",
                     x == 2.5 ~ "-2 to 2.5 (by 0.5)")

    },
  var = case_study_nice_names
)

#' Common plot elements
#' @keywords internal
plot_common_elements <- function() {

  list(
    facet_grid(beta.true ~ var.types + freq, labeller =  lbl),
    #scale_x_continuous(trans = "log2", breaks = c(5,10, 20, 40, 80)),

    xlab("Event Count"),
    theme_light(),
    theme(legend.position = "top",
          strip.text = element_text(color = "black"),
          strip.placement = "outside"))
}

#' Labeller function for estimation methods
#' @keywords internal
prior_ub_labels <- function() {
  list(
    scale_linetype_discrete(name = "Prior 95% UB for HR",
                            labels = c(16, 40, 648, "Firth", "None (MLE)")),
    scale_shape_discrete(name = "Prior 95% UB for HR",
                         labels = c(16, 40, 648, "Firth", "None (MLE)")),
    scale_color_viridis_d(end = 0.7,
                          labels = c(16, 40, 648, "Firth", "None (MLE)"),
                          name = "Prior 95% UB for HR"),
    theme_light(),
    theme(legend.position = "top",
          strip.text = element_text(color = "black"),
          strip.placement = "outside")
  )
}



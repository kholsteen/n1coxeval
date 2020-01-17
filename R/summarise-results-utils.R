#' Constant SE cutoff to identify monotone likelihood
#' @keywords internal
se_cutoff <- function() {
  200
}

#' Estimate power
#' @keywords internal
est_power <- function(mme, beta.true) {

  s = 1.96/(log(mme)/beta.true)
  print(log(mme))
  print(s)
  stats::pnorm(1.96 - s, lower.tail = FALSE) +
    stats::pnorm(-1.96 - s, lower.tail = TRUE)
}

#' Classify confidence limits with respect to threshold value and zero
#' @keywords internal
conf_limit_category <- function(x, threshold_null_or) {

  dplyr::case_when(
    is.na(x) ~ as.numeric(NA),
    x < log(1/threshold_null_or) ~ 1,
    x < 0 ~ 2,
    x < log(threshold_null_or) ~ 3,
    TRUE ~ 4)
}

#' Classify estimates based on confidence limits
#' @keywords internal
classify_effect <- function(lbcat, ubcat) {
  dplyr::case_when(
    is.na(lbcat) | is.na(ubcat) ~ 6,
    lbcat == 1 & ubcat %in% c(1,2) ~ 5,
    lbcat == 1 & ubcat == 3 ~ 4,
    lbcat == 1 & ubcat == 4 ~ 6,
    lbcat == 2 & ubcat %in% c(2,3) ~ 3,
    lbcat == 2 & ubcat == 4 ~ 2,
    lbcat == 3 & ubcat == 3 ~ 3,
    lbcat %in% c(3,4) & ubcat == 4 ~ 1,
    TRUE ~ as.numeric(NA)
  )
}

#' Rowwise Summary
#' @export
#' @keywords internal
rowwise_summary <- function(data,
                            threshold_null_or = 2,
                            threshold_sig_or = 1) {

  if (!("beta.true") %in% colnames(data)) {
    data$beta.true <- as.numeric(NA)
  }

  data %>% dplyr::mutate(
    bias = .data$beta - .data$beta.true,
    ml = as.numeric(ifelse(!is.na(.data$se), .data$se >= se_cutoff(), FALSE)),
    stat.sig = !is.na(.data$lb95) & !is.na(.data$ub95) &
      as.numeric(.data$lb95 > 0 | .data$ub95 < 0),
    effect_null = as.numeric(
      !is.na(.data$lb95) & !is.na(.data$ub95) &
        .data$lb95 > log(1/threshold_null_or) &
        .data$ub95 < log(threshold_null_or)),

    lb95_cat = conf_limit_category(.data$lb95, threshold_null_or),
    ub95_cat = conf_limit_category(.data$ub95, threshold_null_or),
    effect_class = classify_effect(.data$lb95_cat, .data$ub95_cat),
    magnitude.error = ifelse(.data$stat.sig,
                             abs(.data$beta/.data$beta.true), NA),
    sign.error = ifelse(.data$stat.sig,
                        as.numeric(.data$beta < 0), NA),
    me_half_ci = .data$ub95 - .data$beta,
    mme = exp(.data$ub95 - .data$beta),
    me = exp(.data$beta)/exp(.data$beta.true),
    ub = dplyr::case_when(
      is.na(.data$ub) ~ 900,
      is.infinite(.data$ub) ~ 1000,
      TRUE ~ .data$ub
    ),
    beta.true = round(.data$beta.true,2),
    est_issue = dplyr::case_when(
      grepl("Loglik", .data$error) ~ 1,
      grepl("coxphf", .data$error) ~ 5,
      grepl("overflow", .data$error) ~ 3,
      .data$se > se_cutoff() ~ 2,
      is.na(.data$beta) ~ 4,
      TRUE ~ 0),
    ### All good... no apparent issues at all after reviewing results.
    ok = as.numeric(!is.na(.data$beta) & !is.na(.data$se) &
                      .data$se != 0 &  .data$error == "" &
                      .data$se < se_cutoff())
  )
}

#' Reps Summary
#' @keywords internal
reps_summary <- function(data_grouped) {

  data_grouped %>%
    dplyr::summarise(n_reps = max(.data$id),
                      mean_nrow = mean(.data$nrow),

                      n_issue_conv = sum(.data$est_issue == 1),
                      n_issue_se_g200 = sum(.data$est_issue == 2),
                      n_issue_overflow = sum(.data$est_issue == 3),
                      n_issue_beta_na = sum(.data$est_issue == 4),
                      n_issue_coxphf = sum(.data$est_issue == 5),
                      n_issue_none = sum(.data$est_issue == 0),

                      n_beta_na = sum(is.na(.data$beta)),
                      n_beta_0 = sum(!is.na(.data$beta) &
                                       .data$beta == 0),
                      n_errors = sum(.data$error != ""),
                      n_beta_na_no_error =
                        sum(is.na(.data$beta) & .data$error == ""),
                      n_se_0 = sum(!is.na(.data$se) &
                                     .data$se == 0),
                      n_se_nan = sum(is.na(.data$se)),
                      n_se_g200 = sum(.data$ml),
                      n_se_nan_or_g200 = sum(is.na(.data$se) |
                                               .data$ml == 1),

                      n_beta_0_se_g200 = sum(.data$beta == 0 &
                                               .data$ml == 1),
                      n_beta_na_se_0 = sum(is.na(.data$beta) &
                                             !is.na(.data$se) &
                                             .data$se == 0),
                      n_warning_ml =
                        sum(grepl("Loglik", .data$error)),
                      n_warning_num =
                        sum(grepl("overflow", .data$error)),
                      n_warning_par =
                        sum(grepl("parameter", .data$error)),
                      n_warning_prof =
                        sum(grepl("profile", .data$error)),

                      n_ok = sum(.data$ok),
                      mean_b = mean(.data$beta, na.rm = TRUE),
                      mean_b_ok = mean(
                        ifelse(.data$ok == 1, .data$beta, NA),
                        na.rm = TRUE),
                      rel.bias =  mean(.data$bias, na.rm = TRUE)/
                        dplyr::first(.data$beta.true),
                      sd.beta = sd(.data$beta, na.rm = TRUE),
                      std.bias = mean(.data$bias, na.rm = TRUE)/
                        sd(.data$beta, na.rm = TRUE),

                      rel.bias.ok = mean(
                        ifelse(.data$ok == 1, .data$bias, NA),
                        na.rm = TRUE)/dplyr::first(.data$beta.true),
                      sd.beta.ok = sd(
                        ifelse(.data$ok == 1, .data$beta, NA), na.rm = TRUE),
                      std.bias.ok = mean(
                        ifelse(.data$ok == 1, .data$bias, NA), na.rm = TRUE)/sd(
                          ifelse(.data$ok == 1, .data$beta, NA), na.rm = TRUE),

                      median_se = median(.data$se, na.rm = TRUE),

                      n_effect_unknown = sum(.data$effect_class == 6, na.rm = TRUE),
                      n_effect_sig_pos = sum(.data$effect_class == 1, na.rm = TRUE),
                      n_effect_non_neg = sum(.data$effect_class == 2, na.rm = TRUE),
                      n_effect_null = sum(.data$effect_class == 3, na.rm = TRUE),
                      n_effect_non_pos = sum(.data$effect_class == 4, na.rm = TRUE),
                      n_effect_sig_neg = sum(.data$effect_class == 5, na.rm = TRUE),

                      me_half_ci_50 = median(.data$me_half_ci, na.rm = TRUE),
                      me_half_ci_50_ok = median(
                        ifelse(.data$ok == 1, .data$me_half_ci, NA), na.rm = TRUE
                      ),

                      me_half_ci_90 = quantile(.data$me_half_ci, probs = 0.9, na.rm = TRUE),

                      me_10 = quantile(.data$me, probs = 0.1, na.rm = TRUE),
                      me_90 = quantile(.data$me, probs = 0.9, na.rm = TRUE),
                      me_95 = quantile(.data$me, probs = 0.95, na.rm = TRUE),
                      me_975 = quantile(.data$me, probs = 0.975, na.rm = TRUE),
                      me_median = median(.data$me, na.rm = TRUE),

                      mme_10 = quantile(.data$mme, probs = 0.1, na.rm = TRUE),
                      mme_90 = quantile(.data$mme, probs = 0.9, na.rm = TRUE),
                      mme_median = median(.data$mme, na.rm = TRUE),
                      empir_sd = sd(.data$beta, na.rm = TRUE),
                      root_mse = sqrt(.data$rel.bias^2 + .data$empir_sd^2),
                      ct_beta_na = sum(is.na(.data$beta)),

                      ## will give 1 - type I error for beta = 0
                      ct_covers_beta_95 = sum(.data$lb95 <= .data$beta.true &
                                                .data$ub95 >= .data$beta.true,
                                              na.rm = TRUE),
                      ct_covers_beta_95_ok = sum(
                        ifelse(ok == 1, .data$lb95 <= .data$beta.true & .data$ub95 >= .data$beta.true, NA),
                        na.rm = TRUE),
                      ct_covers_beta_90 = sum(.data$lb90 <= .data$beta.true &
                                                .data$ub90 >= .data$beta.true,
                                              na.rm = TRUE),
                      ct_covers_beta_85 = sum(.data$lb85 <= .data$beta.true &
                                                .data$ub85 >= .data$beta.true,
                                              na.rm = TRUE),
                      ct_covers_beta_80 = sum(.data$lb80 <= .data$beta.true &
                                                .data$ub80 >= .data$beta.true,
                                              na.rm = TRUE),

                      ## will give type II error for beta != 0
                      ct_covers_0 = sum(.data$lb95 <= 0 & .data$ub95 >= 0,
                                        na.rm = TRUE),

                      ct_sig_95 = sum(.data$stat.sig, na.rm = TRUE),

                      ct_sig_pos_95 = sum(.data$beta > 0 & .data$stat.sig, na.rm = TRUE),
                      ct_sig_pos_90 = sum(.data$lb90 > 0 & .data$beta > 0, na.rm = TRUE),
                      ct_sig_pos_85 = sum(.data$lb85 > 0 & .data$beta > 0, na.rm = TRUE),
                      ct_sig_pos_80 = sum(.data$lb80 > 0 & .data$beta > 0, na.rm = TRUE),

                      ct_sig_neg_95 = sum(.data$beta < 0 & .data$stat.sig, na.rm = TRUE),
                      ct_sig_neg_90 = sum(.data$ub90 < 0 & .data$beta < 0, na.rm = TRUE),
                      ct_sig_neg_85 = sum(.data$ub85 < 0 & .data$beta < 0, na.rm = TRUE),
                      ct_sig_neg_80 = sum(.data$ub80 < 0 & .data$beta < 0, na.rm = TRUE),

                      ct_sig_95 = .data$ct_sig_pos_95 + .data$ct_sig_neg_95,
                      ct_sig_90 = .data$ct_sig_pos_90 + .data$ct_sig_neg_90,
                      ct_sig_85 = .data$ct_sig_pos_85 + .data$ct_sig_neg_85,
                      ct_sig_80 = .data$ct_sig_pos_80 + .data$ct_sig_neg_80,

                      ct_positive = sum(.data$beta > 0 , na.rm = TRUE),
                      ct_negative = sum(.data$beta < 0, na.rm = TRUE),
                      mag_error_mean = mean(.data$magnitude.error, na.rm = TRUE),
                      sign_error_mean = mean(.data$sign.error, na.rm = TRUE),

                      min_sig_beta = min(ifelse(!is.na(.data$beta) & !is.na(.data$stat.sig) &
                                                  .data$beta > 0 & .data$stat.sig, .data$beta, Inf)))
}

#' Summary of results for multivariable setting
#' @keywords internal
multivariable_summary <- function(data_grouped) {
  data_grouped %>%
    dplyr::summarise(

      n_vars = dplyr::n(),
      mean_effect_unknown = mean(.data$n_effect_unknown/.data$n_reps),
      mean_effect_sig_pos = mean(.data$n_effect_sig_pos/.data$n_reps),
      mean_effect_non_neg = mean(.data$n_effect_non_neg/.data$n_reps),
      mean_effect_null = mean(.data$n_effect_null/.data$n_reps),
      mean_effect_non_pos = mean(.data$n_effect_non_pos/.data$n_reps),
      mean_effect_sig_neg = mean(.data$n_effect_sig_neg/.data$n_reps),

      n_effect_unknown_all = .data$mean_effect_unknown*.data$n_vars,
      n_effect_sig_pos_all = .data$mean_effect_sig_pos*.data$n_vars,
      n_effect_non_neg = .data$mean_effect_non_neg*.data$n_vars,
      n_effect_null_all = .data$mean_effect_null*.data$n_vars,
      n_effect_non_pos_all = .data$mean_effect_non_pos*.data$n_vars,
      n_effect_sig_neg_all = .data$mean_effect_sig_neg*.data$n_vars
    ) %>%
    dplyr::ungroup()
}

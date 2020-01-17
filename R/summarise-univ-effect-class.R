#' Summarise effect classifications (univariable)
#' @keywords internal
#' @param res (tbl) Results from `summarise_results_wrapper`
#' @param vartype (character) Variable type to use ("b" for binary or "c" for continuous)
#' @importFrom dplyr starts_with
summarise_univ_effect_class <- function(res, vartype) {

  levels = c("n_effect_sig_pos",
             "n_effect_non_neg",
             "n_effect_null",
             "n_effect_non_pos",
             "n_effect_sig_neg",
             "n_effect_unknown")

  labels =  c("1. Sig. Pos.", "2. Non Neg.", "3. Null",
    "4. Non Pos.", "5. Sig. Neg", "6. Unknown")

  res %>% dplyr::filter(.data$var.types == vartype) %>%
    dplyr::select(.data$n.events, .data$ub, .data$beta.true,
                  dplyr::starts_with("n_effect")) %>%
    tidyr::gather(class, count, dplyr::starts_with("n_effect")) %>%
    dplyr::mutate(
      n.events.string = stringr::str_pad(
        n.events, width = 2, side = "left", pad = "0")) %>%
    tidyr::unite(col = "ub_nevents", ub, n.events.string, sep = ", ") %>%
    dplyr::mutate(pct = .data$count/1000) %>%
    dplyr::select(-.data$count, -.data$n.events) %>%
    tidyr::spread(.data$ub_nevents, .data$pct) %>%
    dplyr::mutate(class = factor(class,
                                 levels = levels,
                                 labels = labels
    )) %>%
    dplyr::arrange(beta.true, class)
}


#' Summarise effect classifications (multivariable)
#' @keywords internal
#' @param res (tbl) Results from `summarise_results_wrapper`
summarise_multi_effect_class <- function(res) {

  levels = c("n_effect_sig_pos_all",
             "n_effect_non_neg",
             "n_effect_null_all",
             "n_effect_non_pos_all",
             "n_effect_sig_neg_all",
             "n_effect_unknown_all")

  labels =  c("1. Sig. Pos.", "2. Non Neg.", "3. Null",
              "4. Non Pos.", "5. Sig. Neg", "6. Unknown")

  res %>%
    dplyr::group_by(.data$beta.1.true, .data$beta.true,
                    .data$n.events, .data$n_vars) %>%
    dplyr::select(dplyr::group_cols(), dplyr::starts_with("n_effect")) %>%
    tidyr::gather(class, pct, dplyr::starts_with("n_effect")) %>%
    tidyr::spread(.data$n.events, .data$pct) %>%
    dplyr::mutate(class = factor(class, levels = levels,
                                 labels = labels)) %>%
    dplyr::arrange(beta.true, class) %>%
    dplyr::group_by(beta.true, n_vars, class) %>%
    dplyr::summarise_at(dplyr::vars(.data$`6`:.data$`80`), mean)

}

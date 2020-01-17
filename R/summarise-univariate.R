#' Summarize a single variable to match `plot_univariate`
#' @keywords internal
#' @param data (tbl) output from `summarise_results_wrapper`
#' @param var (string) name of variable to summarize

smry_univariate <- function(data, var) {

  data %>% dplyr::select_at(c("freq", "n.events", "beta.true", "ub",
                              var)) %>%
    dplyr::mutate(
      !!rlang::sym(var) := dplyr::case_when(

        !!rlang::sym(var) > 100 ~ formatC(!!rlang::sym(var),
                                          digits = 2, format = "e",
                                          drop0trailing = FALSE),

        TRUE ~ formatC(!!rlang::sym(var), digits = 2, format = "f")),

      n.events = stringr::str_pad(n.events, width = 2, side = "left",
                                  pad = "0"),

      ub = dplyr::case_when(
        ub == 900 ~ "Firth",
        ub == 1000 ~ "MLE",
        TRUE ~ as.character(ub)
      )) %>%

    tidyr::unite(col = "freq_n", .data$freq, .data$n.events, sep = ", ") %>%
    tidyr::spread(key = freq_n, value = !!rlang::sym(var)) %>%
    dplyr::arrange(.data$beta.true, .data$ub)
}

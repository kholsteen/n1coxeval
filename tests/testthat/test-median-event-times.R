context("Median Event Times")
library(n1coxeval)
library(testthat)
set.seed(3165)
test_that("median event times match target", {

### Set up all parameters for paper

  for (v in c("univ", "multiv")) {
    test_dir <- "C:/Users/Katherine Holsteen/Documents/My Documents/Research/Migraine_App/Code/n1coxeval/tests/testthat"
    params <- readRDS(file.path(test_dir,
                                paste0("params-", v, ".rds"))) %>%
      dplyr::mutate(n.subj = 50)

    for (p in 1:nrow(params)) {
      cat(paste0(p, "\n"))
      xvars <- n1coxeval::create_named_beta(sim = TRUE, param_row = params[p, ]) %>%
        names()

      data_list <- n1coxeval::generate_data_wrapper(
        sim = TRUE,
        param = params[p, ],
        x_vars = xvars,
        p = p)

      ### Check on median inter-event time
      purrr::map(data_list, ~dplyr::filter(., mig.ind == 1) %>%
                   dplyr::pull(t1)) %>% unlist() %>%
        summary() %>% print()

      med_event_time <-
        purrr::map(data_list, ~dplyr::filter(., mig.ind == 1) %>%
                   dplyr::pull(t1)) %>% unlist() %>% median()

      testthat::expect_equal(round(med_event_time),
                             params$median.days.btwn.events[[p]])

    }
  }

})

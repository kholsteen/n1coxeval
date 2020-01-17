context("Generated Predictors")
library(n1coxeval)
library(testthat)
set.seed(489484)
freqs =  c(0.25, 0.5, 0.75, NA)
betas = c(0,0,0,0)

data <- purrr::map_dfr(c(1:5), function(i) {

 n1coxeval:::generate_data_cox(n.vars = 4,
                             var.types = "bbbc",
                             beta = betas,
                             freq = freqs,
                             wb.shape = 2,
                             n.events = 1000,
                             max.n.days = NA) %>%
    dplyr::mutate(i = i)
})

test_that("frequency of binary predictors is on goal", {

  data_smry <- data %>% dplyr::group_by(i) %>%
    dplyr::summarise(
      n = dplyr::n(),
      X001 = mean(X001),
      X002 = mean(X002),
      X003 = mean(X003)) %>%
    tidyr::gather(key, value, -i, -n) %>%
    dplyr::mutate(num = as.numeric(substr(key, 4, 5))) %>%
    dplyr::mutate(goal = freqs[num],
                  diff = value - goal,
                  sign = sign(diff),
                  diff_threshold = 2*sqrt(goal*(1-goal)/n))

  ### Test that the differences are within 2 SE
  purrr::walk2(data_smry$diff,
               data_smry$diff_threshold,
               function(diff, diff_threshold) {
                 testthat::expect_lt(abs(diff), diff_threshold)
               })
})

test_that("distribution of continuous predictors is on goal", {

  data_smry <- data %>% dplyr::group_by(i) %>%
    dplyr::summarise(
      n = dplyr::n(),
      mX004 = mean(X004),
      sX004 = sd(X004)) %>%
    tidyr::gather(key, value, -i, -n) %>%
    dplyr::mutate(
      goal = dplyr::case_when(
        substr(key,1,1) == "m" ~ 0,
        substr(key,1,1) == "s" ~ 0.5
      ),
      diff = value - goal,
      sign = sign(diff),
      diff_threshold = dplyr::case_when(
        substr(key, 1, 1) == "m" ~ 2.5*sqrt(0.5/n),
        substr(key, 1, 1) == "s" ~ 0.01
      )
    )

  means <- data_smry %>% dplyr::filter(key == "mX004")
  sds <- data_smry %>% dplyr::filter(key == "sX004")

  purrr::walk2(data_smry$diff,
               data_smry$diff_threshold,
               function(diff, diff_threshold) {
                 testthat::expect_lt(abs(diff),diff_threshold)
               })

})

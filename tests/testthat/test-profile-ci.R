context("Profile CIs")
library(n1coxeval)
library(testthat)

test_that("profile CI matches Firth output", {

  ### coxph example in survival package
  data <- tibble::tibble(time = c(4,3,1,1,2,2,3),
                          status = c(1,1,1,0,1,1,0),
                          x = c(0,2,1,1,1,0,0),
                          sex = c(0,0,0,0,1,1,1))

  ### use breslow method to match Firth
  m1 <- survival::coxph(survival::Surv(time, status) ~ x, data = data,
                        ties = "breslow")
  test1 <- n1coxeval:::profile_ci(m1, "x", data = data)

  ### Firth estimates profile likelihood by default
  ### Need to specify unpenalized maximum likelihood to match survival::coxph
  m2 <- coxphf::coxphf(survival::Surv(time, status) ~ x, data = data,
                       firth = FALSE)

  testthat::expect_lt(abs(exp(test1[1]) - as.numeric(m2$ci.lower)), 1e-4)
  testthat::expect_lt(abs(exp(test1[2]) - as.numeric(m2$ci.upper)), 1e-4)

})

test_that("profile CI gives Inf for monotone likelihood", {

  data <- tibble::tibble(time =   c(1,1,2,2,3,3,4),
                         status = c(0,1,1,1,0,0,0),
                         x =      c(0,-2,-1,-1,-1,0,0),
                         sex =    c(0,0,0,0,1,1,1))

  m1 <- survival::coxph(survival::Surv(time, status) ~ x, data = data)
  test1 <- n1coxeval:::profile_ci(m1, "x", data = data)
  testthat::expect_equal(test1[1], -Inf)
  testthat::expect_lt(test1[2], 0)

  data <- tibble::tibble(time =   c(1,1,2,2,3,3,4),
                         status = c(0,1,1,1,0,0,0),
                         x =      c(0,2,1,1,1,0,0),
                         sex =    c(0,0,0,0,1,1,1))

  m1 <- survival::coxph(survival::Surv(time, status) ~ x, data = data)
  test1 <- n1coxeval:::profile_ci(m1, "x", data = data)
  testthat::expect_equal(test1[2], Inf)
  testthat::expect_gt(test1[1], 0)

})

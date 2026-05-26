test_that("survplot returns fitted and Kaplan-Meier data", {
  msm_fit = test_msm_fit()
  out = suppressMessages(
    survplot(msm_fit, km = TRUE, out = "all", grid = 5)
 )

  expect_named(out, c("p", "fitted", "km"))
  expect_s3_class(out$p, "ggplot")
  expect_s3_class(out$fitted, "data.table")
  expect_s3_class(out$km, "data.table")
})

test_that("survplot can return without printing", {
  msm_fit = test_msm_fit()
  utils::capture.output(
    out <- survplot(msm_fit, km = TRUE, out = "all", grid = 5,
                     print_plot = FALSE)
 )

  expect_named(out, c("p", "fitted", "km"))
  expect_s3_class(out$p, "ggplot")
  expect_s3_class(out$fitted, "data.table")
  expect_s3_class(out$km, "data.table")
})

test_that("prevplot returns a ggplot object", {
  msm_fit = test_msm_fit()
  hosp_aug = attr(msm_fit, "msmtools_data")
  prev = msm::prevalence.msm(
    msm_fit, covariates = "mean", ci = "normal",
    times = seq(min(hosp_aug$augmented_int), max(hosp_aug$augmented_int),
                 length.out = 4)
 )

  out = suppressMessages(prevplot(msm_fit, prev, ci = TRUE, M = FALSE))

  expect_s3_class(out, "ggplot")
})

test_that("prevplot with M = TRUE returns a patchwork when available", {
  testthat::skip_if_not_installed("patchwork")
  msm_fit = test_msm_fit()
  hosp_aug = attr(msm_fit, "msmtools_data")
  prev = msm::prevalence.msm(
    msm_fit, covariates = "mean", ci = "normal",
    times = seq(min(hosp_aug$augmented_int), max(hosp_aug$augmented_int),
                 length.out = 4)
 )

  out = suppressMessages(
    prevplot(msm_fit, prev, ci = TRUE, M = TRUE, print_plot = FALSE)
 )

  expect_s3_class(out, "patchwork")
})

test_that("prevplot can return without printing", {
  msm_fit = test_msm_fit()
  hosp_aug = attr(msm_fit, "msmtools_data")
  prev = msm::prevalence.msm(
    msm_fit, covariates = "mean", ci = "normal",
    times = seq(min(hosp_aug$augmented_int), max(hosp_aug$augmented_int),
                 length.out = 4)
 )
  utils::capture.output(
    out <- prevplot(msm_fit, prev, ci = TRUE, M = FALSE,
                     print_plot = FALSE)
 )

  expect_s3_class(out, "ggplot")
})

test_that("plot verbosity controls messages", {
  msm_fit = test_msm_fit()
  quiet = utils::capture.output(
    survplot(msm_fit, grid = 5, print_plot = FALSE),
    type = "message"
 )
  summary = utils::capture.output(
    survplot(msm_fit, grid = 5, print_plot = FALSE, verbosity = "summary"),
    type = "message"
 )

  expect_equal(quiet, character())
  expect_true(length(summary) > 0L)
})

test_that("survplot validates plotting arguments", {
  msm_fit = test_msm_fit()

  expect_error(
    survplot(msm_fit, exacttimes = NA, print_plot = FALSE),
    "exacttimes must be either TRUE or FALSE"
 )
  expect_error(
    survplot(msm_fit, km = c(TRUE, FALSE), print_plot = FALSE),
    "km must be either TRUE or FALSE"
 )
  expect_error(
    survplot(msm_fit, print_plot = "yes"),
    "print_plot must be either TRUE or FALSE"
 )
  expect_error(
    survplot(msm_fit, from = 0, print_plot = FALSE),
    "from must be a positive scalar numeric"
 )
  expect_error(
    survplot(msm_fit, to = "DEAD", print_plot = FALSE),
    "to must be a positive scalar numeric"
 )
  expect_error(
    survplot(msm_fit, grid = 0, print_plot = FALSE),
    "grid must be a positive scalar numeric"
 )
  expect_error(
    survplot(msm_fit, B = 0, print_plot = FALSE),
    "B must be a positive scalar numeric"
 )
  expect_error(
    survplot(msm_fit, range = c(1, Inf), print_plot = FALSE),
    "range must be a finite numeric vector of two elements"
 )
  expect_error(
    survplot(msm_fit, times = numeric(), print_plot = FALSE),
    "times must be a finite non-empty numeric vector"
 )
})

test_that("prevplot validates plotting arguments", {
  msm_fit = test_msm_fit()
  hosp_aug = attr(msm_fit, "msmtools_data")
  prev = msm::prevalence.msm(
    msm_fit, covariates = "mean", ci = "normal",
    times = seq(min(hosp_aug$augmented_int), max(hosp_aug$augmented_int),
                 length.out = 4)
 )

  expect_error(
    prevplot(msm_fit, prev, exacttimes = NA, print_plot = FALSE),
    "exacttimes must be either TRUE or FALSE"
 )
  expect_error(
    prevplot(msm_fit, prev, M = c(TRUE, FALSE), print_plot = FALSE),
    "M must be either TRUE or FALSE"
 )
  expect_error(
    prevplot(msm_fit, prev, ci = NA, print_plot = FALSE),
    "ci must be either TRUE or FALSE"
 )
  expect_error(
    prevplot(msm_fit, prev, print_plot = "yes"),
    "print_plot must be either TRUE or FALSE"
 )
})

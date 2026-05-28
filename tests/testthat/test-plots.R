test_that("survplot returns a ggplot decorated with fitted and KM data", {
  msm_fit = test_msm_fit()
  p = suppressMessages(survplot(msm_fit, km = TRUE, grid = 5))

  expect_s3_class(p, "ggplot")
  expect_s3_class(p$fitted, "data.table")
  expect_s3_class(p$km, "data.table")
})

test_that("survplot can return without printing", {
  msm_fit = test_msm_fit()
  utils::capture.output(
    p <- survplot(msm_fit, km = TRUE, grid = 5, print_plot = FALSE)
 )

  expect_s3_class(p, "ggplot")
  expect_s3_class(p$fitted, "data.table")
  expect_s3_class(p$km, "data.table")
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

test_that("survplot with ci = 'normal' returns survival curves with bounds", {
  msm_fit = test_msm_fit()
  p = suppressMessages(
    survplot(msm_fit, km = TRUE, ci = "normal", ci_km = "log",
             grid = 5, B = 2L, print_plot = FALSE)
 )

  expect_s3_class(p, "ggplot")
  expect_true(all(c("lwr", "upr") %in% names(p$fitted)))
  expect_true(all(c("lwr", "upr") %in% names(p$km)))
})

test_that("survplot honours exacttimes = FALSE", {
  msm_fit = test_msm_fit()
  p = suppressMessages(
    survplot(msm_fit, km = TRUE, exacttimes = FALSE, ci_km = "log",
             grid = 5, print_plot = FALSE)
 )

  expect_s3_class(p, "ggplot")
  expect_false("time_exact" %in% names(p$km))
})

test_that("survplot honours interp = 'midpoint'", {
  msm_fit = test_msm_fit()
  p = suppressMessages(
    survplot(msm_fit, km = TRUE, interp = "midpoint",
             grid = 5, print_plot = FALSE)
 )

  expect_s3_class(p, "ggplot")
  expect_s3_class(p$km, "data.table")
})

test_that("survplot supports custom range and times", {
  msm_fit = test_msm_fit()
  rg = range(stats::model.extract(msm_fit$data$mf, "time"))
  custom_times = seq(rg[1L], rg[2L], length.out = 6L)

  out_range = suppressMessages(
    survplot(msm_fit, range = rg, grid = 5, print_plot = FALSE)
 )
  out_times = suppressMessages(
    survplot(msm_fit, times = custom_times, print_plot = FALSE)
 )
  out_times_inexact = suppressMessages(
    survplot(msm_fit, times = custom_times, exacttimes = FALSE,
             print_plot = FALSE)
 )

  expect_s3_class(out_range, "ggplot")
  expect_s3_class(out_times, "ggplot")
  expect_s3_class(out_times_inexact, "ggplot")
})

test_that("survplot attaches $fitted always and $km only when km = TRUE", {
  msm_fit = test_msm_fit()
  p_no_km = suppressMessages(
    survplot(msm_fit, grid = 5, print_plot = FALSE)
 )
  p_km = suppressMessages(
    survplot(msm_fit, km = TRUE, grid = 5, print_plot = FALSE)
 )

  expect_s3_class(p_no_km$fitted, "data.table")
  expect_null(p_no_km$km)
  expect_s3_class(p_km$fitted, "data.table")
  expect_s3_class(p_km$km, "data.table")
})

test_that("survplot's $fitted survives print()", {
  msm_fit = test_msm_fit()
  p = suppressMessages(survplot(msm_fit, km = TRUE, grid = 5, print_plot = FALSE))
  out = utils::capture.output(print(p))
  expect_s3_class(p$fitted, "data.table")
  expect_s3_class(p$km, "data.table")
})

test_that("survplot errors on invalid inputs", {
  msm_fit = test_msm_fit()

  expect_error(
    survplot("not a msm fit", print_plot = FALSE),
    "x must be a msm model"
 )
  expect_error(
    survplot(msm_fit, to = 1, print_plot = FALSE),
    "to must be an absorbing state"
 )
})

test_that("survplot rejects the removed `out` argument with a helpful error", {
  msm_fit = test_msm_fit()

  expect_error(
    survplot(msm_fit, out = "all", grid = 5, print_plot = FALSE),
    "removed in msmtools 2\\.2\\.0"
 )
  expect_error(
    survplot(msm_fit, out = "fitted", grid = 5, print_plot = FALSE),
    "p\\$fitted"
 )
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

test_that("prevplot supports the ci = FALSE path", {
  msm_fit = test_msm_fit()
  hosp_aug = attr(msm_fit, "msmtools_data")
  prev = msm::prevalence.msm(
    msm_fit, covariates = "mean", ci = "normal",
    times = seq(min(hosp_aug$augmented_int), max(hosp_aug$augmented_int),
                length.out = 4)
 )

  out = suppressMessages(
    prevplot(msm_fit, prev, ci = FALSE, M = FALSE, print_plot = FALSE)
 )

  expect_s3_class(out, "ggplot")
})

test_that("prevplot prints the combined layout when print_plot = TRUE", {
  testthat::skip_if_not_installed("patchwork")
  msm_fit = test_msm_fit()
  hosp_aug = attr(msm_fit, "msmtools_data")
  prev = msm::prevalence.msm(
    msm_fit, covariates = "mean", ci = "normal",
    times = seq(min(hosp_aug$augmented_int), max(hosp_aug$augmented_int),
                length.out = 4)
 )

  utils::capture.output(
    out <- suppressMessages(
      prevplot(msm_fit, prev, ci = TRUE, M = TRUE, print_plot = TRUE)
   )
 )

  expect_s3_class(out, "patchwork")
})

test_that("prevplot attaches the prevalence data table on both branches", {
  msm_fit = test_msm_fit()
  hosp_aug = attr(msm_fit, "msmtools_data")
  prev = msm::prevalence.msm(
    msm_fit, covariates = "mean", ci = "normal",
    times = seq(min(hosp_aug$augmented_int), max(hosp_aug$augmented_int),
                length.out = 4)
 )

  p_basic = suppressMessages(
    prevplot(msm_fit, prev, ci = FALSE, M = FALSE, print_plot = FALSE)
 )
  expect_s3_class(p_basic$prevalence, "data.table")
  expect_true(all(c("time", "state", "obs", "hat") %in% names(p_basic$prevalence)))

  p_ci = suppressMessages(
    prevplot(msm_fit, prev, ci = TRUE, M = FALSE, print_plot = FALSE)
 )
  expect_s3_class(p_ci$prevalence, "data.table")
  expect_true(all(c("lwr", "upr") %in% names(p_ci$prevalence)))

  testthat::skip_if_not_installed("patchwork")
  p_m = suppressMessages(
    prevplot(msm_fit, prev, ci = TRUE, M = TRUE, print_plot = FALSE)
 )
  expect_s3_class(p_m$prevalence, "data.table")
  expect_true("M" %in% names(p_m$prevalence))
})

test_that("prevplot errors on invalid input classes", {
  msm_fit = test_msm_fit()
  hosp_aug = attr(msm_fit, "msmtools_data")
  prev = msm::prevalence.msm(
    msm_fit, covariates = "mean",
    times = seq(min(hosp_aug$augmented_int), max(hosp_aug$augmented_int),
                length.out = 4)
 )

  expect_error(
    prevplot("not a msm fit", prev, print_plot = FALSE),
    "x must be a msm model"
 )
  expect_error(
    prevplot(msm_fit, "not a list", print_plot = FALSE),
    "prev.obj must be a list"
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

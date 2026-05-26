test_that( "survplot returns fitted and Kaplan-Meier data", {
  msm_fit = test_msm_fit()
  out = suppressMessages(
    survplot( msm_fit, km = TRUE, out = "all", grid = 5 )
  )

  expect_named( out, c( "p", "fitted", "km" ) )
  expect_s3_class( out$p, "ggplot" )
  expect_s3_class( out$fitted, "data.table" )
  expect_s3_class( out$km, "data.table" )
} )

test_that( "survplot can return without printing", {
  msm_fit = test_msm_fit()
  utils::capture.output(
    out <- survplot( msm_fit, km = TRUE, out = "all", grid = 5,
                     print_plot = FALSE )
  )

  expect_named( out, c( "p", "fitted", "km" ) )
  expect_s3_class( out$p, "ggplot" )
  expect_s3_class( out$fitted, "data.table" )
  expect_s3_class( out$km, "data.table" )
} )

test_that( "prevplot returns a ggplot object", {
  msm_fit = test_msm_fit()
  hosp_aug = attr( msm_fit, "msmtools_data" )
  prev = msm::prevalence.msm(
    msm_fit, covariates = "mean", ci = "normal",
    times = seq( min( hosp_aug$augmented_int ), max( hosp_aug$augmented_int ),
                 length.out = 4 )
  )

  out = suppressMessages( prevplot( msm_fit, prev, ci = TRUE, M = FALSE ) )

  expect_s3_class( out, "ggplot" )
} )

test_that( "prevplot can return without printing", {
  msm_fit = test_msm_fit()
  hosp_aug = attr( msm_fit, "msmtools_data" )
  prev = msm::prevalence.msm(
    msm_fit, covariates = "mean", ci = "normal",
    times = seq( min( hosp_aug$augmented_int ), max( hosp_aug$augmented_int ),
                 length.out = 4 )
  )
  utils::capture.output(
    out <- prevplot( msm_fit, prev, ci = TRUE, M = FALSE,
                     print_plot = FALSE )
  )

  expect_s3_class( out, "ggplot" )
} )

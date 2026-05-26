test_that("augment warns when t_cens is used as death time", {
  expect_warning(
    augment(test_hosp(), subj, adm_number, label_3, t_start = dateIN,
             t_end = dateOUT, t_cens = dateCENS),
    "no t_death has been passed"
 )
})

test_that("n_events must be an integer", {
  expect_error(
    augment(test_hosp(), subj, !as.integer(adm_number), label_3,
             t_start = dateIN, t_end = dateOUT, t_cens = dateCENS,
             t_death = dateCENS),
    "n_events must be an integer"
 )
})

test_that("augment validates required inputs and state shape", {
  expect_error(augment(), "dataset")
  expect_error(augment(data.frame()), "keying")
  expect_error(augment(test_hosp(), subj), "pattern")
  expect_error(
    augment(test_hosp(), subj, adm_number, label_3, state = list("IN"),
             t_start = dateIN, t_end = dateOUT, t_cens = dateCENS),
    "state must be a character vector"
 )
  expect_error(
    augment(test_hosp(), subj, adm_number, label_3, t_end = dateOUT,
             t_cens = dateCENS),
    "starting and an ending"
 )
  expect_error(
    augment(test_hosp(), subj, adm_number, label_3, t_start = dateIN,
             t_end = dateOUT),
    "censoring time"
 )
})

test_that("augment validates time classes", {
  hosp_bad = test_hosp()
  hosp_bad[, dateOUT_num := as.numeric(dateOUT)]

  expect_warning(
    expect_error(
      augment(hosp_bad, subj, adm_number, label_3, t_start = dateIN,
               t_end = dateOUT_num, t_cens = dateCENS),
      "same class"
   ),
    "no t_death has been passed"
 )
  expect_error(
    augment(hosp_bad, subj, adm_number, label_3, t_start = dateIN,
             t_end = dateOUT, t_cens = dateCENS, t_death = dateOUT_num),
    "same class"
 )
})

test_that("two-state and three-state pattern inputs are equivalent", {
  hosp_aug_2 = augment_hosp(pattern = "label_2")
  hosp_aug_3 = augment_hosp(pattern = "label_3")

  expect_identical(hosp_aug_2, hosp_aug_3)
})

test_that("missing n_events is reconstructed from subject order", {
  hosp_aug = augment_hosp()
  hosp_aug_no_events = suppressWarnings(
    augment(test_hosp(), subj, pattern = label_3, t_start = dateIN,
             t_end = dateOUT, t_cens = dateCENS)
 )

  expect_identical(hosp_aug$adm_number, hosp_aug_no_events$n_events)
})

test_that("state must be a valid character vector", {
  expect_error(
    augment(test_hosp(), subj, adm_number, label_3,
             state = list("IN", "OUT", "DEAD"), t_start = dateIN,
             t_end = dateOUT, t_cens = dateCENS, t_death = dateCENS),
    "state must be a character vector"
 )
  expect_error(
    augment(test_hosp(), subj, adm_number, label_3,
             state = c("IN", "OUT"), t_start = dateIN,
             t_end = dateOUT, t_cens = dateCENS, t_death = dateCENS),
    "state must be a character vector"
 )
  expect_error(
    augment(test_hosp(), subj, adm_number, label_3,
             state = c("IN", NA, "DEAD"), t_start = dateIN,
             t_end = dateOUT, t_cens = dateCENS, t_death = dateCENS),
    "state must be a character vector"
 )
  expect_error(
    augment(test_hosp(), subj, adm_number, label_3,
             state = c("IN", "", "DEAD"), t_start = dateIN,
             t_end = dateOUT, t_cens = dateCENS, t_death = dateCENS),
    "state must be a character vector"
 )
  expect_error(
    augment(test_hosp(), subj, adm_number, label_3,
             state = c("IN", "IN", "DEAD"), t_start = dateIN,
             t_end = dateOUT, t_cens = dateCENS, t_death = dateCENS),
    "state must be a character vector"
 )
})

test_that("state vector controls generated transition labels", {
  custom_state = c("ENTRY", "EXIT", "ABSORBING")
  hosp_aug_3 = augment_hosp(state = custom_state, pattern = "label_3")
  hosp_aug_2 = augment_hosp(state = custom_state, pattern = "label_2")

  expect_true(all(hosp_aug_3$status %in% custom_state))
  expect_true(all(hosp_aug_2$status %in% custom_state))
  expect_true(all(custom_state %in% hosp_aug_3$status))
  expect_true(all(custom_state %in% hosp_aug_2$status))
  expect_true(all(hosp_aug_3$n_status[hosp_aug_3$status == "ABSORBING"] ==
                      "ABSORBING"))
  expect_true(all(hosp_aug_2$n_status[hosp_aug_2$status == "ABSORBING"] ==
                      "ABSORBING"))
})

test_that("check_NA catches missing values and passes clean data", {
  expect_warning(
    expect_no_error(
      augment(test_hosp(), subj, adm_number, label_3, t_start = dateIN,
               t_end = dateOUT, t_cens = dateCENS, check_NA = TRUE)
   ),
    "no t_death has been passed"
 )

  hosp_missing = test_hosp()
  hosp_missing[1, dateIN := as.Date(NA)]

  expect_warning(
    expect_error(
      augment(hosp_missing, subj, adm_number, label_3, t_start = dateIN,
               t_end = dateOUT, t_cens = dateCENS, check_NA = TRUE),
      "dateIN"
   ),
    "no t_death has been passed"
 )
})

test_that("augment returns a data.table", {
  aug_dt = augment_hosp()

  expect_s3_class(aug_dt, "data.table")
  expect_s3_class(as.data.frame(aug_dt), "data.frame")
})

test_that("verbosity controls informational output", {
  messages = utils::capture.output(
    aug <- augment(test_hosp(), subj, adm_number, label_3, t_start = dateIN,
                    t_end = dateOUT, t_cens = dateCENS, t_death = dateCENS,
                    verbosity = "summary"),
    type = "message"
 )
  expect_s3_class(aug, "data.table")
  expect_true(any(grepl("setting everything up", messages)))

  progress_messages = utils::capture.output(
    aug_progress <- augment(test_hosp(), subj, adm_number, label_3,
                             t_start = dateIN, t_end = dateOUT,
                             t_cens = dateCENS, t_death = dateCENS,
                             verbosity = "progress"),
    type = "message"
 )
  expect_s3_class(aug_progress, "data.table")
  expect_true(any(grepl("adding status flag", progress_messages)))

  expect_error(
    augment(test_hosp(), subj, adm_number, label_3, t_start = dateIN,
             t_end = dateOUT, t_cens = dateCENS, t_death = dateCENS,
             verbosity = "loud"),
    "arg"
 )
})

test_that("copy validates logical flags", {
  expect_error(
    augment(test_hosp(), subj, adm_number, label_3, t_start = dateIN,
             t_end = dateOUT, t_cens = dateCENS, t_death = dateCENS,
             copy = "yes"),
    "copy must be either TRUE or FALSE"
 )
  expect_error(
    augment(test_hosp(), subj, adm_number, label_3, t_start = dateIN,
             t_end = dateOUT, t_cens = dateCENS, t_death = dateCENS,
             check_NA = NA),
    "check_NA must be either TRUE or FALSE"
 )
})

test_that("Date inputs create integer augmented time", {
  hosp_aug = augment_hosp(t_augmented = event_time)

  expect_true("event_time" %in% names(hosp_aug))
  expect_true("event_time_int" %in% names(hosp_aug))
  expect_s3_class(hosp_aug$event_time, "Date")
  expect_type(hosp_aug$event_time_int, "integer")
})

test_that("numeric time inputs keep numeric augmented time", {
  hosp_num = test_hosp()
  hosp_num[, dateIN_num := as.numeric(dateIN)]
  hosp_num[, dateOUT_num := as.numeric(dateOUT)]
  hosp_num[, dateCENS_num := as.numeric(dateCENS)]

  hosp_aug = suppressWarnings(
    augment(hosp_num, subj, adm_number, label_3, t_start = dateIN_num,
             t_end = dateOUT_num, t_cens = dateCENS_num)
 )

  expect_true("augmented" %in% names(hosp_aug))
  expect_false("augmented_int" %in% names(hosp_aug))
  expect_type(hosp_aug$augmented, "double")
})

test_that("integer and factor patterns are accepted", {
  hosp_int = test_hosp()
  hosp_int[, label_int := data.table::fifelse(
    label_3 == "alive", 0L,
    data.table::fifelse(label_3 == "dead_in", 1L, 2L)
 )]
  hosp_factor = test_hosp()
  hosp_factor[, label_factor := factor(label_3,
                                         levels = c("alive", "dead_in", "dead_out"))]

  int_aug = suppressWarnings(
    augment(hosp_int, subj, adm_number, label_int, t_start = dateIN,
             t_end = dateOUT, t_cens = dateCENS)
 )
  factor_aug = suppressWarnings(
    augment(hosp_factor, subj, adm_number, label_factor, t_start = dateIN,
             t_end = dateOUT, t_cens = dateCENS)
 )

  expect_equal(nrow(int_aug), nrow(factor_aug))
  expect_equal(int_aug$status, factor_aug$status)
})

test_that("difftime inputs create numeric augmented time", {
  hosp_diff = test_hosp()
  origin = min(hosp_diff$dateIN)
  hosp_diff[, dateIN_diff := as.difftime(as.numeric(dateIN - origin),
                                           units = "days")]
  hosp_diff[, dateOUT_diff := as.difftime(as.numeric(dateOUT - origin),
                                            units = "days")]
  hosp_diff[, dateCENS_diff := as.difftime(as.numeric(dateCENS - origin),
                                             units = "days")]

  hosp_aug = suppressWarnings(
    augment(hosp_diff, subj, adm_number, label_3, t_start = dateIN_diff,
             t_end = dateOUT_diff, t_cens = dateCENS_diff)
 )

  expect_true("augmented_num" %in% names(hosp_aug))
  expect_s3_class(hosp_aug$augmented, "difftime")
  expect_type(hosp_aug$augmented_num, "double")
})

test_that("supplied t_death avoids the censoring warning", {
  expect_warning(
    augment(test_hosp(), subj, adm_number, label_3, t_start = dateIN,
             t_end = dateOUT, t_cens = dateCENS, t_death = dateCENS),
    NA
 )
})

test_that("more_status creates expanded status columns", {
  hosp_aug = augment_hosp(more_status = rehab_it)

  expect_true(all(c("status_exp", "status_exp_num", "n_status_exp") %in%
                      names(hosp_aug)))
  expect_false(anyNA(hosp_aug$status_exp))
  expect_false(anyNA(hosp_aug$status_exp_num))
  expect_false(anyNA(hosp_aug$n_status_exp))
})

test_that("explicit NULL more_status matches omitted more_status", {
  expect_identical(augment_hosp(), augment_hosp(more_status = NULL))
})

test_that("alive subjects close the at-risk window at t_cens (issue #7)", {
  hosp_raw = data.table::as.data.table(test_hosp())
  hosp_aug = augment_hosp()

  subj2_raw = hosp_raw[subj == 2]
  subj2_aug = hosp_aug[subj == 2]
  expected_last_int = as.integer(max(subj2_raw$dateCENS))
  n = nrow(subj2_aug)

  expect_equal(as.integer(subj2_aug$augmented[n]), expected_last_int)
  expect_equal(subj2_aug$augmented_int[n], expected_last_int)

  last_per_subj = hosp_raw[, .SD[.N], by = subj]
  alive_subjects = last_per_subj[label_3 == "alive", subj]
  expect_true(length(alive_subjects) > 0L)
  for (sid in alive_subjects) {
    rows = hosp_aug[subj == sid]
    m = nrow(rows)
    expect_gt(rows$augmented_int[m], rows$augmented_int[m - 1L])
  }
})

test_that("dead-subject augmented times close at the death/cens time", {
  hosp_raw = data.table::as.data.table(test_hosp())
  hosp_aug = augment_hosp()

  last_per_subj = hosp_raw[, .SD[.N], by = subj]
  dead_subjects = last_per_subj[label_3 %in% c("dead_in", "dead_out"), subj]
  expect_true(length(dead_subjects) > 0L)
  for (sid in dead_subjects) {
    expected_int = as.integer(max(hosp_raw[subj == sid]$dateCENS))
    aug_rows = hosp_aug[subj == sid]
    expect_equal(aug_rows$augmented_int[nrow(aug_rows)], expected_int)
  }
})

test_that("two-value pattern with explicit t_death is augmented", {
  hosp_aug = augment(test_hosp(), subj, adm_number, label_2,
                     t_start = dateIN, t_end = dateOUT,
                     t_cens = dateCENS, t_death = dateCENS)

  expect_s3_class(hosp_aug, "data.table")
  expect_true("status" %in% names(hosp_aug))
})

test_that("non-monotonic n_events stops augment with a helpful error", {
  bad = test_hosp()
  bad[, adm_number := rev(adm_number), by = subj]

  expect_error(
    suppressWarnings(
      augment(bad, subj, adm_number, label_3, t_start = dateIN,
               t_end = dateOUT, t_cens = dateCENS,
               verbosity = "summary")
   ),
    "fix the issues and relaunch"
 )
})

test_that("pattern with fewer than two unique values is rejected", {
  single = test_hosp()
  single[, single_pattern := 0L]

  expect_error(
    suppressWarnings(
      augment(single, subj, adm_number, single_pattern, t_start = dateIN,
               t_end = dateOUT, t_cens = dateCENS)
   ),
    "at least 2 elements"
 )
})

test_that("pattern with more than three unique values is rejected", {
  many = test_hosp()
  many[, many_pattern := rep(c(0L, 1L, 2L, 3L), length.out = .N)]

  expect_error(
    suppressWarnings(
      augment(many, subj, adm_number, many_pattern, t_start = dateIN,
               t_end = dateOUT, t_cens = dateCENS)
   ),
    "2 or 3 unique values"
 )
})

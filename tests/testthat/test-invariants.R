column_classes = function(data) {
  vapply(data, function(x) paste(class(x), collapse = "/"), character(1L))
}

test_that("augment output matches the v2.0.9 Date golden fixture", {
  current = augment_hosp()
  expected = readRDS(testthat::test_path("fixtures", "augment-hosp-date.rds"))

  expect_identical(current, expected)
})

test_that("augment preserves core output structure across time classes", {
  date_aug = augment_hosp()

  numeric_data = test_hosp()
  numeric_data[, dateIN := as.numeric(dateIN)]
  numeric_data[, dateOUT := as.numeric(dateOUT)]
  numeric_data[, dateCENS := as.numeric(dateCENS)]
  numeric_aug = suppressWarnings(
    augment(numeric_data, subj, adm_number, label_3, t_start = dateIN,
             t_end = dateOUT, t_cens = dateCENS)
 )

  diff_data = test_hosp()
  origin = min(diff_data$dateIN)
  diff_data[, dateIN := as.difftime(as.numeric(dateIN - origin),
                                      units = "days")]
  diff_data[, dateOUT := as.difftime(as.numeric(dateOUT - origin),
                                       units = "days")]
  diff_data[, dateCENS := as.difftime(as.numeric(dateCENS - origin),
                                        units = "days")]
  diff_aug = suppressWarnings(
    augment(diff_data, subj, adm_number, label_3, t_start = dateIN,
             t_end = dateOUT, t_cens = dateCENS)
 )

  core_columns = c("status", "status_num", "n_status", "augmented")

  expect_true(all(core_columns %in% names(date_aug)))
  expect_true(all(core_columns %in% names(numeric_aug)))
  expect_true(all(core_columns %in% names(diff_aug)))
  expect_true("augmented_int" %in% names(date_aug))
  expect_false("augmented_int" %in% names(numeric_aug))
  expect_true("augmented_num" %in% names(diff_aug))
  expect_identical(column_classes(date_aug)[core_columns],
                    column_classes(readRDS(testthat::test_path(
                      "fixtures", "augment-hosp-date.rds")))[core_columns])
})

test_that("augment expanded status columns remain stable", {
  expanded = augment_hosp(more_status = rehab_it)

  expect_true(all(c("status_exp", "status_exp_num", "n_status_exp") %in%
                      names(expanded)))
  expect_false(anyNA(expanded$status_exp))
  expect_false(anyNA(expanded$status_exp_num))
  expect_false(anyNA(expanded$n_status_exp))
  expect_true(all(expanded$status_exp[expanded$status == "DEAD"] == "DEAD"))
  expect_true(all(grepl("_", expanded$status_exp[expanded$status != "DEAD"])))
})

test_that("augment current by-reference behavior is documented by tests", {
  input = test_hosp()
  before_names = data.table::copy(names(input))
  before_key = data.table::copy(data.table::key(input))

  suppressWarnings(
    augment(input, subj, pattern = label_3, t_start = dateIN,
             t_end = dateOUT, t_cens = dateCENS)
 )

  expect_false("n_events" %in% before_names)
  expect_true("n_events" %in% names(input))
  expect_false(identical(before_key, data.table::key(input)))
  expect_identical(data.table::key(input), "subj")
})

test_that("augment copy protects caller-owned data", {
  input = test_hosp()
  before_names = data.table::copy(names(input))
  before_key = data.table::copy(data.table::key(input))

  out = suppressWarnings(
    augment(input, subj, pattern = label_3, t_start = dateIN,
             t_end = dateOUT, t_cens = dateCENS, copy = TRUE)
 )

  expect_identical(names(input), before_names)
  expect_identical(data.table::key(input), before_key)
  expect_false("n_events" %in% names(input))
  expect_true("n_events" %in% names(out))
  expect_s3_class(out, "data.table")
})

test_that("augment copy protects data.frame inputs", {
  input = as.data.frame(test_hosp())
  before_names = names(input)
  before_class = class(input)

  out = suppressWarnings(
    augment(input, subj, adm_number, label_3, t_start = dateIN,
             t_end = dateOUT, t_cens = dateCENS, copy = TRUE)
 )

  expect_identical(names(input), before_names)
  expect_identical(class(input), before_class)
  expect_false(inherits(input, "data.table"))
  expect_s3_class(out, "data.table")
})

test_that("polish current by-reference behavior is documented by tests", {
  input = augment_hosp()
  before_names = data.table::copy(names(input))
  before_key = data.table::copy(data.table::key(input))

  polish(input, subj, label_3)

  expect_identical(names(input), before_names)
  expect_false(identical(before_key, data.table::key(input)))
  expect_identical(data.table::key(input), "subj")
})

test_that("polish copy protects caller-owned data", {
  input = augment_hosp()
  before_names = data.table::copy(names(input))
  before_key = data.table::copy(data.table::key(input))

  out = polish(input, subj, label_3, copy = TRUE)

  expect_identical(names(input), before_names)
  expect_identical(data.table::key(input), before_key)
  expect_false("index" %in% names(input))
  expect_s3_class(out, "data.table")
})

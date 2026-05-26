test_that("polish leaves data unchanged when no duplicate transition exists", {
  hosp_aug = augment_hosp()
  hosp_clean = polish(data.table::copy(hosp_aug), subj, label_3)

  expect_equal(as.data.frame(hosp_clean), as.data.frame(hosp_aug))
})

test_that("polish removes subjects with duplicate transition times", {
  hosp_aug = augment_hosp()
  duplicate_input = data.table::copy(hosp_aug)
  rows = which(duplicate_input$subj == 1 & duplicate_input$status != "DEAD")
  duplicate_input[rows[2], augmented := duplicate_input[rows[1], augmented]]
  duplicate_input[rows[2], augmented_int := duplicate_input[rows[1], augmented_int]]

  hosp_clean = polish(duplicate_input, subj, label_3)

  expect_false(1 %in% hosp_clean$subj)
  expect_lt(nrow(hosp_clean), nrow(hosp_aug))
  expect_equal(data.table::uniqueN(hosp_clean$subj), 9L)
})

test_that("polish returns a data.table", {
  hosp_aug = augment_hosp()
  hosp_clean = polish(data.table::copy(hosp_aug), subj, label_3)

  expect_s3_class(hosp_clean, "data.table")
  expect_s3_class(as.data.frame(hosp_clean), "data.frame")
})

test_that("polish resolves time columns explicitly", {
  hosp_aug = augment_hosp()

  expect_identical(
    polish(data.table::copy(hosp_aug), subj, label_3),
    polish(data.table::copy(hosp_aug), subj, label_3, time = NULL)
 )
  expect_identical(
    polish(data.table::copy(hosp_aug), subj, label_3),
    polish(data.table::copy(hosp_aug), subj, label_3, time = augmented_int)
 )

  fallback_input = data.table::copy(hosp_aug)
  fallback_input[, augmented_num := augmented_int]
  fallback_input[, augmented_int := NULL]
  fallback = polish(fallback_input, subj, label_3)

  expect_s3_class(fallback, "data.table")
  expect_true("augmented_num" %in% names(fallback))
})

test_that("polish validates logical flags", {
  hosp_aug = augment_hosp()

  expect_error(
    polish(data.table::copy(hosp_aug), subj, label_3, copy = "yes"),
    "copy must be either TRUE or FALSE"
 )
  expect_error(
    polish(data.table::copy(hosp_aug), subj, label_3, check_NA = NA),
    "check_NA must be either TRUE or FALSE"
 )
})

test_that("polish validates columns and pattern schemas", {
  hosp_aug = augment_hosp()

  no_time = data.table::copy(hosp_aug)
  no_time[, augmented_int := NULL]
  expect_error(
    polish(no_time, subj, label_3),
    "time must be provided"
 )
  expect_error(
    polish(data.table::copy(hosp_aug), missing_subject, label_3),
    "not present in data: missing_subject"
 )
  expect_error(
    polish(data.table::copy(hosp_aug), subj, missing_pattern),
    "not present in data: missing_pattern"
 )
  expect_error(
    polish(data.table::copy(hosp_aug), subj, label_3, time = missing_time),
    "not present in data: missing_time"
 )

  one_value = data.table::copy(hosp_aug)
  one_value[, one_pattern := "alive"]
  expect_error(
    polish(one_value, subj, one_pattern),
    "pattern must have 2 or 3 unique values"
 )

  four_values = data.table::copy(hosp_aug)
  four_values[, four_pattern := rep(c("a", "b", "c", "d"), length.out = .N)]
  expect_error(
    polish(four_values, subj, four_pattern),
    "pattern must have 2 or 3 unique values"
 )
})

test_that("polish reports checked missing values clearly", {
  hosp_aug = augment_hosp()
  missing_time = data.table::copy(hosp_aug)
  missing_time[1L, augmented_int := NA_real_]

  expect_error(
    polish(missing_time, subj, label_3, check_NA = TRUE),
    "missing values detected in: augmented_int"
 )
})

test_that("polish errors on missing mandatory arguments", {
  hosp_aug = augment_hosp()

  expect_error(polish(), "dataset of class")
  expect_error(polish("not a data.frame"), "dataset of class")
  expect_error(polish(hosp_aug), "variable of keying")
  expect_error(polish(hosp_aug, subj), "pattern must be provided")
})

test_that("polish handles a two-value pattern schema", {
  hosp_aug = augment_hosp(pattern = "label_2")
  cleaned = polish(data.table::copy(hosp_aug), subj, label_2)

  expect_s3_class(cleaned, "data.table")
})

test_that("polish reports check_NA success when there are no missing values", {
  hosp_aug = augment_hosp()
  output = utils::capture.output(
    polish(data.table::copy(hosp_aug), subj, label_3,
           check_NA = TRUE, verbosity = "summary"),
    type = "message"
 )

  expect_true(any(grepl("no missing values", output)))
})

test_that("polish accepts summary verbosity", {
  hosp_aug = augment_hosp()
  output = utils::capture.output(
    hosp_clean <- polish(
      data.table::copy(hosp_aug), subj, label_3, verbosity = "summary"
   ),
    type = "message"
 )

  expect_true(length(output) > 0L)
  expect_s3_class(hosp_clean, "data.table")
})

#' Synthetic Hospital Admissions
#'
#' A dataset containing synthetic hospital admissions in the classic longitudinal format.
#' The dataset counts imaginary 10 patients who undergo different (re)admission into a hospital.
#' Some demographic and clinical variables are also included.
#'
#' @format A `data.table` with 53 rows and 12 variables:
#'
#' * `subj`: Subject ID (integer).
#' * `adm_number`: Hospital admissions counter (integer).
#' * `gender`: Gender of patient (factor with 2 levels: `"F"` = females,
#'   `"M"` = males).
#' * `age`: Age of patient in years at the given observation (integer).
#' * `rehab`: Rehabilitation flag. If the admission has been in rehabilitation,
#'   then `rehab = 1`; otherwise `rehab = 0` (integer).
#' * `it`: Intensive Therapy flag. If the admission has been in intensive
#'   therapy, then `it = 1`; otherwise `it = 0` (integer).
#' * `rehab_it`: String marking the hospital admission type based on `rehab`
#'   and `it`. The standard admission is coded as `"df"` (default). Admissions
#'   in rehabilitation or intensive therapy are coded as `"rehab"` or `"it"`
#'   (character).
#' * `label_2`: Subject status at the end of the study. It takes 2 values:
#'   `"alive"` and `"dead"` (character).
#' * `label_3`: Subject status at the end of the study. It takes 3 values:
#'   `"alive"`, `"dead_in"`, and `"dead_out"` (character).
#' * `dateIN`: Exact admission date (date).
#' * `dateOUT`: Exact discharge date (date).
#' * `dateCENS`: Either censoring time or exact death time (date).
"hosp"

test_that( "augment warns when t_cens is used as death time", {
  expect_warning(
    augment( test_hosp(), subj, adm_number, label_3, t_start = dateIN,
             t_end = dateOUT, t_cens = dateCENS, verbose = FALSE ),
    "no t_death has been passed"
  )
} )

test_that( "n_events must be an integer", {
  expect_error(
    augment( test_hosp(), subj, !as.integer( adm_number ), label_3,
             t_start = dateIN, t_end = dateOUT, t_cens = dateCENS,
             t_death = dateCENS, verbose = FALSE ),
    "n_events must be an integer"
  )
} )

test_that( "augment validates required inputs and state shape", {
  expect_error( augment(), "dataset" )
  expect_error( augment( data.frame() ), "keying" )
  expect_error( augment( test_hosp(), subj ), "pattern" )
  expect_error(
    augment( test_hosp(), subj, adm_number, label_3, state = list( "IN" ),
             t_start = dateIN, t_end = dateOUT, t_cens = dateCENS,
             verbose = FALSE ),
    "state pattern"
  )
  expect_error(
    augment( test_hosp(), subj, adm_number, label_3, t_end = dateOUT,
             t_cens = dateCENS, verbose = FALSE ),
    "starting and an ending"
  )
  expect_error(
    augment( test_hosp(), subj, adm_number, label_3, t_start = dateIN,
             t_end = dateOUT, verbose = FALSE ),
    "censoring time"
  )
} )

test_that( "augment validates time classes", {
  hosp_bad = test_hosp()
  hosp_bad[ , dateOUT_num := as.numeric( dateOUT ) ]

  expect_warning(
    expect_error(
      augment( hosp_bad, subj, adm_number, label_3, t_start = dateIN,
               t_end = dateOUT_num, t_cens = dateCENS, verbose = FALSE ),
      "same class"
    ),
    "no t_death has been passed"
  )
  expect_error(
    augment( hosp_bad, subj, adm_number, label_3, t_start = dateIN,
             t_end = dateOUT, t_cens = dateCENS, t_death = dateOUT_num,
             verbose = FALSE ),
    "same class"
  )
} )

test_that( "two-state and three-state pattern inputs are equivalent", {
  hosp_aug_2 = augment_hosp( pattern = "label_2" )
  hosp_aug_3 = augment_hosp( pattern = "label_3" )

  expect_identical( hosp_aug_2, hosp_aug_3 )
} )

test_that( "missing n_events is reconstructed from subject order", {
  hosp_aug = augment_hosp()
  hosp_aug_no_events = suppressWarnings(
    augment( test_hosp(), subj, pattern = label_3, t_start = dateIN,
             t_end = dateOUT, t_cens = dateCENS, verbose = FALSE )
  )

  expect_identical( hosp_aug$adm_number, hosp_aug_no_events$n_events )
} )

test_that( "check_NA catches missing values and passes clean data", {
  expect_warning(
    expect_no_error(
      augment( test_hosp(), subj, adm_number, label_3, t_start = dateIN,
               t_end = dateOUT, t_cens = dateCENS, check_NA = TRUE,
               verbose = FALSE )
    ),
    "no t_death has been passed"
  )

  hosp_missing = test_hosp()
  hosp_missing[ 1, dateIN := as.Date( NA ) ]

  expect_output(
    expect_warning(
      expect_error(
        augment( hosp_missing, subj, adm_number, label_3, t_start = dateIN,
                 t_end = dateOUT, t_cens = dateCENS, check_NA = TRUE,
                 verbose = FALSE ),
        "Please, fix"
      ),
      "no t_death has been passed"
    ),
    "dateIN"
  )
} )

test_that( "convert controls the returned data class", {
  aug_dt = augment_hosp()
  aug_df = augment_hosp( convert = TRUE )

  expect_s3_class( aug_dt, "data.table" )
  expect_s3_class( aug_df, "data.frame" )
  expect_false( inherits( aug_df, "data.table" ) )
  expect_identical( as.data.frame( aug_dt ), aug_df )
} )

test_that( "Date inputs create integer augmented time", {
  hosp_aug = augment_hosp( t_augmented = event_time )

  expect_true( "event_time" %in% names( hosp_aug ) )
  expect_true( "event_time_int" %in% names( hosp_aug ) )
  expect_s3_class( hosp_aug$event_time, "Date" )
  expect_type( hosp_aug$event_time_int, "integer" )
} )

test_that( "numeric time inputs keep numeric augmented time", {
  hosp_num = test_hosp()
  hosp_num[ , dateIN_num := as.numeric( dateIN ) ]
  hosp_num[ , dateOUT_num := as.numeric( dateOUT ) ]
  hosp_num[ , dateCENS_num := as.numeric( dateCENS ) ]

  hosp_aug = suppressWarnings(
    augment( hosp_num, subj, adm_number, label_3, t_start = dateIN_num,
             t_end = dateOUT_num, t_cens = dateCENS_num, verbose = FALSE )
  )

  expect_true( "augmented" %in% names( hosp_aug ) )
  expect_false( "augmented_int" %in% names( hosp_aug ) )
  expect_type( hosp_aug$augmented, "double" )
} )

test_that( "integer and factor patterns are accepted", {
  hosp_int = test_hosp()
  hosp_int[ , label_int := data.table::fifelse(
    label_3 == "alive", 0L,
    data.table::fifelse( label_3 == "dead_in", 1L, 2L )
  ) ]
  hosp_factor = test_hosp()
  hosp_factor[ , label_factor := factor( label_3,
                                         levels = c( "alive", "dead_in", "dead_out" ) ) ]

  int_aug = suppressWarnings(
    augment( hosp_int, subj, adm_number, label_int, t_start = dateIN,
             t_end = dateOUT, t_cens = dateCENS, verbose = FALSE )
  )
  factor_aug = suppressWarnings(
    augment( hosp_factor, subj, adm_number, label_factor, t_start = dateIN,
             t_end = dateOUT, t_cens = dateCENS, verbose = FALSE )
  )

  expect_equal( nrow( int_aug ), nrow( factor_aug ) )
  expect_equal( int_aug$status, factor_aug$status )
} )

test_that( "difftime inputs create numeric augmented time", {
  hosp_diff = test_hosp()
  origin = min( hosp_diff$dateIN )
  hosp_diff[ , dateIN_diff := as.difftime( as.numeric( dateIN - origin ),
                                           units = "days" ) ]
  hosp_diff[ , dateOUT_diff := as.difftime( as.numeric( dateOUT - origin ),
                                            units = "days" ) ]
  hosp_diff[ , dateCENS_diff := as.difftime( as.numeric( dateCENS - origin ),
                                             units = "days" ) ]

  hosp_aug = suppressWarnings(
    augment( hosp_diff, subj, adm_number, label_3, t_start = dateIN_diff,
             t_end = dateOUT_diff, t_cens = dateCENS_diff, verbose = FALSE )
  )

  expect_true( "augmented_num" %in% names( hosp_aug ) )
  expect_s3_class( hosp_aug$augmented, "difftime" )
  expect_type( hosp_aug$augmented_num, "double" )
} )

test_that( "supplied t_death avoids the censoring warning", {
  expect_warning(
    augment( test_hosp(), subj, adm_number, label_3, t_start = dateIN,
             t_end = dateOUT, t_cens = dateCENS, t_death = dateCENS,
             verbose = FALSE ),
    NA
  )
} )

test_that( "more_status creates expanded status columns", {
  hosp_aug = augment_hosp( more_status = rehab_it )

  expect_true( all( c( "status_exp", "status_exp_num", "n_status_exp" ) %in%
                      names( hosp_aug ) ) )
  expect_false( anyNA( hosp_aug$status_exp ) )
  expect_false( anyNA( hosp_aug$status_exp_num ) )
  expect_false( anyNA( hosp_aug$n_status_exp ) )
} )

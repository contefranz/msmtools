# TESTING ERRORS ------------------------------------------------------------------------------

data( hosp )

test_that( 'a warning is printed when t_cens is the only var used',
           expect_warning( augment( hosp, subj, adm_number, label_3,
                                    t_start = dateIN, t_end = dateOUT, t_cens = dateCENS,
                                    verbose = F ) ) )

test_that( 'n_events must be an integer',
           expect_error( augment( hosp, subj, !as.integer( adm_number ), label_3,
                                  t_start = dateIN, t_end = dateOUT, t_cens = dateCENS,
                                  t_death = dateCENS,
                                  verbose = F ) ) )

# # TESTING 2 OR 3 VALUES FOR PATTERN -----------------------------------------------------------

hosp_aug_2 = augment( data = hosp, data_key = subj, n_events = adm_number,
                          pattern = label_2, t_start = dateIN, t_end = dateOUT,
                          t_cens = dateCENS, verbose = F )
hosp_aug_3 = augment( data = hosp, data_key = subj, n_events = adm_number,
                      pattern = label_3, t_start = dateIN, t_end = dateOUT,
                      t_cens = dateCENS, verbose = F )

test_that( "passing to pattern a var with 2 or 3 values is identical",
           expect_identical( hosp_aug_2, hosp_aug_3 ) )

# # TESTING IF N_EVENTS IS MISSING --------------------------------------------------------------
hosp_aug = augment( hosp, subj, adm_number, label_3,
                    t_start = dateIN, t_end = dateOUT, t_cens = dateCENS, verbose = F )
hosp_aug_no_events = augment( hosp, subj, pattern = label_3,
                              t_start = dateIN, t_end = dateOUT, t_cens = dateCENS, verbose = F )

test_that( "adm_number is identical to what augment created",
           expect_identical( hosp_aug$adm_number, hosp_aug_no_events$n_events ) )


# TEST RETURNED CLASSES -----------------------------------------------------------------------
aug.dt = augment( hosp, subj, adm_number, label_3,
                  t_start = dateIN, t_end = dateOUT, t_cens = dateCENS,
                  verbose = F )

aug.df = augment( hosp, subj, adm_number, label_3,
                  t_start = dateIN, t_end = dateOUT, t_cens = dateCENS,
                  verbose = F, convert = F )

test_that( "returning a data.table when convert = FALSE",
           expect_is( aug.dt, "data.table" ) )
test_that( "returning a data.frame when convert = TRUE",
           expect_is( aug.df, "data.frame" ) )

test_that( "objects are identical",
           expect_identical( aug.df, aug.dt ) )



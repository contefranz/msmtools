test_that( "polish leaves data unchanged when no duplicate transition exists", {
  hosp_aug = augment_hosp()
  hosp_clean = polish( data.table::copy( hosp_aug ), subj, label_3,
                       verbose = FALSE )

  expect_equal( as.data.frame( hosp_clean ), as.data.frame( hosp_aug ) )
} )

test_that( "polish removes subjects with duplicate transition times", {
  hosp_aug = augment_hosp()
  duplicate_input = data.table::copy( hosp_aug )
  rows = which( duplicate_input$subj == 1 & duplicate_input$status != "DEAD" )
  duplicate_input[ rows[ 2 ], augmented := duplicate_input[ rows[ 1 ], augmented ] ]
  duplicate_input[ rows[ 2 ], augmented_int := duplicate_input[ rows[ 1 ], augmented_int ] ]

  hosp_clean = polish( duplicate_input, subj, label_3, verbose = FALSE )

  expect_false( 1 %in% hosp_clean$subj )
  expect_lt( nrow( hosp_clean ), nrow( hosp_aug ) )
  expect_equal( data.table::uniqueN( hosp_clean$subj ), 9L )
} )

test_that( "polish can return a data.frame", {
  hosp_aug = augment_hosp()
  hosp_clean = polish( data.table::copy( hosp_aug ), subj, label_3,
                       convert = TRUE, verbose = FALSE )

  expect_s3_class( hosp_clean, "data.frame" )
  expect_false( inherits( hosp_clean, "data.table" ) )
} )

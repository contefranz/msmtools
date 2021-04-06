## ----ninja, echo = FALSE-------------------------------------------------
library( msmtools )

## ----long example, collapse = TRUE---------------------------------------
data( hosp )
hosp[ 1:17, .( subj, adm_number, gender, age, label_2, 
               dateIN, dateOUT, dateCENS ) ]

## ----running augment, collapse = TRUE, eval = F--------------------------
#  hosp_augmented = augment( data = hosp, data_key = subj,
#                            n_events = adm_number, pattern = label_3,
#                            t_start = dateIN, t_end = dateOUT,
#                            t_cens = dateCENS, verbose = FALSE )
#  
#  hosp_augmented[ 1:35, .( subj, adm_number, gender, age, label_2,
#                           augmented, status, n_status ) ]

## ----subsetting, collapse = TRUE-----------------------------------------
hosp[ 18:28, .( subj, adm_number, rehab, it, rehab_it,
                dateIN, dateOUT, dateCENS ) ]


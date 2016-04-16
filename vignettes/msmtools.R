## ----echo = FALSE--------------------------------------------------------
library( msmtools )
data( hosp )
test = hosp[ 1:17, .( subj, adm_number, gender, age, label_2, dateIN, dateOUT, dateCENS ) ]
head( test, 17 )

## ----echo = FALSE--------------------------------------------------------
library( msmtools )
data( hosp )
hosp_augmented = augment( data = hosp, data_key = subj, n_events = adm_number, pattern = label_3,
t_start = dateIN, t_end = dateOUT, t_cens = dateCENS, verbose = F )
head( hosp_augmented[ , .( subj, adm_number, gender,
age, label_2, augmented, status, n_status ) ], 35 )


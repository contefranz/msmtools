## ----echo = FALSE--------------------------------------------------------
library( msmtools )
data( hosp )
test = hosp[ 1:17, .( subj, adm_number, gender, age, label_2, dateIN, dateOUT, dateCENS ) ]
test

## ----echo = FALSE--------------------------------------------------------
library( msmtools )
data( hosp )
hosp_augmented = augment( data = hosp, data_key = subj, n_events = adm_number, pattern = label_3,
t_start = dateIN, t_end = dateOUT, t_cens = dateCENS, verbose = F )
head( hosp_augmented[ , .( subj, adm_number, gender,
age, label_2, augmented, status, n_status ) ], 35 )

## ------------------------------------------------------------------------
data( hosp )
hosp[ 18:28, .( subj, adm_number, rehab, it, rehab_it,
                dateIN, dateOUT, dateCENS ) ]

## ------------------------------------------------------------------------
hosp_augmented = augment( data = hosp, data_key = subj,
                          n_events = adm_number, pattern = label_2,
                          t_start = dateIN, t_end = dateOUT,
                          t_cens = dateCENS, more_status = rehab_it,
                          verbose = FALSE )
hosp_augmented[ 36:60, .( subj, adm_number, rehab_it,
                          augmented, status, status_exp, n_status_exp ) ]

## ------------------------------------------------------------------------
hosp_augmented = augment( data = hosp, data_key = subj,
                          n_events = adm_number, pattern = label_2,
                          t_start = dateIN, t_end = dateOUT,
                          t_cens = dateCENS, verbose = FALSE )

# let's define the initial transition matrix for our model
Qmat = matrix( data = 0, nrow = 3, ncol = 3, byrow = TRUE ) 
Qmat[ 1, 1:3 ] = 1
Qmat[ 2, 1:3 ] = 1
colnames( Qmat ) = c( 'IN', 'OUT', 'D' )
rownames( Qmat ) = c( 'IN', 'OUT', 'D' ) 
Qmat

# attaching the msm package and running the model using gender, age, # rehab and it as covariates
library( msm )
msm_model = msm( status_num ~ augmented_int,
                 subject = subj, data = hosp_augmented,
                 covariates = ~ gender + age + rehab + it,
                 exacttimes = TRUE, gen.inits = TRUE,
                 qmatrix = Qmat, method = 'BFGS',
                 control = list( fnscale = 6e+05, trace = 0, 
                                 REPORT = 1, maxit = 10000 ) )


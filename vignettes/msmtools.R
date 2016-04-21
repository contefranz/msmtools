## ----ninja, echo = FALSE-------------------------------------------------
library( msmtools )

## ----long example, collapse = TRUE---------------------------------------
data( hosp )
hosp[ 1:17, .( subj, adm_number, gender, age, label_2, 
               dateIN, dateOUT, dateCENS ) ]

## ----running augment, collapse = TRUE------------------------------------
hosp_augmented = augment( data = hosp, data_key = subj, 
                          n_events = adm_number, pattern = label_3,
                          t_start = dateIN, t_end = dateOUT, 
                          t_cens = dateCENS, verbose = FALSE )

hosp_augmented[ 1:35, .( subj, adm_number, gender, age, label_2, 
                         augmented, status, n_status ) ]

## ----subsetting, collapse = TRUE-----------------------------------------
hosp[ 18:28, .( subj, adm_number, rehab, it, rehab_it,
                dateIN, dateOUT, dateCENS ) ]

## ----complex status, collapse = TRUE-------------------------------------
hosp_augmented = augment( data = hosp, data_key = subj,
                          n_events = adm_number, pattern = label_2,
                          t_start = dateIN, t_end = dateOUT,
                          t_cens = dateCENS, more_status = rehab_it,
                          verbose = FALSE )

hosp_augmented[ 36:60, .( subj, adm_number, rehab_it,
                          augmented, status, status_exp, n_status_exp ) ]

## ----multistate model, collapse = TRUE-----------------------------------
hosp_augmented = augment( data = hosp, data_key = subj,
                          n_events = adm_number, pattern = label_2,
                          t_start = dateIN, t_end = dateOUT,
                          t_cens = dateCENS, verbose = FALSE )

# let's define the initial transition matrix for our model
Qmat = matrix( data = 0, nrow = 3, ncol = 3, byrow = TRUE )
Qmat[ 1, 1:3 ] = 1
Qmat[ 2, 1:3 ] = 1
colnames( Qmat ) = c( 'IN', 'OUT', 'DEAD' )
rownames( Qmat ) = c( 'IN', 'OUT', 'DEAD' )
Qmat

# attaching the msm package and running the model using
# gender and age as covariates
library( msm )
msm_model = msm( status_num ~ augmented_int,
                 subject = subj, data = hosp_augmented,
                 covariates = ~ gender + age,
                 exacttimes = TRUE, gen.inits = TRUE,
                 qmatrix = Qmat, method = 'BFGS',
                 control = list( fnscale = 6e+05, trace = 0,
                                 REPORT = 1, maxit = 10000 ) )

## ----survplot 1, fig.align = 'center', fig.width = 5, fig.height = 4-----
survplot( msm_model, km = TRUE, ci = 'none',
          verbose = FALSE, devnew = FALSE )

## ----survplot 2, fig.align = 'center', fig.width = 5, fig.height = 4-----
survplot( msm_model, km = TRUE, from = 2, ci = 'none',
          verbose = FALSE, devnew = FALSE )

## ----custom time seq, fig.align = 'center', fig.width = 5, fig.height = 4----
time_seq = seq( 300, 800, by = 30 )
survplot( msm_model, times = time_seq, ci = 'none',
          verbose = FALSE, devnew = FALSE )

## ----returnKM, collapse = TRUE, fig.align = 'center', fig.width = 5, fig.height = 4----
survplot( msm_model, ci = 'none', return.km = TRUE,
          verbose = FALSE, do.plot = FALSE )

## ----returnKM2, collapse = TRUE------------------------------------------
# running survplot() and assigning it to an object
km_data = survplot( msm_model, ci = 'none', return.km = TRUE,
                    verbose = FALSE, do.plot = FALSE )

# let's see the dataset
km_data

## ----returnP, collapse = TRUE--------------------------------------------
survplot( msm_model, ci = 'none', return.p = TRUE,
          verbose = FALSE, do.plot = FALSE )

## ----returnP2, collapse = TRUE, fig.align = 'center', fig.width = 5, fig.height = 4----
# running survplot() and assigning it to an object
fitted_data = survplot( msm_model, ci = 'none', return.p = TRUE,
                        verbose = FALSE, do.plot = FALSE )

# let's see the dataset
fitted_data

## ----return_all, collapse = TRUE, fig.align = 'center', fig.width = 5, fig.height = 4----
# just running survplot()
survplot( msm_model, ci = 'none',
                     return.km = TRUE, return.p = TRUE,
                     verbose = FALSE, do.plot = FALSE )

# running survplot() and assigning it to an object
all_data = survplot( msm_model, ci = 'none',
                     return.km = TRUE, return.p = TRUE,
                     verbose = FALSE, do.plot = FALSE )

# let's see the dataset
all_data

## ----splitting data, collapse = TRUE-------------------------------------
# do not extract data using just one [].
# This keeps the class, so it returns a list
km_data_wrong = all_data[ 1 ]
# extracting data using the list way so be careful to use double []
km_data_1 = all_data[[ 1 ]]
# extracting data using the '$' access operator
km_data_2 = all_data$km
identical( km_data_wrong, km_data_1 )
identical( km_data_1, km_data_2 )
km_data_1

fitted_data_1 = all_data[[ 2 ]]
fitted_data_2 = all_data$fitted
identical( fitted_data_1, fitted_data_2 )
fitted_data_1

## ----prev, collapse = TRUE, fig.align = 'center', fig.width = 7, fig.height = 3----
# defining the times at which compute the prevalences
t_min = min( hosp_augmented$augmented_int )
t_max = max( hosp_augmented$augmented_int )
steps = 100L

# computing prevalences
prev = prevalence.msm( msm_model, covariates = 'mean', ci = 'normal',
                            times = seq( t_min, t_max, steps ) )

# and plotting them using prevplot()
prevplot( msm_model, prev, ci = TRUE, devnew = F )

## ----plot_M, collapse = TRUE, fig.align = 'center', fig.width = 7, fig.height = 3----
prevplot( msm_model, prev, M = TRUE, ci = TRUE, devnew = F )


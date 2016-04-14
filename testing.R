library( devtools )

data( hosp )
prova = augment( data = hosp, data_key = subj, n_events = adm_number,
                 pattern = label_3,
                 t_start = dateIN, t_end = dateOUT, t_cens = dateCENS )




setwd( '~/Dropbox/PhD/MoxOff/anonimi/WeWorld/' )

load( 'msm_1_adop_100_donors.RData' )

covariates = 'mean'
ci = 'none'
quartz()
survplot( msm_100, km = T, from = 1, to = 3, print.res = F,
          covariates = covariates, ci = ci, interp = 'start',
          return.km = T)

library(devtools)
library( data.table )
library( msmtools )

data( hosp )
prova = augment( data = hosp, data_key = subj, n_events = adm_number,
                 pattern = label_3,
                 t_start = dateIN, t_end = dateOUT, t_cens = dateCENS )



pippo = function( DF, pattern, value )
{
  if( 1 == 1 )
  {
    print( substitute( expression( DF[ get( pattern ) == value ] ) ) )

    DF[ get( pattern ) == value  ]
  }
}

assign( 'pippo', pippo, envir = parent.env( environment() ) )

pippo( hosp, 'label_3', 'alive' )




match1 = data[ get( pattern, envir = as.environment( data ) ) == values[ 1 ], .SD[ .N ], by = eval( cols[[ 1 ]] ) ]

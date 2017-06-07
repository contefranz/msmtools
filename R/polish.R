if ( getRversion() >= "2.15.1" ) {
  utils::globalVariables( c( "index" ) )
}
#' Remove observations with different states occurring at the same time
#'
#' Fast algorithm to get rid of transitions to different states occurring at
#' the same exact time in an augmented data structure as computed by
#' \code{augment} (see 'Details').
#'
#' @inheritParams augment
#' @param time The target time variable to check duplicates. By default it is set
#' to 'augmented_int'.
#' @param check_NA If \code{TRUE}, then arguments \code{data_key},
#' \code{pattern}, and \code{time} are looked up for any missing data and if
#' the function finds any, it stops with error. Default is \code{FALSE}.
#'
#' @details The function finds all those cases where two subsequent events for
#' a given subject land on different states but occur at the same time.
#' When this happens, the whole subject, as identified by \code{data_key}, is
#' removed from the data. The total number of subjects to be removed is
#' printed out in order to be more informative.
#'
#' @seealso \code{\link[msmtools]{augment}}
#'
#' @examples
#'
#' # loading data
#' data( hosp )
#'
#' # augmenting longitudinal data
#' hosp_aug = augment( data = hosp, data_key = subj, n_events = adm_number,
#'                     pattern = label_3, t_start = dateIN, t_end = dateOUT,
#'                     t_cens = dateCENS )
#'
#' # cleaning any targeted occurrence
#' hosp_aug_clean = polish( data = hosp_aug, data_key = subj, pattern = label_3 )
#'
#' @author Francesco Grossetti \email{francesco.grossetti@@unibocconi.it}.
#' @import data.table
#' @export

polish = function( data, data_key, pattern, time,
                   check_NA = FALSE, convert = FALSE, verbose = TRUE ) {

  tic = proc.time()

  if ( missing( data ) ) {
    stop( 'a dataset of class data.table or data.frame must be provided' )
  }
  if ( !inherits( data, "data.table" ) && !inherits( data, "data.frame" ) ) {
    stop( "a dataset of class data.table or data.frame must be provided" )
  }
  if ( missing( data_key ) ) {
    stop( 'a variable of keying must be provided' )
  }
  if ( missing( pattern ) ) {
    stop( "a pattern must be provided" )
  }
  if ( inherits( data, 'data.frame' ) ) {
    setDT( data )
  }
  if ( verbose ) {
    cat( '-------------------------------------\n' )
    cat( '# # # # setting everything up # # # #\n' )
    cat( '-------------------------------------\n' )
  }

  setkey( data, NULL )
  cols = as.character( substitute( list( data_key ) )[ -1L ] )
  if ( !length( cols ) ) {
    cols = colnames( data )
  }
  setkeyv( data, cols )
  pattern = as.character( substitute( list( pattern ) )[ -1L ] )
  if ( missing( time ) ) {
    if ( "augmented_int" %in% names( data ) ) {
      if ( verbose ) {
        cat( "augmented_int set as time variable\n" )
        cat( "---\n" )
      }
      time = 'augmented_int'
    } else if ( "augmented_num" %in% names( data ) ) {
      if ( verbose ) {
        cat( "augmented_num set as time variable\n" )
        cat( "---\n" )
      }
      time = 'augmented_num'
    }
  } else {
    time = as.character( substitute( list( time ) )[ -1L ] )
  }

  if ( check_NA == TRUE ) {
    if ( verbose ) {
      message( 'checking for any missing values in function arguments' )
    }
    checks = c( cols, pattern, time )
    test = apply( data[ , checks, with = FALSE ], 2,
                  function( x ) any( sum( is.na( x ) ) > 0 ) )
    if ( any ( test ) ) {
      cat( '---\n' )
      if ( verbose ) {
        message( 'detected missing values in the following variables:' )
      }
      invisible( sapply( names( test[ test == TRUE ] ),
                         function( x ) cat( x, '\n' ) ) )
      stop( 'Please, fix the issues and relaunch shiver()' )
    } else {
      cat( 'Ok, no missing values detected\n')
      cat( '---\n' )
    }
  }

  data[ , index := sequence( .N ) ]
  n_patients = uniqueN( eval( substitute( data$cols ) ) )
  values = sort( eval( substitute( unique( data$pattern ) ) ) )
  if ( length( values ) < 2 ) {
    stop( 'unit identification label must be an integer,
          a factor or a character with at least 2 elements' )
  }

  alive = data[ get( pattern ) == values[ 1 ] ]
  alive.last = alive[ alive[ , .I[ .N ], by = eval( cols ) ]$V1 ]
  setkey( alive.last, index )
  setkey( alive, index )
  alive.no.last = alive[ !alive.last ]

  if ( verbose ) {
    message( 'checking ', substitute( pattern ), ' and defining patterns' )
  }
  if ( length( values ) == 2 ) {
    if ( verbose ) {
      cat( 'detected only 2 values\n' )
      cat( '---\n' )
    }
    dead = data[ get( pattern ) == values[ 2 ] ]
  } else if ( length( values ) == 3 ) {
    if ( verbose ) {
      cat( 'Ok, detected 3 values\n' )
      cat( '---\n' )
    }
    dead = data[ get( pattern ) != values[ 1 ] ]
  }

  l = list( alive.no.last, dead )
  data.no.last.event = rbindlist( l )
  row.duplicated = duplicated( data.no.last.event,
                               by = c( eval( cols ), eval( time ) ) )
  duplicated = data.no.last.event[ row.duplicated == TRUE ]
  n_duplicated = uniqueN( eval( substitute( duplicated$cols ) ) )
  setkeyv( duplicated, cols )

  if ( n_duplicated == 0 ) {
    if ( verbose ) {
      cat( 'Hurray! No duplicated occurrences have been found in ',
           substitute( data ), ' according to variable ',
           substitute( time ), "\n", sep = "" )
    }
  } else {
    if ( verbose ) {
      message( 'Spotted ', n_duplicated,
               ' patients with at least a duplicated occurrence according to variable ',
               substitute( time ) )
    }
    data.clean = data[ !duplicated ]
    n_patients.to.keep = uniqueN( eval( substitute( data.clean$cols ) ) )
    if ( verbose ) {
      cat( n_patients.to.keep, ' patients have been reained corresponding to ',
           round( 100 * ( n_patients.to.keep / n_patients ), 2 ), '%\n', sep = '' )
      cat( 'Duplicated patients have been sucessfully removed\n' )
    }
  }

  data[ , index := NULL ]
  if ( n_duplicated > 0 ) {
    data.clean[ , index := NULL ]
  }
  toc = proc.time()
  time = toc - tic
  if ( verbose ) {
    cat( '---------------------------\n' )
    cat( 'polish() took:', time[ 3 ], 'sec. \n', sep = ' ' )
    cat( '---------------------------\n' )
  }

  if ( n_duplicated == 0 ) {
    if ( convert == TRUE ) {
      setDF( data )
      return( data )
    }
    data[]
    return( data )
  } else {
    if ( convert == TRUE ) {
      setDF( data.clean )
      return( data.clean )
    }
    data.clean[]
    return( data.clean )
  }
}

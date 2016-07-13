#' Delete different events occurring at the same time
#'
#' Fast algorithm to get rid of transitions to different states occurring at the same exact time
#' when dealing with augmented data as computed by \code{augment}.
#'
#' @param data An augmented \code{data.table} or \code{data.frame} object where each row
#' represents a transition. If \code{data} is a \code{data.frame}, then \code{quake} internally
#' casts it to a \code{data.table}.
#' @param data_key A keying variable which \code{quake} uses to define a key for \code{data}.
#' This represents the subject ID (See \code{\link[data.table]{setkey}}).
#' @param pattern ID status at the end of the study as passed to \code{augment} (See
#' \code{\link[msmtools]{augment}}).
#' @param target The target variable to check duplicates. By default it is set to 'augmented_int'.
#' @param check_NA If \code{TRUE}, then arguments \code{data_key}, \code{pattern},
#' and \code{target} are looked up for any missing data and if the function finds
#' any, it stops with error. Default is \code{FALSE}.
#' @param verbose If \code{FALSE}, all information produced by \code{print}, \code{cat} and
#' \code{message} are suppressed. All is done internally so that no global
#' options are changed. \code{verbose} can be set to \code{FALSE} on all common OS
#' (see also \code{\link[base]{sink}} and \code{\link[base]{options}}). Default is \code{TRUE}.
#'
#' @details blablabla some details to write down
#' @seealso \code{\link[msmtools]{augment}}
#'
#' @examples
#' data( hosp )
#' hosp_aug = augment( data = hosp, data_key = subj, n_events = adm_number, pattern = label_3,
#'                     t_start = dateIN, t_end = dateOUT, t_cens = dateCENS )
#' hosp_aug_clean = quake( data = hosp_aug, data_key = subj, pattern = label_3 )
#'
#' @author Francesco Grossetti \email{francesco.grossetti@@polimi.it}.
#' @import data.table
#' @export

quake = function( data, data_key, pattern, target, check_NA = FALSE, verbose = TRUE ) {

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
  if ( verbose == TRUE ) {
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
  if ( missing( target ) ) {
    target = 'augmented_int'
  } else {
    target = as.character( substitute( list( target ) )[ -1L ] )
  }

  if ( check_NA == TRUE ) {
    if ( verbose == TRUE ) {
      message( 'checking for any missing values in function arguments' )
    }
    checks = c( cols, pattern, target )
    test = apply( data[ , checks, with = FALSE ], 2, function( x ) any( sum( is.na( x ) ) > 0 ) )
    if ( any ( test ) ) {
      cat( '---\n' )
      if ( verbose == TRUE ) {
        message( 'detected missing values in the following variables:' )
      }
      invisible( sapply( names( test[ test == TRUE ] ), function( x ) cat( x, '\n' ) ) )
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
    stop( 'unit identification label must be an integer, a factor or a character
          with at least 2 elements' )
  }

  alive = data[ get( pattern ) == values[ 1 ] ]
  alive.last = alive[ alive[ , .I[ .N ], by = eval( cols ) ]$V1 ]
  setkey( alive.last, index )
  setkey( alive, index )
  alive.no.last = alive[ !alive.last ]

  if ( verbose == TRUE ) {
    message( 'checking ', substitute( pattern ), ' and defining patterns' )
  }
  if ( length( values ) == 2 ) {
    cat( 'detected only 2 values\n' )
    cat( '---\n' )
    dead = data[ get( pattern ) == values[ 2 ] ]
  } else if ( length( values ) == 3 ) {
    cat( 'Ok, detected 3 values\n' )
    dead = data[ get( pattern ) != values[ 1 ] ]
    cat( '---\n' )
  }

  l = list( alive.no.last, dead )
  data.no.last.event = rbindlist( l )
  row.duplicated = duplicated( data.no.last.event, by = c( eval( cols ), eval( target ) ) )
  duplicated = data.no.last.event[ row.duplicated == TRUE ]
  n_duplicated = uniqueN( eval( substitute( duplicated$cols ) ) )
  setkeyv( duplicated, cols )

  if ( n_duplicated == 0 ) {
    message( 'Hurray! No duplicated occurrences have been found in ', substitute( data ),
             ' according to variable ', substitute( target ) )
  } else {
    message( 'Spotted ', n_duplicated,
             ' patients with at least a duplicated occurrence according to variable ',
             substitute( target ) )
    data.clean = data[ !duplicated ]
    n_patients.to.keep = uniqueN( eval( substitute( data.clean$cols ) ) )
    cat( n_patients.to.keep, ' patients have been reained corresponding to ',
         round( 100 * ( n_patients.to.keep / n_patients ), 2 ), '%\n', sep = '' )
    cat( 'Duplicated patients have been sucessfully removed\n' )
  }

  data[ , index := NULL ]
  if ( n_duplicated > 0 ) {
    data.clean[ , index := NULL ]
  }
  toc = proc.time()
  time = toc - tic
  cat( '---------------------------\n' )
  cat( 'quake() took:', time[ 3 ], 'sec. \n', sep = ' ' )
  cat( '---------------------------\n' )

  if ( n_duplicated == 0 ) {
    return( invisible( data ) )
  } else {
    return( invisible( data.clean ) )
  }
}

#' A starting function which does something
#'
#' Figuring out what actually this function does
#'
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
  target = as.character( substitute( list( target ) )[ -1L ] )

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
    message( 'No duplicated occurrences have been found ', substitute( data ),
             'according to variable ', substitute( target ), '\n' )
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
  toc = proc.time()
  time = toc - tic
  cat( '---------------------------\n' )
  cat( 'shiver() took:', time[ 3 ], 'sec. \n', sep = ' ' )
  cat( '---------------------------\n' )

  return( invisible( data.clean ) )

}

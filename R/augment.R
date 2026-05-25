if ( getRversion() >= "2.15.1" ) {
  utils::globalVariables( c( "status", "status_num", "n_status",
                             "status_exp", "status_exp_num", "n_status_exp",
                             ":=", ".", ".I", ".N", ".SD", "N", "V2" ) )
}
#' Build augmented transition data
#'
#' Reshape standard longitudinal data into augmented transition data suitable
#' for multi-state models fitted with **msm**.
#'
#' @param data A `data.table` or `data.frame` object in longitudinal
#' format where each row represents an observation with known start and end
#' times. If `data` is a `data.frame`, `augment()` internally casts it to a
#' `data.table`.
#' @param data_key A keying variable used to identify subjects and define a key
#' for `data` (see [data.table::setkey()]).
#' @param n_events An integer variable indicating the progressive (monotonic)
#' event number for each subject. `augment()` checks whether `n_events` is
#' monotonically increasing within each `data_key` and stops if the check fails
#' (see Details). If missing, `augment()` creates a variable named `"n_events"`.
#' @param pattern Either an integer, a factor or a character with 2 or 3 unique
#' values that gives each subject's status at the end of the study. `pattern`
#' has a predefined structure. When 2 values are detected, they must be in the
#' format: 0 = "alive", 1 = "dead". When 3 values are detected, then the format
#' must be: 0 = "alive", 1 = "dead during a transition", 2 = "dead after a
#' transition has ended" (see Details).
#' @param state A list of exactly three possible states that a subject can
#' reach. `state` has a predefined structure: `IN`, `OUT`, `DEAD`
#' (see Details).
#' @param t_start The starting time of an observation. It can be passed as date,
#' integer, or numeric format.
#' @param t_end The ending time of an observation. It can be passed as date,
#' integer, or numeric format.
#' @param t_cens The censoring time of the study. This is the date until each
#' ID is observed, if still active in the cohort.
#' @param t_death The exact death time of a subject ID. If `t_death` is
#' missing, `t_cens` is assumed to contain both censoring and death times
#' and a warning is raised.
#' @param t_augmented A variable indicating the name of the new time variable
#' in the augmented format. If `t_augmented` is missing, the default name
#' `"augmented"` is used and the new variable is added to `data`. When
#' `t_start` is a date or difftime, `augment()` also creates an integer or
#' numeric companion variable. The suffix `"_int"` or `"_num"` is added to
#' `t_augmented` accordingly. This is needed because **msm** does not handle
#' date or difftime variables directly. Both variables are positioned before
#' `t_start`.
#' @param more_status A variable that marks further transitions beyond the
#' default ones given by `state`. `more_status` can be a factor or character
#' (see Details). If missing, `augment()` ignores it.
#' @param check_NA If `TRUE`, `data_key`, `n_events`, `pattern`, `t_start`, and
#' `t_end` are checked for missing values. If any missing values are found, the
#' function stops with an error. Default is `FALSE` because `augment()` is not
#' intended for general consistency checks and the scan can add memory overhead
#' on very large datasets. `more_status` is always checked for missing values
#' when supplied.
#' @param convert If `TRUE`, the returned object is automatically
#' converted to the class `data.frame`. This is done in place and comes
#' at very low runtime and memory cost (see [data.table::setDF()]).
#' @param verbose If `FALSE`, output produced by `print()`, `cat()`, and
#' `message()` is suppressed. Default is `TRUE`.
#' @details `augment()` requires a monotonic event sequence within each subject.
#' The data are ordered with [data.table::setkey()] using `data_key` as the
#' primary key and `t_start` as the secondary key. The function then checks the
#' monotonicity of `n_events`; if the check fails, it stops and reports the
#' subjects that violate the condition. If `n_events` is missing, `augment()`
#' first computes a progression number named *n_events* and then runs the same
#' check.
#'
#' Argument `pattern` must follow the expected ordering. With two statuses,
#' values must correspond to `0 = "alive"` and `1 = "dead"`. With three
#' statuses, integer values must correspond to `0 = "alive"`,
#' `1 = "dead inside a transition"`, and
#' `2 = "dead outside a transition"`. Character and factor values must follow
#' the same order. For example, `0` cannot be used to indicate death.
#'
#' The order of `state` also matters. The first element is the state at
#' `t_start` (for example, `"IN"`), the second element is the state at `t_end`
#' (for example, `"OUT"`), and the third element is the absorbing state (for
#' example, `"DEAD"`).
#'
#' `more_status` lets `augment()` represent transitions beyond the defaults in
#' `state`. Standard admissions that add no extra information should use `"df"`
#' for "default" (see Examples, or run `?hosp` and inspect `rehab_it`). More
#' complex transitions should use concise, self-explanatory labels.
#'
#' @return An augmented dataset of class `data.table`, or `data.frame` when
#' `convert = TRUE`. Each row represents a specific transition for a given
#' subject. `augment()` computes the following key variables:
#'
#' * `augmented`: The transition time variable. If `t_augmented` is missing,
#'   `augment()` creates *augmented* by default. The variable is built from
#'   `t_start` and `t_end` and inherits their class. If `t_start` is a date,
#'   `augment()` also creates an integer variable named *augmented_int*. If
#'   `t_start` is a difftime, it creates a numeric variable named
#'   *augmented_num*.
#' * `status`: A status flag that contains the states as specified in `state`.
#'   `augment()` automatically checks whether argument `pattern` has 2 or 3
#'   unique values and computes the correct structure of a given subject as
#'   reported in the vignette. The variable is cast as character.
#' * `status_num`: The corresponding integer version of *status*.
#' * `n_status`: A mix of `status` and `n_events` cast as character. This is
#'   useful when modelling process progression.
#'
#' If `more_status` is passed, `augment()` computes additional variables.
#' They mirror the meaning of *status*, *status_num*, and *n_status* but they
#' account for the more complex structure defined. They are: `status_exp`,
#' `status_exp_num`, and `n_status_exp`.
#'
#' @examples
#' # loading data
#' data( hosp )
#'
#' # 1.
#' # augmenting hosp
#' hosp_augmented = augment( data = hosp, data_key = subj, n_events = adm_number,
#'                           pattern = label_3, t_start = dateIN, t_end = dateOUT,
#'                           t_cens = dateCENS )
#'
#' # 2.
#' # augmenting hosp by passing more information regarding transitions
#' # with argument more_status
#' hosp_augmented_more = augment( data = hosp, data_key = subj, n_events = adm_number,
#'                                pattern = label_3, t_start = dateIN, t_end = dateOUT,
#'                                t_cens = dateCENS, more_status = rehab_it )
#' # 3.
#' # augmenting hosp and returning a data.frame
#' hosp_augmented = augment( data = hosp, data_key = subj, n_events = adm_number,
#'                           pattern = label_3, t_start = dateIN, t_end = dateOUT,
#'                           t_cens = dateCENS, convert = TRUE )
#' class( hosp_augmented )
#'
#' @references Jackson, C.H. (2011). Multi-State Models for Panel Data: The
#' **msm** Package for R. Journal of Statistical Software, 38(8), 1-29.
#' <https://www.jstatsoft.org/v38/i08/>.
#'
#' M. Dowle, A. Srinivasan, T. Short, S. Lianoglou with contributions from
#' R. Saporta and E. Antonyan (2016): **data.table**: Extension of `data.frame`.
#' R package version 1.9.6. <https://github.com/Rdatatable/data.table/wiki>
#'
#' @seealso [data.table::data.table()], [data.table::setkey()]
#' @author Francesco Grossetti <francesco.grossetti@unibocconi.it>.
#' @importFrom data.table setDT setDF setkey setkeyv rbindlist uniqueN setcolorder
#' @export

augment = function( data, data_key, n_events, pattern,
                    state = list ( 'IN', 'OUT', 'DEAD' ),
                    t_start, t_end, t_cens, t_death, t_augmented,
                    more_status, check_NA = FALSE, convert = FALSE,
                    verbose = TRUE ) {

  tic = proc.time()
  oldw = getOption( "warn" )
  if ( verbose ) {
    options( warn = 1 )
  }
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
  if ( !inherits( state, "list" ) || length( state ) != 3 ) {
    stop( "state pattern must be a list of 3 elements" )
  }
  if ( missing( t_start ) || missing( t_end ) ) {
    stop( 'a starting and an ending event times must be provided' )
  }
  if ( missing( t_cens ) ) {
    stop( 'a censoring time must be provided' )
  }
  if ( missing( t_death ) ) {
    warning( 'no t_death has been passed. Assuming that ', deparse( substitute( t_cens ) ),
             ' contains both censoring and death times' )
  }
  if ( inherits( data, 'data.frame' ) ) {
    setDT( data )
  }
  if ( verbose ) {
    cat( '-------------------------------------\n' )
    cat( '# # # # setting everything up # # # #\n' )
    cat( '-------------------------------------\n' )
  }
  pattern = as.character( substitute( pattern ) )
  t_start = as.character( substitute( t_start ) )
  t_end   = as.character( substitute( t_end ) )
  t_cens  = as.character( substitute( t_cens ) )

  if ( class( data[[ t_start ]] ) != class( data[[ t_end ]] ) ) {
    stop( 'the starting and the ending event times must be of the same class' )
  } else if ( class( data[[ t_start ]] ) != class( data[[ t_cens ]] ) ) {
    stop( 'the starting and the censoring event times must be of the same class' )
  }
  setkey( data, NULL )
  if ( !missing( n_events ) ) {
    cols = c( as.character( substitute( data_key ) ), as.character( substitute( n_events ) ) )
    if ( !length( cols ) )
      cols = colnames( data )
    if ( !inherits( data[[ cols[[ 2 ]] ]], "integer" ) ) {
      stop( 'n_events must be an integer' )
    }
    if ( verbose ) {
      message( 'checking monotonicity of ', cols[[ 2 ]] )
    }
    ev = data[ , .( ev = all( get( cols[[ 2 ]] ) == cummax( get( cols[[ 2 ]] ) ) ) ),
               by = eval( cols[[ 1 ]] ) ]
    setkeyv( data, c( cols[[ 1 ]], t_start ) )
    if ( all( ev$ev ) == FALSE ) {
      if ( verbose ) {
        message( cols[[ 2 ]], ' is not monotonic increasing within ',
                 cols[[ 1 ]] )
        message( 'the corresponding subjects are:' )
        message( paste( ev[ ev == FALSE ][ , get( cols[[ 1 ]] ) ], collapse = '; ' ) )
      }
      stop( 'Please, fix the issues and relaunch augment()' )
    } else {
      if ( verbose ) {
        cat( 'Ok, ', cols[[ 2 ]], ' is monotonic\n', sep = '' )
        cat( '---\n' )
      }
    }
    setkeyv( data, cols )
  } else {
    cols = as.character( substitute( data_key ) )
    if ( !length( cols ) )
      cols = colnames( data )
    setkeyv( data, c( cols, t_start ) )
    data[ , n_events := seq( .N ), by = eval( cols ) ]
    cols = c( cols, names( data )[ dim( data )[ 2 ] ] )
    if ( verbose ) {
      message( 'checking monotonicity of ', cols[[ 2 ]] )
    }
    ev = data[ , .( ev = all( get( cols[[ 2 ]] ) == cummax( get( cols[[ 2 ]] ) ) ) ),
               by = eval( cols[[ 1 ]] ) ]
    setkeyv( data, c( cols[[ 1 ]], t_start ) )
    if ( all( ev$ev ) == FALSE ) {
      if ( verbose ) {
        message( cols[[ 2 ]], ' is not monotonic increasing within ',
                 cols[[ 1 ]] )
        message( 'the corresponding subjects are:' )
        message( paste( ev[ ev == FALSE ][ , get( cols[[ 1 ]] ) ], collapse = '; ' ) )
      }
      stop( 'Please, fix the issues and relaunch augment()' )
    } else {
      if ( verbose ) {
        cat( 'Ok, n_events is monotonic\n' )
        cat( '---\n' )
      }
    }
    setkeyv( data, cols )
  }
  if ( !missing( t_death ) ) {
    t_death = as.character( substitute( t_death ) )
    if ( class( data[[ t_cens ]] ) != class( data[[ t_death ]] ) ) {
      stop( 'the censoring and the death event times must be of the same class' )
    }
  }
  if ( check_NA == TRUE ) {
    if ( verbose ) {
      message( 'checking for any missing values in function arguments' )
    }
    checks = c( cols, pattern, t_start, t_end )
    test = apply( data[ , checks, with = FALSE ], 2, function( x ) any( sum( is.na( x ) ) > 0 ) )
    if ( any ( test ) ) {
      cat( '---\n' )
      if ( verbose ) {
        message( 'detected missing values in the following variables:' )
      }
      invisible( sapply( names( test[ test == TRUE ] ), function( x ) cat( x, '\n' ) ) )
      stop( 'Please, fix the issues and relaunch augment()' )
    } else {
      if ( verbose ) {
        cat( 'Ok, no missing values detected\n' )
        cat( '---\n' )
      }
    }
  }
  if ( !missing( more_status ) ) {
    more_status = as.character( substitute( more_status ) )
    test = apply( data[ , more_status, with = FALSE ], 2,
                  function( x ) any( sum( is.na( x ) ) > 0 ) )
    if ( any ( test ) ) {
      if ( verbose ) {
        cat( '---\n' )
        message( 'detected missing values in variable:' )
      }
      invisible( sapply( names( test[ test == TRUE ] ), function( x ) cat( x, '\n' ) ) )
      stop( 'Please, fix the issues and relaunch augment()' )
    }
  }
  values = sort( unique( data[[ pattern ]] ) )

  if ( verbose ) {
    message( 'checking ', pattern, ' and defining patterns' )
  }
  if ( length( values ) < 2 ) {
    stop( 'unit identification label must be an integer, a factor or a character
          with at least 2 elements' )
  } else if ( length( values ) == 2 ) {
    if ( verbose ) {
      cat( 'detected only 2 values\n' )
      cat( '---\n' )
    }
    if ( inherits( data[[ pattern ]], c( 'integer', 'numeric' ) ) ) {
      match1 = data[ data[ get( pattern ) == 0, .I[ .N ], by = eval( cols[[ 1 ]] ) ]$V1 ]
      if ( missing( t_death ) ) {
        match3 = data[ data[ get( pattern ) == 1,
                             .I[ .N ], by = eval( cols[[ 1 ]] ) ]$V1
                       ][ get( t_end ) != get( t_cens ) ]
      } else {
        match3 = data[ data[ get( pattern ) == 1,
                             .I[ .N ], by = eval( cols[[ 1 ]] ) ]$V1
                       ][ get( t_end ) != get( t_death ) ]
      }
    } else if ( inherits( data[[ pattern ]], 'factor' ) ) {
      match1 = data[ data[ as.integer( get( pattern ) ) - 1 == 0,
                           .I[ .N ], by = eval( cols[[ 1 ]] ) ]$V1 ]
      if ( missing( t_death ) ) {
        match3 = data[ data[ as.integer( get( pattern ) ) - 1 == 1,
                             .I[ .N ], by = eval( cols[[ 1 ]] ) ]$V1
                       ][ get( t_end ) != get( t_cens ) ]
      } else {
        match3 = data[ data[ as.integer( get( pattern ) ) - 1 == 1,
                             .I[ .N ], by = eval( cols[[ 1 ]] ) ]$V1
                       ][ get( t_end ) != get( t_death ) ]
      }
    } else if ( inherits( data[[ pattern ]], 'character' ) ) {
      match1 = data[ data[ get( pattern ) == values[ 1 ], .I[ .N ], by = eval( cols[[ 1 ]] ) ]$V1 ]
      if ( missing( t_death ) ) {
        match3 = data[ data[ get( pattern ) == values[ 2 ],
                             .I[ .N ], by = eval( cols[[ 1 ]] ) ]$V1
                       ][ get( t_end ) != get( t_cens ) ]
      } else {
        match3 = data[ data[ get( pattern ) == values[ 2 ],
                             .I[ .N ], by = eval( cols[[ 1 ]] ) ]$V1
                       ][ get( t_end ) != get( t_death ) ]
      }
    }
  } else if ( length( values ) == 3 ) {
    if ( verbose ) {
      cat( 'Ok, detected 3 values\n' )
      cat( '---\n' )
    }
    if ( inherits( data[[ pattern ]], c( 'integer', 'numeric' ) ) ) {
      match1 = data[ data[ get( pattern ) == 0, .I[ .N ], by = eval( cols[[ 1 ]] ) ]$V1 ]
      match3 = data[ data[ get( pattern ) == 2, .I[ .N ], by = eval( cols[[ 1 ]] ) ]$V1 ]
    } else if ( inherits( data[[ pattern ]], 'factor' ) ) {
      match1 = data[ data[ as.integer( get( pattern ) ) - 1 == 0,
                           .I[ .N ], by = eval( cols[[ 1 ]] ) ]$V1 ]
      match3 = data[ data[ as.integer( get( pattern ) ) - 1 == 2,
                           .I[ .N ], by = eval( cols[[ 1 ]] ) ]$V1 ]
    } else if ( inherits( data[[ pattern ]], 'character' ) ) {
      match1 = data[ data[ get( pattern ) == values[ 1 ], .I[ .N ], by = eval( cols[[ 1 ]] ) ]$V1 ]
      match3 = data[ data[ get( pattern ) == values[ 3 ], .I[ .N ], by = eval( cols[[ 1 ]] ) ]$V1 ]
    }
  }
  if ( verbose ) {
    message( 'augmenting data' )
  }
  l = list( data, data, match1, match3 )
  final = rbindlist( l )
  setkeyv( final, cols )

  if ( verbose ) {
    cat( 'Ok, data have been augmented\n' )
    cat( '---\n' )
  }

  if ( length( values ) == 2 ) {
    if ( verbose ) {
      message( 'defining dimensions' )
    }
    if ( missing( t_death ) ) {
      t1 = data[ , .( .N,
                      t_end = max( get( t_end ) ),
                      t_cens = max( get( t_cens ) ) ), by = eval( cols[[ 1 ]] ) ]
      setkeyv( data, c( cols[[ 1 ]], pattern ) )
      t2 = unique( data[ , .( get( cols[[ 1 ]]), get( pattern ) ) ] )
      setkeyv( data, c( cols[[ 1 ]] ) )
      maker = t1[ t2 ]
      s = dim( maker )[ 1 ]
      flag_temp = vector( mode = 'list', dim( maker )[ 1 ] )
    } else {
      t1 = data[ , .( .N,
                      t_end = max( get( t_end ) ),
                      t_death = max( get( t_death ) ) ), by = eval( cols[[ 1 ]] ) ]
      setkeyv( data, c( cols[[ 1 ]], pattern ) )
      t2 = unique( data[ , .( get( cols[[ 1 ]]), get( pattern ) ) ] )
      setkeyv( data, c( cols[[ 1 ]] ) )
      maker = t1[ t2 ]
      s = dim( maker )[ 1 ]
      flag_temp = vector( mode = 'list', dim( maker )[ 1 ] )
    }
    if ( verbose ) {
      cat( 'Ok, dimensions computed\n' )
      cat( '---\n' )
    }
  } else if ( length( values ) == 3 ) {
    if ( verbose ) {
      message( 'defining dimensions' )
    }
    t1 = data[ , .N, by = eval( cols[[ 1 ]] ) ]
    setkeyv( data, c( cols[[ 1 ]], pattern ) )
    t2 = unique( data[ , .( get( cols[[ 1 ]]), get( pattern ) ) ] )
    setkeyv( data, c( cols[[ 1 ]] ) )
    maker = t1[ t2 ]
    s = dim( maker )[ 1 ]
    if ( verbose ) {
      cat( 'Ok, dimensions computed\n' )
      cat( '---\n' )
    }
  }
  if ( verbose ) {
    message( 'adding status flag' )
  }
  if ( length( values ) == 2 ) {
    a = maker[ V2 == values[ 1 ] ]
    arow = nrow( a )
    if ( missing( t_death ) ) {
      din  = maker[ V2 == values[ 2 ] & t_end == t_cens ]
      dout = maker[ V2 == values[ 2 ] & t_end != t_cens ]
    } else {
      din  = maker[ V2 == values[ 2 ] & t_end == t_death ]
      dout = maker[ V2 == values[ 2 ] & t_end != t_death ]
    }
    dinrow  = nrow( din )
    doutrow = nrow( dout )
    temp1 = din[ , .SD, .SDcols = cols[[ 1 ]] ]
    temp2 = dout[ , .SD, .SDcols = cols[[ 1 ]] ]
    setkeyv( temp1, cols[[ 1 ]] )
    setkeyv( temp2, cols[[ 1 ]] )
    setkeyv( final, cols[[ 1 ]] )
    din_long  = final[ temp1 ]
    dout_long = final[ temp2 ]
    a_long = final[ get( pattern ) == values[ 1 ] ]
    rm( temp1, temp2, final )
    flag_temp_a    = vector( mode = 'list', arow )
    flag_temp_din  = vector( mode = 'list', dinrow )
    flag_temp_dout = vector( mode = 'list', doutrow )

    cat( '---\n' )
    if ( verbose ) {
      message( 'processing alive units...' )
    }
    for ( i in seq_along( a$N ) ) {
      if ( verbose ) {
        if ( arow >= 1e6 ) {
          if ( i %% 1e6 == 0 ) {
            cat( '* * * iteration', i, 'of', arow, '\n' )
          }
        } else  {
          if ( i %% 1e5 == 0 ) {
            cat( '* * * iteration', i, 'of', arow, '\n' )
          }
        }
      }
      flag_temp_a[[ i ]] = c( rep( c( state[[ 1 ]], state[[ 2 ]] ), a$N[ i ] ),
                              state [[ 2 ]] )
    }
    if ( verbose ) {
      message( 'processing units dead inside a transition...' )
    }
    for ( i in seq_along( din$N ) ) {
      if ( verbose ) {
        if ( dinrow >= 1e6 ) {
          if ( i %% 1e6 == 0 ) {
            cat( '* * * iteration', i, 'of', dinrow, '\n' )
          }
        } else  {
          if ( i %% 1e5 == 0 ) {
            cat( '* * * iteration', i, 'of', dinrow, '\n' )
          }
        }
      }
      flag_temp_din[[ i ]] = c( rep( c( state[[ 1 ]], state[[ 2 ]] ),
                                     ( din$N[ i ] - 1 ) ),
                                state[[ 1 ]], state[[ 3 ]] )
    }
    if ( verbose ) {
      message( 'processing units dead outside a transition...' )
    }
    for( i in seq_along( dout$N ) ) {
      if ( verbose ) {
        if ( doutrow >= 1e6 ) {
          if ( i %% 1e6 == 0 ) {
            cat( '* * * iteration', i, 'of', doutrow, '\n' )
          }
        } else  {
          if ( i %% 1e5 == 0 ) {
            cat( '* * * iteration', i, 'of', doutrow, '\n' )
          }
        }
      }
      flag_temp_dout[[ i ]] = c( rep( c( state[[ 1 ]], state[[ 2 ]] ),
                                      dout$N[ i ] ),
                                 state[[ 3 ]] )
    }
    flag_a    = unlist( flag_temp_a, recursive = FALSE )
    flag_din  = unlist( flag_temp_din, recursive = FALSE )
    flag_dout = unlist( flag_temp_dout, recursive = FALSE )
    a_long[ , status := flag_a ]
    din_long[ , status := flag_din ]
    dout_long[ , status := flag_dout ]
    l = list( a_long, din_long, dout_long )
    final = rbindlist( l )
    setkeyv( final, cols )
    rm( a, a_long, din, din_long, dout, dout_long )
    if ( verbose ) {
      cat( '---\n' )
    }
  } else if ( length( values ) == 3 ) {
    flag_temp = vector( mode = 'list', dim( maker )[ 1 ] )
    for ( i in seq_along( maker$N ) ) {
      if ( verbose ) {
        if ( s >= 1e6 ) {
          if ( i %% 1e6 == 0 ) {
            cat( '* * * iteration', i, 'of', s, '\n' )
          }
        } else  {
          if ( i %% 1e5 == 0 ) {
            cat( '* * * iteration', i, 'of', s, '\n' )
          }
        }
      }
      if ( maker$V2[ i ] == values[ 1 ] ) {
        flag_temp[[ i ]] = c( rep( c( state[[ 1 ]], state[[ 2 ]] ), maker$N[ i ] ), state [[ 2 ]] )
      } else if ( maker$V2[ i ] == values[ 2 ] ) {
        flag_temp[[ i ]] = c( rep( c( state[[ 1 ]], state[[ 2 ]] ), ( maker$N[ i ] - 1 ) ),
                              state[[ 1 ]], state[[ 3 ]] )
      } else if ( maker$V2[ i ] == values[ 3 ] ) {
        flag_temp[[ i ]] = c( rep( c( state[[ 1 ]], state[[ 2 ]] ), maker$N[ i ] ),
                              state[[ 3 ]] )
      }
    }
    flag = unlist( flag_temp, recursive = FALSE )
    final[ , status := flag ]
    if ( sum( is.na( final$status ) ) == 0 ) {
      if ( verbose ) {
        cat( 'status flag has been added successfully \n' )
        cat( '---\n' )
      }
    } else {
      stop( 'status flag has not been build correctly' )
    }
  }
  if ( verbose ) {
    message( 'adding numeric status flag' )
  }
  k = uniqueN( final$status )
  lev = unique( final$status )
  for ( i in 1:k ) {
    final[ status == lev[ i ], status_num := i ]
  }
  if ( i == k ) {
    if ( verbose ) {
      cat( 'numeric status has been added successfully \n' )
      cat( '---\n' )
    }
  } else {
    stop( 'numeric status has not been build correctly' )
  }
  if ( verbose ) {
    message( 'adding sequential status flag' )
  }
  if ( missing( n_events ) ) {
    final[ status != state[[ 3 ]], n_status := paste( n_events, ' ', status, sep = '' ) ]
    final[ status == state[[ 3 ]], n_status := state[[ 3 ]]]
  } else {
    final[ status != state[[ 3 ]], n_status := paste( get( cols[[ 2 ]] ), ' ', status, sep = '' ) ]
    final[ status == state[[ 3 ]], n_status := state[[ 3 ]] ]
  }
  if ( sum( is.na( final$n_status ) ) == 0 ) {
    if ( verbose ) {
      cat( 'sequential status flag has been added successfully \n' )
      cat( '---\n' )
    }
  } else {
    stop( 'sequential status flag has not been build correctly' )
  }
  if ( missing( t_augmented ) ) {
    t_augmented = 'augmented'
  } else {
    t_augmented = as.character( substitute( t_augmented ) )
  }
  if ( verbose ) {
    message( 'adding variable ', t_augmented, ' as new time variable' )
  }
  final[ status == state[[ 1 ]], ( t_augmented ) := get( t_start ) ]
  final[ status == state[[ 2 ]], ( t_augmented ) := get( t_end ) ]
  if ( missing( t_death ) ) {
    final[ status == state[[ 3 ]], ( t_augmented ) := get( t_cens ) ]
  } else {
    final[ status == state[[ 3 ]], ( t_augmented ) := get( t_death ) ]
  }
  if ( inherits( data[[ t_start ]], 'Date' ) ) {
    final[ , ( paste0( t_augmented, '_int' ) ) := as.integer( get( t_augmented ) ) ]
    id_col = which( names( data ) == t_start )
    setcolorder( final, c( 1:( id_col - 1 ), ( dim( final )[ 2 ] - 1 ), dim( final )[ 2 ],
                           id_col:( dim( final )[ 2 ] - 2 ) ) )
    if ( verbose ) {
      cat( 'variables \"', t_augmented, '\" and \"',
           paste0( t_augmented, '_int' ),
           '\" successfully added and repositioned\n', sep = '' )
      cat( '---\n' )
    }
  } else if ( inherits( data[[ t_start ]], 'difftime' ) ) {
    final[ , ( paste0( t_augmented, '_num' ) ) := as.numeric( get( t_augmented ) ) ]
    id_col = which( names( data ) == t_start )
    setcolorder( final, c( 1:( id_col - 1 ), ( dim( final )[ 2 ] - 1 ), dim( final )[ 2 ],
                           id_col:( dim( final )[ 2 ] - 2 ) ) )
    if ( verbose ) {
      cat( 'variables \"', t_augmented, '\" and \"',
           paste0( t_augmented, '_num' ),
           '\" successfully added and repositioned\n', sep = '' )
      cat( '---\n' )
    }
  } else if ( inherits( data[[ t_start ]], 'integer' ) ||
              inherits( data[[ t_start ]], 'numeric' ) ) {

    id_col = which( names( data ) == t_start )
    setcolorder( final, c( 1:( id_col - 1 ), dim( final )[ 2 ],
                           id_col:( dim( final )[ 2 ] - 1 ) ) )
    if ( verbose ) {
      cat( 'variable \"', t_augmented,
           '\" successfully added and repositioned\n', sep = '' )
      cat( '---\n' )
    }
  }

  if ( !missing( more_status ) ) {
    if ( verbose ) {
      message( '* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *' )
      message( 'detected a more complex status given by variable ', more_status,
               '. Processing...')
      message( '* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *' )
      cat( '---\n' )
    }
    values = unique( data[[ more_status ]] )
    if ( verbose ) {
      message( 'adding expanded status flag' )
    }
    final[ status == state[[ 3 ]], status_exp := state[[ 3 ]] ]
    for ( i in seq_along( values ) ) {
      final[ status != state[[ 3 ]] & get( more_status ) == values[ i ],
             status_exp := paste( values[ i ], '_', status, sep = '' ) ]
    }
    if ( sum( is.na( final$status_exp ) ) == 0 ) {
      if ( verbose ) {
        cat( 'expanded status flag has been added successfully \n' )
        cat( '---\n' )
      }
    } else {
      stop( 'expanded status flag has not been build correctly' )
    }
    if ( verbose ) {
      message( 'adding numeric expanded status flag' )
    }
    k = uniqueN( final$status_exp )
    lev = unique( final$status_exp )
    for ( i in 1:k ) {
      final[ status_exp == lev[ i ], status_exp_num := i ]
    }
    if ( i == k ) {
      if ( verbose ) {
        cat( 'numeric expanded status has been added successfully \n' )
        cat( '---\n' )
      }
    } else {
      stop( 'numeric expanded status has not been build correctly' )
    }
    if ( verbose ) {
      message( 'adding sequential expanded status flag' )
    }
    if ( missing( n_events ) ) {
      final[ status_exp != state[[ 3 ]], n_status_exp := paste( n_events, ' ', status_exp, sep = '' ) ]
      final[ status_exp == state[[ 3 ]], n_status_exp := state[[ 3 ]] ]
    } else {
      final[ status_exp != state[[ 3 ]],
             n_status_exp := paste( get( cols[[ 2 ]] ), ' ', status_exp, sep = '' ) ]
      final[ status_exp == state[[ 3 ]], n_status_exp := state[[ 3 ]]]
    }
    if ( sum( is.na( final$n_status_exp ) ) == 0 ) {
      if ( verbose ) {
        cat( 'sequential expanded status flag has been added successfully \n' )
      }
    } else {
      stop( 'sequential expanded status flag has not been build correctly' )
    }
  }
  toc = proc.time()
  time = toc - tic
  if ( verbose ) {
    cat( '---------------------------\n' )
    cat( 'augment() took:', time[ 3 ], 'sec. \n', sep = ' ' )
    cat( '---------------------------\n' )
  }
  options( warn = oldw )
  if ( convert == TRUE ) {
    setDF( final )
    return( final )
  }
  final[]
  return( final )
}

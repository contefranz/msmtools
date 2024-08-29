if ( getRversion() >= "2.15.1" ) {
  utils::globalVariables( c( "status", "status_num", "n_status",
                             "status_exp", "status_exp_num", "n_status_exp",
                             ".", "V2" ) )
}
#' A fast and general method for building augmented data
#'
#' A fast and general method for reshaping standard longitudinal data into a new
#' structure called augmented'. This format is suitable under a multi-state
#' framework using the \code{\link[msm]{msm}} package.
#'
#' @param data A \code{data.table} or \code{data.frame} object in longitudinal
#' format where each row represents an observation in which the exact starting
#' and ending time of the process are known and recorded. If \code{data} is a
#' \code{data.frame}, then \code{augment} internally casts it to a \code{data.table}.
#' @param data_key A keying variable which \code{augment} uses to define a key
#' for \code{data}. This represents the subject ID (see
#' \code{\link[data.table]{setkey}}).
#' @param n_events An integer variable indicating the progressive (monotonic)
#' event number of a given ID. \code{augment} always checks whether
#' \code{n_events} is monotonic increasing within the provided \code{data_key}
#' and stops the execution in case the check fails (see 'Details').
#' If missing, \code{augment} fastly creates a variable named \code{"n_events"}.
#' @param pattern Either an integer, a factor or a character with 2 or 3 unique
#' values which provides the ID status at the end of the study. \code{pattern}
#' has a predefined structure. When 2 values are detected, they must be in the
#' format: 0 = "alive", 1 = "dead". When 3 values are detected, then the format
#' must be: 0 = "alive", 1 = "dead during a transition", 2 = "dead after a
#' transition has ended" (see 'Details').
#' @param state A list of three and exactly three possible states which a
#' subject can reach. \code{state} has a predefined structure as follows:
#' IN, OUT, DEAD (see 'Details').
#' @param t_start The starting time of an observation. It can be passed as date,
#' integer, or numeric format.
#' @param t_end The ending time of an observation. It can be passed as date,
#' integer, or numeric format.
#' @param t_cens The censoring time of the study. This is the date until each
#' ID is observed, if still active in the cohort.
#' @param t_death The exact death time of a subject ID. If \code{t_death} is
#' missing, \code{t_cens} is assumed to contain both censoring and death times
#' and a warning is raised.
#' @param t_augmented A variable indicating the name of the new time variable
#' of the process in the augmented format. If \code{t_augmented} is missing,
#' then the default name 'augmented' is assumed and the corresponding new
#' variable is added to \code{data}. \code{t_augmented} is cast to integer
#' or to numeric depending whether \code{t_start} is a date or a difftime,
#' respectively. The suffix '_int' or '_num' is pasted to \code{t_augmented}
#' and a new variable is computed accordingly.
#' This is done because \code{\link[msm]{msm}} can't correctly deal with date
#' or difftime variables. Both variables are positioned before \code{t_start}.
#' @param more_status A variable which marks further transitions beside the
#' default ones given by \code{state}. \code{more_status} can be a factor or a
#' character (see 'Details'). If missing, \code{augment} ignores it.
#' @param check_NA If \code{TRUE}, then arguments \code{data_key},
#' \code{n_events}, \code{pattern}, \code{t_start} and \code{t_end} are looked
#' up for any missing data and if the function finds any, it stops with error.
#' Default is \code{FALSE} because \code{augment} is not intended for
#' running consistency checks, beside what is mandatory, and because the
#' procedure is computationally onerous and could cause memory overhead for
#' very large datasets. Argument \code{more_status} is the only one for which
#' \code{augment} always checks for the presence of missing data and, again,
#' if it finds any it just stops with error.
#' @param convert If \code{TRUE}, then the returned object is automatically
#' converted to the class \code{data.frame}. This is done in place and comes
#' at very low cost both from running time and memory consumption
#' (see \code{\link[data.table]{setDF}}).
#' @param verbose If \code{FALSE}, all information produced by \code{print},
#' \code{cat} and \code{message} are suppressed. Default is \code{TRUE}.
#' @details In order to get the data processed, a monotonic increasing process
#' needs to be ensured. In the first place, \code{augment} checks this both in
#' case \code{n_events} is missing or not. The data are efficiently ordered through
#' \code{\link[data.table]{setkey}} function with \code{data_key} as the primary
#' key and \code{t_start} as the secondary key. In the second place, it checks
#' the monotonicity of \code{n_events} and if it fails, it stops with error and
#' returns the subjects given by \code{data_key} for whom the condition is not
#' met. If \code{n_events} is missing, then \code{augment} internally computes
#' the progression number with the name \emph{n_events} and runs the same
#' procedure.
#'
#' Attention needs to be payed to argument \code{pattern}. Integer values can
#' be 0 and 1 if only two status are defined and they must correspond to the
#' status 'alive' and 'dead'. If three values are defined, then they must be 0,
#' 1 and 2 if \code{pattern} is an integer, or 'alive', 'dead inside a
#' transition' and dead outside a transition' if \code{pattern} is either a
#' character or a factor. The order matters: it is not possible to specify
#' 0 as 'dead' for instance.
#'
#' When passing a list of states, the order is important so that the first
#' element must be the state corresponding to the starting time
#' (i.e. 'IN', inside the hospital), the second element must correspond
#' to the ending time (i.e. 'OUT', outside the hospital), and the third state
#' is the absorbing state (i.e. 'DEAD').
#'
#' \code{more_status} allows to manage multiple transitions beside what already
#' specified in \code{state}. In particular, if the corresponding observation
#' is a standard admission which adds no other information than what is inside
#' \code{state}, then \code{more_status} must be set to 'df' which stands for
#' 'Default' (see 'Examples' or run ?hosp and look at the variable 'rehab_it').
#' In general, it is always a good practice to fully specify the transition
#' with a bunch of self-explanatory characters in order to quickly understand
#' which is the current transition.
#'
#' @return An augmented format dataset of class \code{data.table}, or
#' \code{data.frame} when \code{convert} is \code{TRUE}, where each row
#' represents a specific transition for a given subject. \code{augment} returns
#' them after some important variables have been computed:\cr
#'
#' \item{\code{augmented}}{The new timing variable for the process when looking
#' at transitions. If \code{t_augmented} is missing, then \code{augment} creates
#' \emph{augmented} by default. \emph{augmented}. The function looks directly
#' to \code{t_start} and \code{t_end} to build it and thus it inherits their class.
#' In particular, if \code{t_start} is a date format, then \code{augment}
#' computes a new variable cast as integer and names it \emph{augmented_int}.
#' If \code{t_start} is a difftime format, then \code{augment} computes a new
#' variable cast as a numeric and names it \emph{augmented_num.}}
#' \item{\code{status}}{A status flag which contains the states as specified
#' in \code{state}. \code{augment} automatically checks whether argument
#' \code{pattern} has 2 or 3 unique values and computes the correct structure
#' of a given subject as reported in the vignette. The variable is cast as
#' character.}
#' \item{\code{status_num}}{The corresponding integer version of \emph{status}.}
#' \item{\code{n_status}}{A mix of \code{status} and \code{n_events} cast as
#' character. This becomes useful when a multi-state model on the progression
#' of the process needs to be implemented.}
#'
#' If \code{more_status} is passed, then \code{augment} computes some more
#' variables. They mimic the meaning of \emph{status}, \emph{status_num},
#' and \emph{n_status} but they account for the more complex structure defined.
#' They are: \code{status_exp}, \code{status_exp_num}, and \code{n_status_exp}.
#'
#' @examples
#' \dontrun{
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
#'}
#' @references Jackson, C.H. (2011). Multi-State Models for Panel Data:\cr
#' The \emph{msm} Package for R. Journal of Statistical Software, 38(8), 1-29.\cr
#' URL \url{https://www.jstatsoft.org/v38/i08/}.
#'
#' M. Dowle, A. Srinivasan, T. Short, S. Lianoglou with contributions from
#' R. Saporta and E. Antonyan (2016):\cr
#' \emph{data.table}: Extension of \emph{data.frame}. R package version 1.9.6\cr
#' URL \url{https://github.com/Rdatatable/data.table/wiki}
#'
#' @seealso \code{\link[data.table]{data.table}} \code{\link[data.table]{setkey}}
#' @author Francesco Grossetti \email{francesco.grossetti@@unibocconi.it}.
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
    warning( 'no t_death has been passed. Assuming that ', substitute( t_cens ),
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

  # Convert arguments to symbols for NSE
  data_key = as.character(substitute(data_key))
  pattern  = as.character(substitute(pattern))
  t_start  = as.character(substitute(t_start))
  t_end    = as.character(substitute(t_end))
  t_cens   = as.character(substitute(t_cens))

  if ( class(data[[t_start]]) != class(data[[t_end]]) ) {
    stop( 'the starting and the ending event times must be of the same class' )
  } else if ( class(data[[t_start]]) != class(data[[t_cens]]) ) {
    stop( 'the starting and the censoring event times must be of the same class' )
  }

  # OLD CODE
  # pattern = as.character( substitute( list( pattern ) )[ -1L ] )
  # t_start = as.character( substitute( list( t_start ) )[ -1L ] )
  # t_end   = as.character( substitute( list( t_end ) )[ -1L ] )
  # t_cens  = as.character( substitute( list( t_cens ) )[ -1L ] )
  # if ( eval( substitute( class( data$t_start ) ) ) != eval( substitute( class( data$t_end ) ) ) ) {
  #   stop( 'the starting and the ending event times must be of the same class' )
  # } else if ( eval( substitute( class( data$t_start ) ) ) != eval( substitute( class( data$t_cens ) ) ) ) {
  #   stop( 'the starting and the censoring event times must be of the same class' )
  # }
  # OLD CODE

  setkey( data, NULL )
  # check if n_events is passed
  if ( !missing( n_events ) ) {
    # Convert to character for dynamic column name handling
    n_events = as.character(substitute(n_events))
    cols = c(data_key, n_events)
    # cols = as.character( substitute( list( data_key, n_events ) )[ -1L ] )
    if ( !length( cols ) )
      cols = colnames( data )
    # check if n_events is an integer
    if ( !inherits(data[[cols[2L]]], "integer") ) {
      stop( 'n_events must be an integer' )
    }
    # if ( !inherits( eval( substitute( data$n_events ) ), "integer" ) ) {
    #   stop( 'n_events must be an integer' )
    # }
    if ( verbose ) {
      message( 'checking monotonicity of ', cols[2L] )
    }
    # Check monotonicity of n_events
    ev = data[, .(ev = all(.SD[[1]] == cummax(.SD[[1]]))), by = data_key, .SDcols = n_events]

    # ev = data[, .(ev = all(get(cols[2]) == cummax(get(cols[2])))), by = get(cols[1])]
    # ev = data[ , .( ev = all( get( cols[[ 2 ]] ) == cummax( get( cols[[ 2 ]] ) ) ) ),
    #            by = eval( cols[[ 1 ]] ) ]
    setkeyv(data, c(data_key, t_start))
    if ( any( ev$ev ) == FALSE ) {
      if ( verbose ) {
        message(n_events, ' is not monotonic increasing within ', data_key)
        message('The corresponding subjects are:')
        # Use .SDcols to avoid get() here
        message(paste(ev[ ev == FALSE, .SD[[1]], .SDcols = data_key], collapse = '; '))
      }
      stop( 'Please, fix the issues and relaunch augment()' )
    } else {
      if ( verbose ) {
        message( 'Ok, ', cols[2L], ' is monotonic', sep = '' )
        message( '---' )
      }
    }
    setkeyv( data, cols )
  } else {
    # cols = as.character( substitute( list( data_key ) )[ -1L ] )
    # if ( !length( cols ) )
    #   cols = colnames( data )
    setkeyv( data, c( data_key, t_start ) )
    data[ , n_events := seq( .N ), by = data_key ]
    n_events = "n_events"
    # cols = c( cols, names( data )[ dim( data )[ 2 ] ] )
    if ( verbose ) {
      message( 'checking monotonicity of ', n_events )
    }
    ev = data[, .(ev = all(.SD[[1]] == cummax(.SD[[1]]))), by = data_key, .SDcols = n_events]
    # ev = data[ , .( ev = all( get( cols[[ 2 ]] ) == cummax( get( cols[[ 2 ]] ) ) ) ),
    #            by = eval( cols[[ 1 ]] ) ]
    setkeyv(data, c(data_key, t_start))
    if ( any( ev$ev ) == FALSE ) {
      if ( verbose ) {
        message(n_events, ' is not monotonic increasing within ', data_key)
        message('The corresponding subjects are:')
        message(paste(ev[ ev == FALSE, .SD[[1]], .SDcols = data_key], collapse = '; '))
      }
      stop( 'Please, fix the issues and relaunch augment()' )
    } else {
      if ( verbose ) {
        message( 'Ok, ', cols[2L], ' is monotonic', sep = '' )
        message( '---' )
      }
    }
    # setkeyv( data, cols )
  }
  if ( !missing( t_death ) ) {
    t_death = as.character(substitute(t_death))
    if ( class(data[[t_cens]]) != class(data[[t_death]]) ) {
      stop( 'the censoring and the death event times must be of the same class' )
    }
  }
  if ( check_NA == TRUE ) {
    if ( verbose ) {
      message( 'checking for any missing values in function arguments' )
    }
    # checks = c( cols, pattern, t_start, t_end )
    checks = c(data_key, n_events, pattern, t_start, t_end)
    test = apply( data[ , checks, with = FALSE ], 2, function( x ) any( sum( is.na( x ) ) > 0 ) )
    if ( any(test) ) {
      message( '---\n' )
      if ( verbose ) {
        message( 'detected missing values in the following variables:' )
      }
      invisible( sapply( names( test[ test == TRUE ] ), function( x ) cat( x, '\n' ) ) )
      stop( 'Please, fix the issues and relaunch augment()' )
    } else {
      if ( verbose ) {
        message( 'Ok, no missing values detected' )
        message( '---\n' )
      }
    }
  }
  if ( !missing( more_status ) ) {
    more_status = as.character(substitute(more_status))
    test = apply( data[ , more_status, with = FALSE ], 2, function( x ) any( sum( is.na( x ) ) > 0 ) )
    if ( any(test) ) {
      if ( verbose ) {
        cat( '---\n' )
        message( 'detected missing values in variable:' )
      }
      invisible( sapply( names( test[ test == TRUE ] ), function( x ) cat( x, '\n' ) ) )
      stop( 'Please, fix the issues and relaunch augment()' )
    }
  }

  # values = sort( eval( substitute( unique( data$pattern ) ) ) )
  values = data[ , sort(unique(.SD[[1L]])), .SDcols = pattern]

  if ( verbose ) {
    message( 'checking ', pattern, ' and defining patterns' )
  }

  # store the class of pattern
  pattern_class = class(data[[pattern]])

  # check how many status labels we have
  if ( length( values ) < 2 ) {
    stop( 'status identification label must be an integer, a factor or a character with at least 2 elements' )
  } else if ( length(values) == 2 ) {
    if ( verbose ) {
      message( 'detected only 2 values' )
      message( '---\n' )
    }
    if ( inherits(pattern_class, "integer") || inherits(pattern_class, "numeric") ) {
      temp = data[ data[ , .SD[[pattern]] ] == 0 ]
      match1 = temp[temp[, .I[.N], by = data_key]$V1]
      temp = data[ data[ , .SD[[pattern]] ] == 1 ]
      if ( missing(t_death) ) {
        match3 = temp[temp[, .SD[[1]] != .SD[[2]], .SDcols = c(t_end, t_cens)]]
      } else {
        match3 = temp[temp[, .SD[[1]] != .SD[[2]], .SDcols = c(t_end, t_death)]]
      }
    } else if ( inherits(pattern_class, "factor") ) {
      temp = data[ data[ , as.integer(.SD[[pattern]]) - 1L] == 0 ]
      match1 = temp[temp[, .I[.N], by = data_key]$V1]
      temp = data[ data[ , as.integer(.SD[[pattern]]) - 1L ] == 1 ]
      if ( missing(t_death) ) {
        match3 = temp[temp[, .SD[[1]] != .SD[[2]], .SDcols = c(t_end, t_cens)]]
      } else {
        match3 = temp[temp[, .SD[[1]] != .SD[[2]], .SDcols = c(t_end, t_death)]]
      }
    } else if ( inherits(pattern_class, "character") ) {
      temp = data[ data[ , .SD[[pattern]] ] == values[1L] ]
      match1 = temp[temp[, .I[.N], by = data_key]$V1]
      temp = data[ data[ , .SD[[pattern]] ] == values[2L] ]
      if ( missing(t_death) ) {
        match3 = temp[temp[, .SD[[1]] != .SD[[2]], .SDcols = c(t_end, t_cens)]]
      } else {
        match3 = temp[temp[, .SD[[1]] != .SD[[2]], .SDcols = c(t_end, t_death)]]
      }
    }


    # if ( class( eval( substitute( data$pattern ) ) ) == 'integer' ||
    #      class( eval( substitute( data$pattern ) ) ) == 'numeric' ) {
    #   match1 = data[ data[ get( pattern ) == 0, .I[ .N ], by = eval( cols[[ 1 ]] ) ]$V1 ]
    #   if ( missing( t_death ) ) {
    #     match3 = data[ data[ get( pattern ) == 1,
    #                          .I[ .N ], by = eval( cols[[ 1 ]] ) ]$V1
    #     ][ get( t_end ) != get( t_cens ) ]
    #   } else {
    #     match3 = data[ data[ get( pattern ) == 1,
    #                          .I[ .N ], by = eval( cols[[ 1 ]] ) ]$V1
    #     ][ get( t_end ) != get( t_death ) ]
    #   }
    # } else if ( class( eval( substitute( data$pattern ) ) ) == 'factor' ) {
    #   match1 = data[ data[ as.integer( get( pattern ) ) - 1 == 0,
    #                        .I[ .N ], by = eval( cols[[ 1 ]] ) ]$V1 ]
    #   if ( missing( t_death ) ) {
    #     match3 = data[ data[ as.integer( get( pattern ) ) - 1 == 1,
    #                          .I[ .N ], by = eval( cols[[ 1 ]] ) ]$V1
    #     ][ get( t_end ) != get( t_cens ) ]
    #   } else {
    #     match3 = data[ data[ as.integer( get( pattern ) ) - 1 == 1,
    #                          .I[ .N ], by = eval( cols[[ 1 ]] ) ]$V1
    #     ][ get( t_end ) != get( t_death ) ]
    #   }
    # } else if ( class( eval( substitute( data$pattern ) ) ) == 'character' ) {
    #   match1 = data[ data[ get( pattern ) == values[ 1 ], .I[ .N ], by = eval( cols[[ 1 ]] ) ]$V1 ]
    #   if ( missing( t_death ) ) {
    #     match3 = data[ data[ get( pattern ) == values[ 2 ],
    #                          .I[ .N ], by = eval( cols[[ 1 ]] ) ]$V1
    #     ][ get( t_end ) != get( t_cens ) ]
    #   } else {
    #     match3 = data[ data[ get( pattern ) == values[ 2 ],
    #                          .I[ .N ], by = eval( cols[[ 1 ]] ) ]$V1
    #     ][ get( t_end ) != get( t_death ) ]
    #   }
    # }
  } else if ( length( values ) == 3 ) {
    if ( verbose ) {
      message( 'Ok, detected 3 values' )
      message( '---' )
    }
    if ( inherits(pattern_class, "integer") || inherits(pattern_class, "numeric") ) {
      temp = data[ data[ , .SD[[pattern]] ] == 0 ]
      match1 = temp[temp[, .I[.N], by = data_key]$V1]
      temp = data[ data[ , .SD[[pattern]] ] == 2 ]
      match3 = temp[temp[, .I[.N], by = data_key]$V1]
    } else if ( inherits(pattern_class, "factor") ) {
      temp = data[ data[ , as.integer(.SD[[pattern]]) - 1L] == 0 ]
      match1 = temp[temp[, .I[.N], by = data_key]$V1]
      temp = data[ data[ , as.integer(.SD[[pattern]]) - 1L] == 2]
      match3 = temp[temp[, .I[.N], by = data_key]$V1]
    } else if ( inherits(pattern_class, "character") ) {
      temp = data[ data[ , .SD[[pattern]] ] == values[1L] ]
      match1 = temp[temp[, .I[.N], by = data_key]$V1]
      temp = data[ data[ , .SD[[pattern]] ] == values[3L] ]
      match3 = temp[temp[, .I[.N], by = data_key]$V1]
    }

    # if ( class( eval( substitute( data$pattern ) ) ) == 'integer' ||
    #      class( eval( substitute( data$pattern ) ) ) == 'numeric'  ) {
    #   temp = data[ data[ , .SD[[pattern]] ] == 0 ]
    #   match1 = temp[temp[, .I[.N], by = data_key]$V1]
    #   temp = data[ data[ , .SD[[pattern]] ] == 2 ]
    #   match3 = temp[temp[, .I[.N], by = data_key]$V1]
    #   # match1 = data[ data[ get( pattern ) == 0, .I[ .N ], by = eval( cols[[ 1 ]] ) ]$V1 ]
    #   # match3 = data[ data[ get( pattern ) == 2, .I[ .N ], by = eval( cols[[ 1 ]] ) ]$V1 ]
    # } else if ( class( eval( substitute( data$pattern ) ) ) == 'factor' ) {
    #   match1 = data[ data[ as.integer( get( pattern ) ) - 1 == 0,
    #                        .I[ .N ], by = eval( cols[[ 1 ]] ) ]$V1 ]
    #   match3 = data[ data[ as.integer( get( pattern ) ) - 1 == 2,
    #                        .I[ .N ], by = eval( cols[[ 1 ]] ) ]$V1 ]
    # } else if ( class( eval( substitute( data$pattern ) ) ) == 'character' ) {
    #   match1 = data[ data[ get( pattern ) == values[ 1 ], .I[ .N ], by = eval( cols[[ 1 ]] ) ]$V1 ]
    #   match3 = data[ data[ get( pattern ) == values[ 3 ], .I[ .N ], by = eval( cols[[ 1 ]] ) ]$V1 ]
    # }

  }
  if ( verbose ) {
    message( 'augmenting data' )
  }
  l = list( data, data, match1, match3 )
  final = rbindlist( l )
  setkeyv(final, c(data_key, n_events))

  if ( verbose ) {
    message( 'Ok, data have been augmented' )
    message( '---' )
  }

  if ( length( values ) == 2 ) {
    if ( verbose ) {
      message( 'defining dimesions' )
    }
    if ( missing( t_death ) ) {
      t1 = data[ , .( .N, t_end = max(.SD[[1]]), t_cens = max(.SD[[2]]) ),
                 .SDcols = c(t_end, t_cens), by = data_key ]
    } else {
      t1 = data[ , .( .N, t_end = max(.SD[[1]]), t_death = max(.SD[[2]]) ),
                 .SDcols = c(t_end, t_death), by = data_key ]
    }
    setkeyv( data, c( data_key, pattern ) )
    t2 = unique(data[ , .SD, .SDcols = c(data_key, pattern)])
    setkeyv( data, data_key )
    maker = t1[ t2 ]
    Nrow = nrow(maker)
    flag_temp = vector( mode = 'list', Nrow )
    if ( verbose ) {
      cat( 'Ok, dimensions computed\n' )
      cat( '---\n' )
    }
  } else if ( length( values ) == 3 ) {
    if ( verbose ) {
      message( 'defining dimesions' )
    }
    t1 = data[ , .N, by = data_key ]
    setkeyv( data, c(data_key, n_events, pattern) )
    t2 = unique(data[ , .SD, .SDcols = c(data_key, pattern)])
    setkeyv( data, data_key )
    maker = t1[ t2 ]
    Nrow = nrow(maker)
    if ( verbose ) {
      message( 'Ok, dimensions computed' )
      message( '---' )
    }
  }

  # if ( verbose ) {
  #   message( 'adding status flag' )
  # }

  if ( length( values ) == 2 ) {
    a = maker[ maker[ , .SD[[pattern]] ] == values[ 1 ] ]
    if ( missing( t_death ) ) {
      din  = maker[ maker[ , .SD[[pattern]] ] == values[ 2 ] & t_end == t_cens ]
      dout = maker[ maker[ , .SD[[pattern]] ] == values[ 2 ] & t_end != t_cens ]
    } else {
      din  = maker[ maker[ , .SD[[pattern]] ] == values[ 2 ] & t_end == t_death ]
      dout = maker[ maker[ , .SD[[pattern]] ] == values[ 2 ] & t_end != t_death ]
    }
    arow = nrow( a )
    dinrow  = nrow( din )
    doutrow = nrow( dout )
    temp1 = din[ , .SD, .SDcols = data_key ]
    temp2 = dout[ , .SD, .SDcols = data_key ]
    setkeyv(temp1, data_key)
    setkeyv(temp2, data_key)
    setkeyv(final, data_key)
    din_long  = final[ temp1 ]
    dout_long = final[ temp2 ]
    a_long = final[ final[ , .SD[[pattern]] ] == values[ 1 ] ]
    rm(temp1, temp2, final)
    flag_temp_a    = vector( mode = 'list', arow )
    flag_temp_din  = vector( mode = 'list', dinrow )
    flag_temp_dout = vector( mode = 'list', doutrow )

    cat( '---\n' )
    if ( verbose ) {
      message( 'processing alive units...' )
    }
    for ( i in seq_along( a$N ) ) {
      # if ( verbose ) {
      #   if ( arow >= 1e6 ) {
      #     if ( i %% 1e6 == 0 ) {
      #       cat( '* * * iteration', i, 'of', arow, '\n' )
      #     }
      #   } else  {
      #     if ( i %% 1e5 == 0 ) {
      #       cat( '* * * iteration', i, 'of', arow, '\n' )
      #     }
      #   }
      # }
      flag_temp_a[[ i ]] = c( rep( c( state[[ 1 ]], state[[ 2 ]] ), a$N[ i ] ),
                              state [[ 2 ]] )
    }
    if ( verbose ) {
      message( 'processing units dead inside a transition...' )
    }
    for ( i in seq_along( din$N ) ) {
      # if ( verbose ) {
      #   if ( dinrow >= 1e6 ) {
      #     if ( i %% 1e6 == 0 ) {
      #       cat( '* * * iteration', i, 'of', dinrow, '\n' )
      #     }
      #   } else  {
      #     if ( i %% 1e5 == 0 ) {
      #       cat( '* * * iteration', i, 'of', dinrow, '\n' )
      #     }
      #   }
      # }
      flag_temp_din[[ i ]] = c( rep( c( state[[ 1 ]], state[[ 2 ]] ),
                                     ( din$N[ i ] - 1 ) ),
                                state[[ 1 ]], state[[ 3 ]] )
    }
    if ( verbose ) {
      message( 'processing units dead outside a transition...' )
    }
    for( i in seq_along( dout$N ) ) {
      # if ( verbose ) {
      #   if ( doutrow >= 1e6 ) {
      #     if ( i %% 1e6 == 0 ) {
      #       cat( '* * * iteration', i, 'of', doutrow, '\n' )
      #     }
      #   } else  {
      #     if ( i %% 1e5 == 0 ) {
      #       cat( '* * * iteration', i, 'of', doutrow, '\n' )
      #     }
      #   }
      # }
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
    flag_temp = vector( mode = 'list', Nrow )

    for ( i in seq_along( maker$N ) ) {

      # check in what state a given subject is
      if ( maker[ i, .SD[[pattern]]] == values[ 1 ] ) {
        flag_temp[[ i ]] = c(
          rep( c( state[[ 1 ]], state[[ 2 ]] ), maker$N[ i ] ),
          state [[ 2 ]]
        )
      } else if ( maker[ i, .SD[[pattern]]] == values[ 2 ] ) {
        flag_temp[[ i ]] = c(
          rep( c( state[[ 1 ]], state[[ 2 ]] ), ( maker$N[ i ] - 1 ) ),
          state[[ 1 ]], state[[ 3 ]]
        )
      } else if ( maker[ i, .SD[[pattern]]] == values[ 3 ] ) {
        flag_temp[[ i ]] = c(
          rep( c( state[[ 1 ]], state[[ 2 ]] ), maker$N[ i ] ),
          state[[ 3 ]]
        )
      }
    }

    flag = unlist( flag_temp, recursive = FALSE )
    final[ , status := flag ]
    if ( sum( is.na( final$status ) ) == 0 ) {
      if ( verbose ) {
        message( 'status flag has been added successfully' )
        message( '---' )
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
  # if ( i == k ) {
  #   if ( verbose ) {
  #     message( 'numeric status has been added successfully' )
  #     message( '---' )
  #   }
  # } else {
  #   stop( 'numeric status has not been build correctly' )
  # }
  if ( verbose ) {
    message( 'adding sequential status flag' )
  }
  if ( missing( n_events ) ) {
    final[ status != state[[ 3 ]], n_status := paste( n_events, ' ', status, sep = '' ) ]
    final[ status == state[[ 3 ]], n_status := state[[ 3 ]]]
  } else {
    # final[ status != state[[ 3 ]], n_status := paste( get( cols[[ 2 ]] ), ' ', status, sep = '' ) ]
    final[ status != state[[ 3 ]], n_status := paste0(.SD[[n_events]], " ", status)]
    final[ status == state[[ 3 ]], n_status := state[[ 3 ]] ]
  }
  if ( sum( is.na( final$n_status ) ) == 0 ) {
    if ( verbose ) {
      message( 'sequential status flag has been added successfully' )
      message( '---' )
    }
  } else {
    stop( 'sequential status flag has not been build correctly' )
  }

  if ( missing( t_augmented ) ) {
    t_augmented = "augmented"
  } else {
    t_augmented = as.character( substitute( t_augmented ) )
  }
  if ( verbose ) {
    message( 'adding variable ', t_augmented, ' as new time variable' )
  }
  # final[ status == state[[ 1 ]], substitute( t_augmented ) := get( t_start ) ]
  # final[ status == state[[ 2 ]], substitute( t_augmented ) := get( t_end ) ]
  final[ status == state[[ 1 ]], (t_augmented) := .SD[[t_start]] ]
  final[ status == state[[ 2 ]], (t_augmented) := .SD[[t_end]] ]
  if ( missing( t_death ) ) {
    final[ status == state[[ 3 ]], (t_augmented) := .SD[[t_cens]] ]
  } else {
    final[ status == state[[ 3 ]], (t_augmented) := .SD[[t_death]] ]
  }
  t_start_class = class(data[[t_start]])
  if ( t_start_class == "Date" ) {
    final[ , paste0( t_augmented, '_int' ) := as.integer( .SD[[t_augmented]] ) ]
    # if ( verbose ) {
    #   cat( 'variables \"', t_augmented, '\" and \"',
    #        paste( t_augmented, '_int', sep = '' ),
    #        '\" successfully added and repositioned\n', sep = '' )
    #   cat( '---\n' )
    # }
  } else if ( t_start_class == "difftime" ) {
    final[ , paste0(t_augmented, '_num') := as.numeric( .SD[[t_augmented]] ) ]
    # if ( verbose ) {
    #   cat( 'variables \"', substitute( t_augmented ), '\" and \"',
    #        paste( substitute( t_augmented ), '_num', sep = '' ),
    #        '\" successfully added and repositioned\n', sep = '' )
    #   cat( '---\n' )
    # }
  } else if ( inherits( t_start_class, "integer") || inherits( t_start_class, "numeric") ) {
    # if ( verbose ) {
    #   cat( 'variable \"', substitute( t_augmented ),
    #        '\" successfully added and repositioned\n', sep = '' )
    #   cat( '---\n' )
    # }
  }
  id_col = which(names(final) == t_start)
  setcolorder( final, c( 1:( id_col - 1 ), ( dim( final )[ 2 ] - 1 ), dim( final )[ 2 ],
                         id_col:( dim( final )[ 2 ] - 2 ) ) )


  if ( !missing( more_status ) ) {
    more_status = as.character(substitute(more_status))
    if ( verbose ) {
      message( '* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *' )
      message( 'detected a more complex status given by variable ', more_status,
               '. Processing...')
      message( '* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *' )
      cat( '---' )
    }
    # values = eval( substitute( unique( data$more_status ) ) )
    values = data[ , sort(unique(.SD[[1L]])), .SDcols = more_status]
    if ( verbose ) {
      message( 'adding expanded status flag' )
    }
    final[ status == state[[ 3 ]], status_exp := state[[ 3 ]] ]
    for ( i in seq_along( values ) ) {
      # final[ status != state[[ 3 ]] & get( more_status ) == values[ i ],
      #        status_exp := paste( values[ i ], '_', status, sep = '' ) ]
      final[ status != state[[ 3 ]] & final[ , .SD[[more_status]] ] == values[ i ],
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
             n_status_exp := paste(.SD[[n_events]], " ", status_exp, sep = '' ) ]
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


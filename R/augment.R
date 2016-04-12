augment = function( data, data_key, n_events, pattern, state = list ( 'IN', 'OUT', 'DEAD' ), 
                    t_start, t_end, t_cens, t_death, t_augmented = 'augmented',
                    status_more ) { 
  
  tic = proc.time()
  if ( require( "data.table" ) ) {
    cat( '---\n' )
    cat( "data.table has been loaded correctly", '\n' )
    cat( '---\n' )
  } else {
    cat( '---\n' )
    cat( "trying to install data.table", '\n' )
    install.packages( "data.table" )
    if ( require( data.table ) ) {
      cat( "data.table installed and loaded correctly", '\n' )
      cat( '---\n' )
    } else {
      stop( "could not install data.table" )
    }
  }
  if ( !inherits( data, "data.table" ) ) {
    stop( "data must be a data.table" )
  }
  if ( missing( data ) ) {
    stop( 'data is missing. Nothing to do' )
  }
  if ( missing( data_key ) || missing( n_events ) ) {
    stop( 'a variable of keying and a sequential event counter must be provided' )
  }
  if ( missing( pattern ) ) {
    stop( "a pattern must be provided" )
  }
  if ( !inherits( state, "list" ) ) {
    stop( "state pattern must be a list" )
  }
  if ( missing( t_start ) || missing( t_end ) ) {
    stop( 'augmented need a starting and an ending event time to work. Nothing to do.' )
  }
  if ( missing( t_death ) ) {
    warning( 'no t_death has been passed. Assuming that ', substitute( t_cens ), 
             ' contains both censoring and death time' )
  }
  
  message( 'Setting up everything to augment the long format' )
  cat( '---\n' )
  
  setkey( data, NULL )
  cols = as.character( substitute( list( data_key, n_events ) )[ -1L ] )
  if ( !length( cols ) )
    cols = colnames( data )
  setkeyv( data, cols )
  pattern = as.character( substitute( list( pattern ) )[ -1L ] )
  t_start = as.character( substitute( list( t_start ) )[ -1L ] )
  t_end = as.character( substitute( list( t_end ) )[ -1L ] )
  t_cens = as.character( substitute( list( t_cens ) )[ -1L ] )
  checks = c( cols, pattern, t_start, t_end )
  if ( !missing( t_death ) ) {
    t_death = as.character( substitute( list( t_death ) )[ -1L ] )
  }
  
  test = apply( data[ , checks, with = FALSE ], 2, function( x ) any( sum( is.na( x ) ) > 0 ) )
  if ( any ( test ) ) {
    cat( '---\n' )
    message( 'detected missing values in variables:' )
    invisible( sapply( names( test[ test == TRUE ] ), function( x ) cat( x, '\n' ) ) )
    stop( 'Please, fix the issues and relaunch augment()' )
  }
  
  l = lapply( 1:2, function( x ) data )  
  expand = rbindlist( l )
  setkeyv( expand, cols )
  values = sort( eval( substitute( unique( data$pattern ) ) ) )
  
  if ( length( values ) < 2 ) {
    stop( 'unit identification label must be an integer, a factor or a character 
            with at least 2 elements' )
  } else if ( length( values ) == 2 ) {
    message( 'detected only 2 values in ', substitute( pattern ) )
    cat( '---\n' )
    if ( inherits( eval( substitute( unique( data$pattern ) ) ), 'integer' ) || 
         inherits( eval( substitute( unique( data$pattern ) ) ), 'numeric' ) ) {
      match1 = data[ get( pattern ) == 0, .SD[ .N ], by = eval( cols[[ 1 ]] ) ]
      if ( missing( t_death ) ) {
        match3 = data[ get( pattern ) == 1, 
                       .SD[ .N ], by = eval( cols[[ 1 ]] ) ][ get( t_end ) != get( t_cens ) ]
      } else {
        match3 = data[ get( pattern ) == 1, 
                       .SD[ .N ], by = eval( cols[[ 1 ]] ) ][ get( t_end ) != get( t_death ) ]
      }
    } else if ( inherits( eval( substitute( unique( data$pattern ) ) ), 'factor' ) ) {
      match1 = data[ as.integer( get( pattern ) ) - 1 == 0, .SD[ .N ], by = eval( cols[[ 1 ]] ) ]
      if ( missing( t_death ) ) {
        match3 = data[ as.integer( get( pattern ) ) - 1 == 1, 
                       .SD[ .N ], by = eval( cols[[ 1 ]] ) ][ get( t_end ) != get( t_cens ) ]
      } else {
        match3 = data[ as.integer( get( pattern ) ) - 1 == 1, 
                       .SD[ .N ], by = eval( cols[[ 1 ]] ) ][ get( t_end ) != get( t_death ) ]
      }
    } else if ( inherits( eval( substitute( unique( data$pattern ) ) ), 'character' ) ) {
      match1 = data[ get( pattern ) == values[ 1 ], .SD[ .N ], by = eval( cols[[ 1 ]] ) ]
      if ( missing( t_death ) ) {
        match3 = data[ get( pattern ) == values[ 2 ], 
                       .SD[ .N ], by = eval( cols[[ 1 ]] ) ][ get( t_end ) != get( t_cens ) ]
      } else {
        match3 = data[ get( pattern ) == values[ 2 ], 
                       .SD[ .N ], by = eval( cols[[ 1 ]] ) ][ get( t_end ) != get( t_death ) ]
      }
    }
  } else if ( length( values ) == 3 ) { 
    message( 'detected 3 values in ', substitute( pattern ) )
    cat( '---\n' )
    if ( inherits( eval( substitute( unique( data$pattern ) ) ), 'integer' ) || 
         inherits( eval( substitute( unique( data$pattern ) ) ), 'numeric' ) ) {
      match1 = data[ get( pattern ) == 0, .SD[ .N ], by = eval( cols[[ 1 ]] ) ]
      match3 = data[ get( pattern ) == 2, .SD[ .N ], by = eval( cols[[ 1 ]] ) ]
    } else if ( inherits( eval( substitute( unique( data$pattern ) ) ), 'factor' ) ) {
      match1 = data[ as.integer( get( pattern ) ) - 1 == 0, .SD[ .N ], by = eval( cols[[ 1 ]] ) ]
      match3 = data[ as.integer( get( pattern ) ) - 1 == 2, .SD[ .N ], by = eval( cols[[ 1 ]] ) ]
    } else if ( inherits( eval( substitute( unique( data$pattern ) ) ), 'character' ) ) {
      match1 = data[ get( pattern ) == values[ 1 ], .SD[ .N ], by = eval( cols[[ 1 ]] ) ]
      match3 = data[ get( pattern ) == values[ 3 ], .SD[ .N ], by = eval( cols[[ 1 ]] ) ]
    }
  }
  l = list( expand, match1, match3 )
  final = rbindlist( l )
  setkeyv( final, cols )
  
  if ( length( values ) == 2 ) {
    if ( missing( t_death ) ) {
      counter_events = final[ , .( N = .N, j = unique( get( pattern ) ),
                                   t_end = max( get( t_end ) ),
                                   t_cens = max( get( t_cens ) ) ), by = eval( cols[[ 1 ]] ) ]
      .i = dim( counter_events )[ 1 ]
      status_flag_temp = vector( mode = 'list', dim( counter_events )[ 1 ] )
    } else {
      counter_events = final[ , .( N = .N, j = unique( get( pattern ) ),
                                   t_end = max( get( t_end ) ),
                                   t_death = max( get( t_death ) ) ), by = eval( cols[[ 1 ]] ) ]
      .i = dim( counter_events )[ 1 ]
      status_flag_temp = vector( mode = 'list', dim( counter_events )[ 1 ] )
    }
  } else if ( length( values ) == 3 ) {
    counter_events = data[ , .( N = .N, j = unique( get( pattern ) ) ), by = eval( cols[[ 1 ]] ) ]
    .i = dim( counter_events )[ 1 ]
    status_flag_temp = vector( mode = 'list', dim( counter_events )[ 1 ] )
  }
  
  message( 'adding status flag...' )
  for ( i in seq_along( counter_events$N ) ) {
    if ( .i > 10000 ) {
      if ( i %% 5000 == 0 ) { 
        cat( '* * * iteration', i, 'of', .i, '\n' )
      }
    } else {
      if ( i %% 1000 == 0 ) { 
        cat( '* * * iteration', i, 'of', .i, '\n' )
      }
    }
    if ( length( values ) == 2 ) {
      if ( missing( t_death ) ) {
        if ( counter_events$j[ i ] == values[ 1 ] ) {
          status_flag_temp[[ i ]] = c( rep( c( state[[ 1 ]], state[[ 2 ]] ), 
                                            ( floor( counter_events$N[ i ] / 2 ) ) ), 
                                       state [[ 2 ]] )
        } else if ( counter_events$j[ i ] == values[ 2 ] & 
                    counter_events$t_end[ i ] == counter_events$t_cens[ i ] ) {
          status_flag_temp[[ i ]] = c( rep( c( state[[ 1 ]], state[[ 2 ]] ), 
                                            ( floor( counter_events$N[ i ] / 2 ) - 1 ) ), 
                                       state[[ 1 ]], state[[ 3 ]] )
        } else if ( counter_events$j[ i ] == values[ 2 ] & 
                    counter_events$t_end[ i ] != counter_events$t_cens[ i ] ) {
          status_flag_temp[[ i ]] = c( rep( c( state[[ 1 ]], state[[ 2 ]] ), 
                                            ( floor( counter_events$N[ i ] / 2 ) ) ), state[[ 3 ]] )
        }
      } else {
        if ( counter_events$j[ i ] == values[ 1 ] ) {
          status_flag_temp[[ i ]] = c( rep( c( state[[ 1 ]], state[[ 2 ]] ), 
                                            ( floor( counter_events$N[ i ] / 2 ) ) ), 
                                       state [[ 2 ]] )
        } else if ( counter_events$j[ i ] == values[ 2 ] & 
                    counter_events$t_end[ i ] == counter_events$t_death[ i ] ) {
          status_flag_temp[[ i ]] = c( rep( c( state[[ 1 ]], state[[ 2 ]] ), 
                                            ( floor( counter_events$N[ i ] / 2 ) - 1 ) ), 
                                       state[[ 1 ]], state[[ 3 ]] )
        } else if ( counter_events$j[ i ] == values[ 2 ] & 
                    counter_events$t_end[ i ] != counter_events$t_death[ i ] ) {
          status_flag_temp[[ i ]] = c( rep( c( state[[ 1 ]], state[[ 2 ]] ), 
                                            ( floor( counter_events$N[ i ] / 2 ) ) ), state[[ 3 ]] )
        }
      }
    } else if ( length( values ) == 3 ) {
      if ( counter_events$j[ i ] == values[ 1 ] ) {
        status_flag_temp[[ i ]] = c( rep( c( state[[ 1 ]], state[[ 2 ]] ), 
                                          counter_events$N[ i ] ), state [[ 2 ]] )
      } else if ( counter_events$j[ i ] == values[ 2 ] ) {
        status_flag_temp[[ i ]] = c( rep( c( state[[ 1 ]], state[[ 2 ]] ), 
                                          ( counter_events$N[ i ] - 1 ) ), state[[ 1 ]], 
                                     state[[ 3 ]] )
      } else if ( counter_events$j[ i ] == values[ 3 ] ) {
        status_flag_temp[[ i ]] = c( rep( c( state[[ 1 ]], state[[ 2 ]] ), counter_events$N[ i ] ), 
                                     state[[ 3 ]] )
      }
    }
  }
  status_flag = unlist( status_flag_temp, recursive = FALSE )
  final[ , status := status_flag ]
  if ( sum( is.na( final$status ) ) == 0 ) {
    cat( 'status flag has been added successfully \n' )
    cat( '---\n' )
  } else {
    stop( 'status flag has not been build correctly' )
  }
  message( 'adding numeric status flag...' )
  k = length( unique( final$status ) )
  lev = unique( final$status )
  for ( i in 1:k ) {
    final[ status == lev[ i ], status_num := i ]
  }
  if ( i == k ) {
    cat( 'numeric status has been added successfully \n' )
    cat( '---\n' )
  } else {
    stop( 'numeric status status has not been build correctly' )
  }
  message( 'adding sequential status flag...' )
  final[ , n_status := ifelse( status != state[[ 3 ]], 
                               paste( eval( substitute( n_events ) ), ' ', status, sep = '' ), 
                               state[[ 3 ]] ) ]
  if ( sum( is.na( final$n_status ) ) == 0 ) {
    cat( 'sequential status flag has been added successfully \n' )
    cat( '---\n' )
  } else {
    stop( 'sequential status flag has not been build correctly' )
  }
  
  message( 'adding variable ', substitute( t_augmented ), ' as new time variable...' )
  final[ status == state[[ 1 ]], substitute( t_augmented ) := get( t_start ) ]
  final[ status == state[[ 2 ]], substitute( t_augmented ) := get( t_end ) ]
  if ( missing( t_death ) ) {
    final[ status == state[[ 3 ]], substitute( t_augmented ) := get( t_cens ) ]
  } else {
    final[ status == state[[ 3 ]], substitute( t_augmented ) := get( t_death ) ]
  }
  
  id_col = which( names( data ) == substitute( t_start ) )
  setcolorder( final, c( 1:( id_col - 1 ), dim( final )[ 2 ], 
                         id_col:( dim( final )[ 2 ] - 1 ) ) )
  cat( 'variable \"', substitute( t_augmented ), 
       '\" successfully added and repositioned\n', sep = '' )
  cat( '---\n' )
  
  if ( !missing( status_more ) ) {
    status_more = as.character( substitute( list( status_more )  )[ -1L ] )
    test = apply( data[ , status_more, with = FALSE ], 2, 
                  function( x ) any( sum( is.na( x ) ) > 0 ) )
    if ( any ( test ) ) {
      cat( '---\n' )
      message( 'detected missing values in variable:' )
      invisible( sapply( names( test[ test == TRUE ] ), function( x ) cat( x, '\n' ) ) )
      stop( 'Please, fix the issues and relaunch augment()' )
    }
    
    values = eval( substitute( unique( data$status_more ) ) )
    message( 'adding expanded status flag...' )
    final[ status == state[[ 3 ]], status_exp := state[[ 3 ]] ]
    for ( i in seq_along( values ) ) {
      final[ status != state[[ 3 ]] & get( status_more ) == values[ i ], 
             status_exp := paste( values[ i ], '_', status, sep = '' ) ]
    }
    if ( sum( is.na( final$status_exp ) ) == 0 ) {
      cat( 'expanded status flag has been added successfully \n' )
      cat( '---\n' )
    } else {
      stop( 'expanded status flag has not been build correctly' )
    }
    message( 'adding numeric expanded status flag...' )
    k = length( unique( final$status_exp ) )
    lev = unique( final$status_exp )
    for ( i in 1:k ) {
      final[ status_exp == lev[ i ], status_exp_num := i ]
    }
    if ( i == k ) {
      cat( 'expanded numeric status has been added successfully \n' )
      cat( '---\n' )
    } else {
      stop( 'expanded numeric status has not been build correctly' )
    }
    message( 'adding sequential expanded status flag...' )
    final[ , n_status_exp := ifelse( status_exp != state[[ 3 ]], 
                                     paste( eval( substitute( n_events ) ), ' ', 
                                            status_exp, sep = '' ), state[[ 3 ]] ) ]
    if ( sum( is.na( final$n_status_exp ) ) == 0 ) {
      cat( 'expanded sequential status flag has been added successfully \n' )
    } else {
      stop( 'expanded sequential status flag has not been build correctly' )
    }
  }
  
  toc = proc.time()
  time = toc - tic 
  cat( '---------------------------\n' )
  cat( 'Function took:', time[ 3 ], 'sec. \n', sep = ' ' )
  cat( '---------------------------\n' )
  return( invisible( final ) )
}


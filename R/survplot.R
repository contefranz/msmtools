survplot = function( x, km = FALSE, from = 1, to = NULL, range = NULL, covariates, 
                     exact.times = TRUE, grid = 100L, return.km = FALSE, add = FALSE,
                     ci = c( "none", "normal", "bootstrap" ), 
                     interp = c( "start", "midpoint" ), B = 100L, legend.pos = 'topright', 
                     xlab = "Time", ylab = "Survival Probability", 
                     lty.fit = 1, lwd.fit = 1, col.fit = "red", lty.ci.fit = 3, lwd.ci.fit = 1, 
                     col.ci.fit = col.fit, mark.time = FALSE, col.km = "darkblue", 
                     lty.km = 5, lwd.km = 1 ) {
  
  message( '------------------------------------------------------------------' ) 
  message( 'survplot requires \"data.table\", \"msm\", and \"survival\" to be loaded' )  
  message( '------------------------------------------------------------------' ) 
  
  time.start = proc.time()
  
  if ( !inherits( x, "msm" ) ) 
    stop( "x must be a msm model" )
  if ( !is.numeric( from ) )
    stop( 'from must be numeric' )
  if ( is.null( to ) ) 
    to = max( absorbing.msm( x ) )
  else {
    if ( !is.numeric( to ) ) 
      stop( "to must be numeric" )
    if ( !( to %in% absorbing.msm( x ) ) ) 
      stop( "to must be an absorbing state" )
  }
  if ( is.null( range ) ) 
    rg = range( model.extract( x$data$mf, "time" ) )
  else {
    if ( !is.numeric( range ) || length( range ) != 2 ) 
      stop( "range must be a numeric vector of two elements" )
    rg = range
  }

  interp = match.arg( interp )
  ci = match.arg( ci )
  timediff = ( rg[ 2 ] - rg[ 1 ] ) / grid
  if ( exact.times == TRUE ) {
    times = seq( 1, diff( rg ), timediff )
  } else { 
    times = seq( rg[ 1 ], rg[ 2 ], timediff )
  }
  
  pr = lower = upper = numeric()
  counter = 0L
  for ( t in times ) {
    
    counter = counter + 1
    if ( counter %% 10 == 0 ) {
      cat( '---\n' )
      cat( 't =', round( t, 0 ), '\n' )
    }
    
    P = pmatrix.msm( x, t, t1 = times[ 1 ], covariates = covariates, ci = ci, B = B )
    
    if ( ci != "none" ) {
      pr = c( pr, P$estimates[ from, to ] )
      lower = c( lower, P$L[ from, to ] )
      upper = c( upper, P$U[ from, to ] )
    }
    else pr = c( pr, P[ from, to ] )
  }
  if ( add == FALSE ) {
    plot( times, 1 - pr, type = "l", xlab = xlab, ylab = ylab, ylim = c( 0, 1 ),
          lwd = lwd.fit, lty = lty.fit, col = col.fit )
  } else {
    lines( times, 1 - pr, lwd = lwd.fit, lty = lty.fit, col = col.fit )
  }
  if ( ci != "none" ) {
    lines( times, 1 - lower, lwd = lwd.ci.fit, lty = lty.ci.fit, col = col.ci.fit )
    lines( times, 1 - upper, lwd = lwd.ci.fit, lty = lty.ci.fit, col = col.ci.fit )
  }
  if ( km == TRUE ) {
    # if ( franz == FALSE ) {
    #   dat = x$data$mf[ , c("(subject)", "(time)", "(state)" ) ]
    #   wide = as.data.table( do.call( "rbind",
    #                                  by( dat, dat$"(subject)",
    #                                      function( x ) {
    #                                        dind = which( x[ , "(state)" ] == to )
    #                                        if ( any( x[ , "(state)" ] == to ) )
    #                                          mintime = if ( interp == "start" )
    #                                            min( x[ dind, "(time)" ] )
    #                                        else 0.5 * ( x[ dind, "time" ] + x[ dind - 1, "time" ] )
    #                                        else mintime = max( x[ , "(time)" ] )
    #                                        c( anystate = as.numeric( any( x[ , "(state)" ] == to ) ),
    #                                           mintime = mintime )
    #                                      } ) ) )
    # }
    dat = as.data.table( x$data$mf[ , c( "(subject)", "(time)", "(state)" ) ] )
    setnames( dat, c( 'subject', 'time', 'state' ) )
    absind = which( dat$state == to ) 
    if ( any( dat[ state == to ] ) ) {
      if ( interp == 'start' ) {
        mintime = dat[ absind, min( time ), by = subject ]
      } else if ( interp == 'midpoint' ) { 
        mintime = 0.5 * ( dat[ absind, .( time ), by = subject ] + 
                            dat[ absind - 1, .( time ), by = subject ] ) 
      } else { 
        mintime = dat[ , max( time ), by = subject ]
      }
      wide = data.table( mintime = mintime, 
                         anystate = as.numeric( any( dat[ state == to, .( state ) ] ) ) 
      )
      setnames( wide, c( 'subject', 'mintime', 'anystate' ) )
    }
    if ( add == FALSE ) {
      if ( exact.times == FALSE ) {
        lines( survfit( Surv( wide$mintime, wide$anystate ) ~ 1 ), mark.time = mark.time, 
               col = col.km, lty = lty.km, lwd = lwd.km )
      } else {
        wide[ , mintime_exact := mintime - min( mintime ) ]
        lines( survfit( Surv( wide$mintime_exact, wide$anystate ) ~ 1 ), mark.time = mark.time, 
               col = col.km, lty = lty.km, lwd = lwd.km )
      }
      legend( legend.pos, legend = c( "Fitted (solid)", 'Kaplan-Meier (dashed)' ), cex = 0.8 )
    }
  }
  if ( return.km == TRUE ) {
    assign( paste( 'survival_data' ), wide, envir = .GlobalEnv )
  }
  time.end = proc.time()
  time.total = time.end - time.start
  cat( '---\n' )
  cat( 'Function took:', time.total[ 3 ], '\n' )
  cat( '---\n' )
}




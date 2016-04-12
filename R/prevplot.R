prevplot = function( x, prev.obj, M, ci = FALSE, scale = TRUE, grid = 100L, 
                     x.lab.grid = 500L, xlab = 'Time', ylab = 'Prevalence (%)',
                     col.obs = 'darkblue', col.obs.ci = 'darkblue', 
                     lwd.obs = 1, lty.obs = 1, lty.obs.ci = 2, 
                     col.fit = 'red', col.fit.ci = 'red', lty.fit = 1, lty.fit.ci = 2,
                     legend.show = FALSE, par.col = 3, plot.width = 10, plot.height = 5 ) {
  
  message( '------------------------------------' ) 
  message( 'prevplot requires \"msm\" to be loaded' )  
  message( '------------------------------------' ) 
  
  if ( !inherits( x, "msm" ) ) 
    stop( "x must be a msm model" )
  if ( !inherits( prev.obj, "list" ) ) 
    stop( "prev.obj must be a list computed by \"prevalence.msm\"" )
  
  t_min = range( model.extract( x$data$mf, "time" ) )[ 1 ]
  t_max = range( model.extract( x$data$mf, "time" ) )[ 2 ]
  x_axis = seq( t_min, t_max, grid ) 
  x_axis_scaled = seq( t_min, t_max, grid ) - t_min
  abs_state = as.integer( absorbing.msm( x ) )
  status_names = colnames( x$qmodel$imatrix )
  
  if ( Sys.info()[ 'sysname' ] == 'Darwin' )
    quartz( width = plot.width, height = plot.height )
  else x11( width = plot.width, height = plot.height )  
  if ( abs_state <= par.col ) {
    n_row = 1
  } else {
    n_row = ceiling( abs_state / par.col )
  }
  if ( length( prev.obj$Expected ) == 2 ) {
    par( mfrow = c( n_row, par.col ) )
    
    if ( scale == FALSE ) {
      for ( i in 1:abs_state ) {
        plot( x_axis, prev.obj$`Observed percentages`[ , i ], type = 'l', xaxt = 'n',
              col = col.obs, lty = lty.obs, lwd = lwd.obs, 
              main = paste( 'State ', status_names[ i ], sep = '' ),
              xlab = xlab, ylab = ylab, ylim = c( 0, 100 ) ) 
        axis( side = 1, at = seq( min( x_axis ), max( x_axis ), x.lab.grid ) )
        lines( x_axis, prev.obj$`Expected percentages`$estimates[ , i ], type = 'l', 
               lwd = lwd.obs, lty = lty.fit, col = col.fit ) 
        if ( ci == TRUE ) {
          lines( x_axis, prev.obj$`Expected percentages`$ci[ , i, 1 ], type = 'l', 
                 lwd = lwd.obs, lty = lty.fit.ci, col = col.fit.ci ) 
          lines( x_axis, prev.obj$`Expected percentages`$ci[ , i, 2 ], type = 'l', 
                 lwd = lwd.obs, lty = lty.fit.ci, col = col.fit.ci ) 
        }
        if ( legend.show == TRUE ) {
          legend( 'topright', legend = c( 'Fitted', 'Observed' ), 
                  lty = c( lty.fit, lty.obs ), col = c( col.fit, col.obs ) )
        }
      }
    } else {
      for ( i in 1:abs_state ) {
        plot( x_axis_scaled, prev.obj$`Observed percentages`[ , i ], type = 'l', xaxt = 'n',
              col = col.obs, lty = lty.obs, lwd = lwd.obs, 
              main = paste( 'State ', status_names[ i ], sep = '' ),
              xlab = xlab, ylab = ylab, ylim = c( 0, 100 ) ) 
        axis( side = 1, at = seq( min( x_axis_scaled ), max( x_axis_scaled ), x.lab.grid ) )
        lines( x_axis_scaled, prev.obj$`Expected percentages`$estimates[ , i ], type = 'l', 
               lwd = lwd.obs, lty = lty.fit, col = col.fit ) 
        if ( ci == TRUE ) {
          lines( x_axis_scaled, prev.obj$`Expected percentages`$ci[ , i, 1 ], type = 'l', 
                 lwd = lwd.obs, lty = lty.fit.ci, col = col.fit.ci ) 
          lines( x_axis_scaled, prev.obj$`Expected percentages`$ci[ , i, 2 ], type = 'l', 
                 lwd = lwd.obs, lty = lty.fit.ci, col = col.fit.ci ) 
        }
        if ( legend.show == TRUE ) {
          legend( 'topright', legend = c( 'Fitted', 'Observed' ), 
                  lty = c( lty.fit, lty.obs ), col = c( col.fit, col.obs ) )
        }
      }
    }
  } else {
    if ( ci == TRUE ) {
      message( substitute( prev.obj ), ' has no confidence intervals. 
               Argument \"ci\" will be ignored' )
    }
    par( mfrow = c( n_row, par.col ) )
    
    if ( scale == FALSE ) { 
      for ( i in 1:abs_state ) {
        plot( x_axis, prev.obj$`Observed percentages`[ , i ], type = 'l', xaxt = 'n',
              col = col.obs, lty = lty.obs, lwd = lwd.obs,
              main = paste( 'State ', status_names[ i ], sep = '' ),
              xlab = xlab, ylab = ylab, ylim = c( 0, 100 ) )
        axis( side = 1, at = seq( min( x_axis ), max( x_axis ), x.lab.grid ) )
        lines( x_axis, prev.obj$`Expected percentages`[ , i ], type = 'l',
               lwd = lwd.obs, lty = lty.fit, col = col.fit )
        if ( legend.show == TRUE ) {
          legend( 'topright', legend = c( 'Fitted', 'Observed' ), 
                  lty = c( lty.fit, lty.obs ), col = c( col.fit, col.obs ) )
        }
      }
    } else {
      for ( i in 1:abs_state ) {
        plot( x_axis_scaled, prev.obj$`Observed percentages`[ , i ], type = 'l', xaxt = 'n',
              col = col.obs, lty = lty.obs, lwd = lwd.obs,
              main = paste( 'State ', status_names[ i ], sep = '' ),
              xlab = xlab, ylab = ylab, ylim = c( 0, 100 ) )
        axis( side = 1, at = seq( min( x_axis_scaled ), max( x_axis_scaled ), x.lab.grid ) )
        lines( x_axis_scaled, prev.obj$`Expected percentages`[ , i ], type = 'l',
               lwd = lwd.obs, lty = lty.fit, col = col.fit )
        if ( legend.show == TRUE ) {
          legend( 'topright', legend = c( 'Fitted', 'Observed' ), 
                  lty = c( lty.fit, lty.obs ), col = c( col.fit, col.obs ) )
        }
      }
    }
  }
  if ( !missing( M ) ) {
    if ( !inherits( M, "matrix" ) ) 
      stop( "M must be a matrix" )
    colnames( M )[ 1:abs_state ] = status_names
    M[ is.na( M ) ] = 0
    temp = sort( c( M ) )
    y.max.M = temp[ length( temp ) - 1 ] + 0.1 * temp[ length( temp ) - 1 ]
    
    if ( Sys.info()[ 'sysname' ] == 'Darwin' )
      quartz( width = plot.width, height = plot.height )
    else x11( width = plot.width, height = plot.height )  
    par( mfrow = c( n_row, par.col ) )
    
    if ( scale == FALSE ) {
      for ( i in 1:abs_state ) {
        plot( x_axis, M[ , i ], type = 'l', xaxt = 'n',
              main = paste( 'M for state ', status_names[ i ], sep = '' ),
              xlab = xlab, ylab = 'M', ylim = c( 0, y.max.M ) ) 
        axis( side = 1, at = seq( min( x_axis ), max( x_axis ), x.lab.grid ) )
      }
    } else {
      for ( i in 1:abs_state ) {
        plot( x_axis_scaled, M[ , i ], type = 'l', xaxt = 'n',
              main = paste( 'M for state ', status_names[ i ], sep = '' ),
              xlab = xlab, ylab = 'M', ylim = c( 0, y.max.M ) ) 
        axis( side = 1, at = seq( min( x_axis_scaled ), max( x_axis_scaled ), x.lab.grid ) )
      }
    }
  }
}



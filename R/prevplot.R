#' Plot observed and expected prevalences for a multi-state model.
#'
#' Provides a graphical indication of goodness of fit of a multi-state model computed by
#' \code{\link[msm]{msm}}. It also computes a rough indicator of where the data depart from the
#' fitted Markov model.
#'
#' @param x A \code{msm} object.
#' @param prev.obj A list computed by \code{\link[msm]{prevalence.msm}}. It can be with or without
#' confidence intervals. \code{prevplot} will behaves accordingly.
#' @param M If \code{TRUE}, then a rough indicator of deviance from the model is computed
#' (see 'Details'). Default is \code{FALSE}.
#' @param exacttimes If \code{TRUE} (default) then transition times are known and exact. This
#' is inherited from \code{msm} and should be set the same way.
#' @param ci If \code{TRUE}, then confidence intervals, if they exist, are plotted.
#' Default is \code{FALSE}.
#' @param grid Define how many points should be used to build the \emph{x} axis. Defaul is 100.
#' @param x.lab.grid Define the interval on the \emph{x} axis at which draw tick marks. Default
#' is 500.
#' @param xlab \emph{x} axis label.
#' @param ylab \emph{y} axis label.
#' @param lty.fit Line type for the expected prevalences. See \code{\link[graphics]{par}}.
#' @param lwd.fit Line width for the expected prevalences. See \code{\link[graphics]{par}}.
#' @param col.fit Line color for the expected prevalences. See \code{\link[graphics]{par}}.
#' @param lty.ci.fit Line type for the expected prevalences confidence limits.
#' See \code{\link[graphics]{par}}.
#' @param lwd.ci.fit Line width for the expected prevalences confidence limits.
#' See \code{\link[graphics]{par}}.
#' @param col.ci.fit Line color for the expected prevalences confidence limits.
#' See \code{\link[graphics]{par}}.
#' @param lty.obs Line type for the observed prevalences. See \code{\link[graphics]{par}}.
#' @param lwd.obs Line width for the observed prevalences. See \code{\link[graphics]{par}}.
#' @param col.obs Line color for the observed prevalences. See \code{\link[graphics]{par}}.
#' @param legend.pos Where to position the legend. Default is \code{"topright"}, but \emph{x} and
#' \emph{y} coordinate can be passed. If \code{NULL}, then legend is not shown.
#' @param par.col The number of columns of the plot. Default is 3.
#' @param plot.width Width of new graphical device. Default is 7. See \code{\link[graphics]{par}}.
#' @param plot.height Height of new graphical device. Default is 7. See \code{\link[graphics]{par}}.
#' @param max.m If \code{M = TRUE}, it adjusts the upper \emph{y} limit when plotting M.
#' @param devnew Set the graphical device where to plot. By default, \code{prevplot} plots on a new
#' device by setting \code{dev.new}. If \code{FALSE}, then a plot is drawn onto the current device
#' as specified by \code{dev.cur}. If \code{FALSE} and no external devices are opened, then
#' a plot is drawn using internal graphics. See \code{\link[grDevices]{dev}}.
#' @details When \code{M = TRUE}, a rough indicator of the deviance from the Markov model is
#' computed according to Titman and Sharples (2008). A comparison at a given time \eqn{t_i} of a
#' patient \emph{k} in the state \emph{s} between observed counts \eqn{O_{is}}
#' with expected ones \eqn{E_{is}} is build as follows:
#' \deqn{M_{is} = \frac{(O_{is} - E_{is})^2}{E_{is}}}{ (O_{is} - E_{is})^2 / E_{is} }
#' @seealso \code{\link[msm]{plot.prevalence.msm}}
#' @references Titman, A. and Sharples, L.D. (2008). A general goodness-of-fit test for Markov and
#' hidden Markov models, \emph{Statistics in Medicine}, 27, 2177-2195.
#' @author Francesco Grossetti \email{francesco.grossetti@@polimi.it}.
#' @import data.table
#' @export
prevplot = function( x, prev.obj, M = FALSE, exacttimes = TRUE, ci = FALSE, grid = 100L,
                     x.lab.grid = 500L, xlab = 'Time', ylab = 'Prevalence (%)',
                     lty.fit = 1, lwd.fit = 1, col.fit = 'red',
                     lty.ci.fit = 2, lwd.ci.fit = 1, col.ci.fit = col.fit,
                     lwd.obs = 1, lty.obs = 1, col.obs = 'darkblue',
                     legend.pos = 'topright', par.col = 3, plot.width = 10, plot.height = 5,
                     max.m = 0.1, devnew = TRUE ) {

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

  if ( devnew == TRUE ) {
    dev.new( noRStudioGD = TRUE, width = plot.width, height = plot.height )
  } else if ( devnew == FALSE ) {
    dev.set( dev.cur() )
  }
  if ( abs_state <= par.col ) {
    n_row = 1
  } else {
    n_row = ceiling( abs_state / par.col )
  }
  if ( length( prev.obj$Expected ) == 2 ) {
    par( mfrow = c( n_row, par.col ) )

    if ( exacttimes == FALSE ) {
      for ( i in 1:abs_state ) {
        plot( x_axis, prev.obj$`Observed percentages`[ , i ], type = 'l', xaxt = 'n',
              col = col.obs, lty = lty.obs, lwd = lwd.obs,
              main = paste( 'State ', status_names[ i ], sep = '' ),
              xlab = xlab, ylab = ylab, ylim = c( 0, 100 ) )
        axis( side = 1, at = seq( min( x_axis ), max( x_axis ), x.lab.grid ) )
        lines( x_axis, prev.obj$`Expected percentages`$estimates[ , i ], type = 'l',
               lwd = lwd.fit, lty = lty.fit, col = col.fit )
        if ( ci == TRUE ) {
          lines( x_axis, prev.obj$`Expected percentages`$ci[ , i, 1 ], type = 'l',
                 lwd = lwd.ci.fit, lty = lty.ci.fit, col = col.ci.fit )
          lines( x_axis, prev.obj$`Expected percentages`$ci[ , i, 2 ], type = 'l',
                 lwd = lwd.ci.fit, lty = lty.ci.fit, col = col.ci.fit )
        }
        if ( !is.null( legend.pos ) ) {
          legend( legend.pos, legend = c( 'Fitted', 'Observed' ),
                  lwd = c( lwd.fit, lwd.obs ), lty = c( lty.fit, lty.obs ),
                  col = c( col.fit, col.obs ) )
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
               lwd = lwd.fit, lty = lty.fit, col = col.fit )
        if ( ci == TRUE ) {
          lines( x_axis_scaled, prev.obj$`Expected percentages`$ci[ , i, 1 ], type = 'l',
                 lwd = lwd.ci.fit, lty = lty.ci.fit, col = col.ci.fit )
          lines( x_axis_scaled, prev.obj$`Expected percentages`$ci[ , i, 2 ], type = 'l',
                 lwd = lwd.ci.fit, lty = lty.ci.fit, col = col.ci.fit )
        }
        if ( !is.null( legend.pos ) ) {
          legend( legend.pos, legend = c( 'Fitted', 'Observed' ),
                  lwd = c( lwd.fit, lwd.obs ), lty = c( lty.fit, lty.obs ),
                  col = c( col.fit, col.obs ) )
        }
      }
    }
  } else {
    if ( ci == TRUE ) {
      message( substitute( prev.obj ), ' has no confidence intervals. Argument ci will be ignored.' )
    }
    par( mfrow = c( n_row, par.col ) )
    if ( exacttimes == FALSE ) {
      for ( i in 1:abs_state ) {
        plot( x_axis, prev.obj$`Observed percentages`[ , i ], type = 'l', xaxt = 'n',
              col = col.obs, lty = lty.obs, lwd = lwd.obs,
              main = paste( 'State ', status_names[ i ], sep = '' ),
              xlab = xlab, ylab = ylab, ylim = c( 0, 100 ) )
        axis( side = 1, at = seq( min( x_axis ), max( x_axis ), x.lab.grid ) )
        lines( x_axis, prev.obj$`Expected percentages`[ , i ], type = 'l',
               lwd = lwd.fit, lty = lty.fit, col = col.fit )
        if ( !is.null( legend.pos ) ) {
          legend( legend.pos, legend = c( 'Fitted', 'Observed' ),
                  lwd = c( lwd.fit, lwd.obs ), lty = c( lty.fit, lty.obs ),
                  col = c( col.fit, col.obs ) )
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
               lwd = lwd.fit, lty = lty.fit, col = col.fit )
        if ( !is.null( legend.pos ) ) {
          legend( legend.pos, legend = c( 'Fitted', 'Observed' ),
                  lwd = c( lwd.fit, lwd.obs ), lty = c( lty.fit, lty.obs ),
                  col = c( col.fit, col.obs ) )
        }
      }
    }
  }
  if ( M == TRUE ) {
    if ( length( prev.obj$Expected ) == 2 ) {
      M.obj = ( prev.obj$Observed - prev.obj$Expected$estimates )^2 / prev.obj$Expected$estimates
    } else {
      M.obj = ( prev.obj$Observed - prev.obj$Expected )^2 / prev.obj$Expected
    }

    if ( !inherits( M.obj, "matrix" ) )
      stop( "M must be a matrix" )
    colnames( M.obj )[ 1:abs_state ] = status_names
    M.obj[ is.na( M.obj ) ] = 0
    temp = sort( c( M.obj ) )
    y.max.M = temp[ length( temp ) - 1 ] + max.m * temp[ length( temp ) - 1 ]

    dev.new( noRStudioGD = TRUE, width = plot.width, height = plot.height )
    par( mfrow = c( n_row, par.col ) )
    if ( exacttimes == FALSE ) {
      for ( i in 1:abs_state ) {
        plot( x_axis, M.obj[ , i ], type = 'l', xaxt = 'n',
              main = paste( 'M for state ', status_names[ i ], sep = '' ),
              xlab = xlab, ylab = 'M', ylim = c( 0, y.max.M ) )
        axis( side = 1, at = seq( min( x_axis ), max( x_axis ), x.lab.grid ) )
      }
    } else {
      for ( i in 1:abs_state ) {
        plot( x_axis_scaled, M.obj[ , i ], type = 'l', xaxt = 'n',
              main = paste( 'M for state ', status_names[ i ], sep = '' ),
              xlab = xlab, ylab = 'M', ylim = c( 0, y.max.M ) )
        axis( side = 1, at = seq( min( x_axis_scaled ), max( x_axis_scaled ), x.lab.grid ) )
      }
    }
  }
}



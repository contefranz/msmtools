#' Plot and get survival data from a multi-state model
#'
#' Plot a Kaplan-Meier curve and compare it with the fitted survival probability computed from a
#' \code{\link[msm]{msm}} model. Fast builds and returns the associated datasets.
#'
#' @param x A \code{msm} object.
#' @param from State from which to compute the estimated survival. Default to state 1.
#' @param to The absorbing state to which compute the estimated survival. Default to the highest
#' state found by \code{\link[msm]{absorbing.msm}}.
#' @param range A numeric vector of two elements which gives the time range of the plot.
#' @param covariates Covariate values for which to evaluate the expected probabilities.
#' This can either be: the string \code{"mean"}, denoting the means of the covariates in the data
#' (this is the default), the number 0, indicating that all the covariates should be set to zero,
#' or a list of values, with optional names. For example:\cr
#' \code{list (75, 1)}\cr
#' where the order of the list follows the order of the covariates originally given in the
#' model formula, or a named list:\cr
#' \code{list (age = 75, gender = "M")}.
#' @param exacttimes If \code{TRUE} (default) then transition times are known and exact. This
#' is inherited from \code{msm} and should be set the same way.
#' @param times An optional numeric vector giving the times at which to compute the fitted survival.
#' @param grid An integer which tells at how many points to compute the fitted survival (See 'Details').
#' If \code{times} is passed, \code{grid} is ignored. It has a default of 100 points.
#' @param km If \code{TRUE}, then the Kaplan-Meier curve is shown. Default is \code{FALSE}.
#' @param return.all If \code{TRUE}, then all the datasets used to draw the plot will be return to
#' the environment. This argument saves you some typing time since you do not have to pass neither
#' \code{return.km} nor \code{return.p}. Default is \code{FALSE} (See 'Details').
#' @param return.km If \code{TRUE}, then the dataset used for building the Kaplan-Meier is returned
#' as an object of class \code{data.table} unless \code{convert} is set to \code{TRUE}
#' (See \code{convert}). Default is \code{FALSE}.
#' \code{survplot} must be assigned to an object in order to get the data in the environment
#' (see 'Details').
#' @param return.p If \code{TRUE}, then the dataset used for building the fitted survival curve
#' is returned as an object of class \code{data.table} unless \code{convert} is set to \code{TRUE}
#' (See \code{convert}). Default is \code{FALSE}.
#' \code{survplot} must be assigned to an object in order to get the data in the environment
#' (see 'Details').
#' @param convert If \code{TRUE}, then any returned object is automatically converted to the
#' class \code{data.frame}. This is done in place and comes at very low cost both from running time
#' and memory consumption (See \code{\link[data.table]{setDF}}).
#' @param add If \code{TRUE}, then a new layer is added to the current plot. Default is \code{FALSE}.
#' @param ci If \code{"none"} (the default), then no confidence intervals are plotted.
#' If \code{"normal"} or \code{"bootstrap"}, confidence intervals are plotted based on the
#' respective method in \code{\link[msm]{pmatrix.msm}}. This is very computationally-intensive,
#' since intervals must be computed at a series of times.
#' @param interp If \code{"start"} (the default), then the entry time into the absorbing state
#' is assumed to be the time it is first observed in the data. If \code{"midpoint"}, then the
#' entry time into the absorbing state is assumed to be halfway between the time it is first
#' observed and the previous observation time. This is generally more reasonable for "progressive"
#' models with observations at arbitrary times.
#' @param B Number of bootstrap or normal replicates for the confidence interval. The default is
#' 100 rather than the usual 1000, since these plots are for rough diagnostic purposes.
#' @param legend.pos Where to position the legend. Default is \code{"topright"}, but \emph{x} and
#' \emph{y} coordinate can be passed. If \code{NULL}, then legend is not shown.
#' @param xlab \emph{x} axis label.
#' @param ylab \emph{y} axis label.
#' @param lty.fit Line type for the fitted curve. See \code{\link[graphics]{par}}.
#' @param lwd.fit Line width for the fitted curve. See \code{\link[graphics]{par}}.
#' @param col.fit Line color for the fitted curve. See \code{\link[graphics]{par}}.
#' @param lty.ci.fit Line type for the fitted curve confidence limits.
#' See \code{\link[graphics]{par}}.
#' @param lwd.ci.fit Line width for the fitted curve confidence limits.
#' See \code{\link[graphics]{par}}.
#' @param col.ci.fit Line color for the fitted curve confidence limits.
#' See \code{\link[graphics]{par}}.
#' @param mark.time Mark the empirical survival curve at each censoring point.
#' See \code{\link[survival]{lines.survfit}}.
#' @param lty.km Line type for the Kaplan-Meier passed to \code{\link[survival]{lines.survfit}}.
#' See \code{\link[graphics]{par}}.
#' @param lwd.km Line width for the Kaplan-Meier passed to \code{\link[survival]{lines.survfit}}.
#' See \code{\link[graphics]{par}}.
#' @param col.km Line color for the Kaplan-Meier passed to \code{\link[survival]{lines.survfit}}.
#' See \code{\link[graphics]{par}}.
#' @param do.plot If \code{FALSE}, then no plot is shown at all. Default is \code{TRUE}.
#' @param plot.width Width of new graphical device. Default is 7. See \code{\link[graphics]{par}}.
#' @param plot.height Height of new graphical device. Default is 7. See \code{\link[graphics]{par}}.
#' @param devnew Set the graphical device where to plot. By default, \code{survplot} plots on a new
#' device by setting \code{dev.new}. If \code{FALSE}, then a plot is drawn onto the current device
#' as specified by \code{dev.cur}. If \code{FALSE} and no external devices are opened, then
#' a plot is drawn using internal graphics. See \code{\link[grDevices]{dev}}.
#' @param verbose If \code{FALSE}, all information produced by \code{print}, \code{cat} and
#' \code{message} are suppressed. All is done internally so that no global
#' options are changed. \code{verbose} can be set to \code{FALSE} on all common OS
#' (see also \code{\link[base]{sink}} and \code{\link[base]{options}}). Default is \code{TRUE}.
#' @details The function is a wrapper of \code{\link[msm]{plot.survfit.msm}} and does more things.
#' \code{survplot} manages correctly the plot of a fitted survival in
#' an exact times framework (when \code{exacttimes = TRUE}) by just resetting the time scale
#' and looking at the follow-up time.
#' It can fastly build and return to the user the datasets used to compute the Kaplan-Meier
#' and the fitted survival by setting \code{return.all = TRUE}. When this is \code{TRUE}, setting
#' \code{return.km} or \code{return.p} to \code{FALSE} produces an error and \code{survplot} does not
#' conclude the job. If these are set to \code{TRUE}, a warning is raised but the job is taken
#' to the end. For more details about how \code{survplot} returns objects,
#' please refer to the vignette with \code{vignette("msmtools")}.
#'
#' The user can defined custom times (through \code{times}) or let \code{survplot} choose
#' them on its own (through \code{grid}). In the latter case, \code{survplot} looks for the follow-up
#' time and divides it by \code{grid}. The higher it is, the finer the grid will be so that computing
#' the fitted survival will take longer, but will be more precise.
#' @return If \code{return.all} is set to \code{TRUE}, then \code{survplot}
#' returns a named list with \code{$km} and \code{$fitted} as \code{data.table} or as \code{data.frame}
#' when \code{convert = TRUE}. To save them in the
#' working environment, assign \code{survplot} to an object (see 'Examples')\cr
#'
#' ------\cr
#' \code{$km} contains up to 4 columns:\cr
#' \emph{subject}: the ordered subject ID as passed in the \code{msm} function.\cr
#' \emph{mintime}: the times at which to compute the fitted survival.\cr
#' \emph{mintime_exact}: if \code{exacttimes} is \code{TRUE}, then the relative timing is reported.\cr
#' \emph{anystate}: state of transition to compute the Kaplan-Meier.\cr
#' ------\cr
#' \code{$fitted} contains 2 columns:\cr
#' \emph{time}: times at which to compute the fitted survival.\cr
#' \emph{probs}: the corresponding values of the fitted survival.\cr
#' @examples
#' \dontrun{
#' data( hosp )
#'
#' # augmenting the data
#' hosp_augmented = augment( data = hosp, data_key = subj, n_events = adm_number, pattern = label_3,
#'                           t_start = dateIN, t_end = dateOUT, t_cens = dateCENS )
#'
#' # let's define the initial transition matrix for our model
#' Qmat = matrix( data = 0, nrow = 3, ncol = 3, byrow = TRUE )
#' Qmat[ 1, 1:3 ] = 1
#' Qmat[ 2, 1:3 ] = 1
#' colnames( Qmat ) = c( 'IN', 'OUT', 'DEAD' )
#' rownames( Qmat ) = c( 'IN', 'OUT', 'DEAD' )
#'
#' # attaching the msm package and running the model using
#' # gender and age as covariates
#' library( msm )
#' msm_model = msm( status_num ~ augmented_int,
#'                  subject = subj, data = hosp_augmented, covariates = ~ gender + age,
#'                  exacttimes = TRUE, gen.inits = TRUE, qmatrix = Qmat, method = 'BFGS',
#'                  control = list( fnscale = 6e+05, trace = 0,
#'                  REPORT = 1, maxit = 10000 ) )
#'
#' # plotting the fitted and empirical survival from state = 1
#' survplot( msm_model, km = TRUE, ci = 'none',
#'           verbose = FALSE )
#'
#' # plotting the fitted and empirical survival from state = 2 and
#' # adding it to the previous plot
#' survplot( msm_model, from = 2, km = TRUE, ci = 'none', add = TRUE,
#'           verbose = FALSE )
#'
#' # returning fitted and empirical data
#' all_data = survplot( msm_model, ci = 'none', return.all = TRUE,
#'                      verbose = FALSE, do.plot = FALSE )
#'
#' # saving them separately
#' km_data = all_data[[ 1 ]]
#' fitted_data = all_data[[ 2 ]]
#' }
#'
#' @references Titman, A. and Sharples, L.D. (2010). Model diagnostics for multi-state models,
#' \emph{Statistical Methods in Medical Research}, 19, 621-651.\cr
#'
#' Titman, A. and Sharples, L.D. (2008). A general goodness-of-fit test for Markov and
#' hidden Markov models, \emph{Statistics in Medicine}, 27, 2177-2195. \cr
#'
#' Jackson, C.H. (2011). Multi-State Models for Panel Data:\cr
#' The \emph{msm} Package for R. Journal of Statistical Software, 38(8), 1-29.\cr
#' URL \url{http://www.jstatsoft.org/v38/i08/}.
#' @seealso \code{\link[msm]{plot.survfit.msm}} \code{\link[msm]{msm}},
#' \code{\link[msm]{pmatrix.msm}}, \code{\link[data.table]{setDF}}
#' @author Francesco Grossetti \email{francesco.grossetti@@polimi.it}.
#' @import data.table
#' @importFrom msm absorbing.msm
#' @importFrom msm pmatrix.msm
#' @importFrom survival Surv
#' @importFrom survival survfit
#' @export
survplot = function( x, from = 1, to = NULL, range = NULL, covariates = "mean",
                     exacttimes = TRUE, times, grid = 100L,
                     km = FALSE, return.all = FALSE, return.km = NULL, return.p = NULL,
                     convert = FALSE, add = FALSE,
                     ci = c( "none", "normal", "bootstrap" ), interp = c( "start", "midpoint" ),
                     B = 100L, legend.pos = 'topright',
                     xlab = "Time", ylab = "Survival Probability",
                     lty.fit = 1, lwd.fit = 1, col.fit = "red", lty.ci.fit = 3, lwd.ci.fit = 1,
                     col.ci.fit = col.fit, mark.time = FALSE,
                     lty.km = 5, lwd.km = 1, col.km = "darkblue",
                     do.plot = TRUE, plot.width = 7, plot.height = 7,
                     devnew = TRUE, verbose = TRUE ) {

  time.start = proc.time()
  state         = NULL
  subject       = NULL
  mintime_exact = NULL
  .             = NULL
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
  oldw = getOption( "warn" )
  if ( verbose == FALSE ) {
    options( warn = -1 )
    if ( .Platform$OS.type == 'windows' ) {
      sink( file = "NUL" )
    } else {
      sink( file = "/dev/null" )
    }
    cat( '---\n' )
  }
  interp = match.arg( interp )
  ci = match.arg( ci )
  if ( exacttimes == TRUE ) {
    if ( missing( times ) ) {
      timediff = ( rg[ 2 ] - rg[ 1 ] ) / grid
      times = seq( 1, diff( rg ), timediff )
    } else {
      times = times
    }
  } else {
    if ( missing( times ) ) {
      timediff = ( rg[ 2 ] - rg[ 1 ] ) / grid
      times = seq( rg[ 1 ], rg[ 2 ], timediff )
    } else {
      times = times
    }
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
  if ( do.plot == TRUE ) {
    if ( add == FALSE ) {
      if ( devnew == TRUE ) {
        dev.new( noRStudioGD = TRUE, width = plot.width, height = plot.height )
      } else if ( devnew == FALSE ) {
        dev.set( dev.cur() )
      }
      plot( times, 1 - pr, type = "l", xlab = xlab, ylab = ylab, ylim = c( 0, 1 ),
            lwd = lwd.fit, lty = lty.fit, col = col.fit )
    } else {
      lines( times, 1 - pr, lwd = lwd.fit, lty = lty.fit, col = col.fit )
    }
    if ( ci != "none" ) {
      lines( times, 1 - lower, lwd = lwd.ci.fit, lty = lty.ci.fit, col = col.ci.fit )
      lines( times, 1 - upper, lwd = lwd.ci.fit, lty = lty.ci.fit, col = col.ci.fit )
    }
  }
  if ( return.all == TRUE || km == TRUE || ( !is.null( return.km ) && return.km == TRUE ) ) {
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
    if ( exacttimes == TRUE ) {
      wide[ , mintime_exact := mintime - min( mintime ) ]
      setcolorder( wide, c( 'subject', 'mintime', 'mintime_exact', 'anystate' ) )
      setkey( wide, subject )
    }
  }
  if ( do.plot == TRUE && km == TRUE ) {
    if ( add == FALSE ) {
      if ( exacttimes == FALSE ) {
        lines( survfit( Surv( wide$mintime, wide$anystate ) ~ 1 ), mark.time = mark.time,
               col = col.km, lty = lty.km, lwd = lwd.km )
      } else {
        lines( survfit( Surv( wide$mintime_exact, wide$anystate ) ~ 1 ), mark.time = mark.time,
               col = col.km, lty = lty.km, lwd = lwd.km )
      }
      if ( !is.null( legend.pos ) ) {
        legend( legend.pos, legend = c( "Fitted (solid)", 'Kaplan-Meier (dashed)' ), cex = 0.8 )
      }
    }
  }

  time.end = proc.time()
  time.total = time.end - time.start
  cat( '---\n' )
  cat( 'Function took:', time.total[ 3 ], '\n' )
  cat( '---\n' )
  if ( verbose == FALSE ) {
    sink()
  }
  options( warn = oldw )

  if ( return.all == TRUE ) {
    if ( !is.null( return.km ) && return.km == FALSE ) {
        stop( 'return.all is TRUE, but return.km is FALSE. Please, leave it set to NULL' )
    }
    if ( !is.null( return.p ) && return.p == FALSE ) {
      stop( 'return.all is TRUE, but return.p is FALSE. Please, leave it set to NULL' )
    }
    if ( !is.null( return.km ) && return.km == TRUE ) {
      warning( 'return.all is already set to TRUE. return.km will be ignored' )
    }
    if ( !is.null( return.p ) && return.p == TRUE ) {
      warning( 'return.all is already set to TRUE. return.p will be ignored' )
    }
    if ( convert == TRUE ) {
      setDF( wide )
      return( invisible( list( km = wide,
                               fitted = data.frame( time = times,
                                                    probs = round( 1 - pr, 4 ) ) ) ) )
    }
    return( invisible( list( km = wide,
                             fitted = data.table( time = times,
                                                  probs = round( 1 - pr, 4 ) ) ) ) )
  } else {
    if ( !is.null( return.km ) && return.km == TRUE && is.null( return.p ) ) {
      if ( convert == TRUE ) {
        setDF( wide )
        return( invisible( wide ) )
      }
      return( invisible( wide ) )
    } else if ( is.null( return.km ) && !is.null( return.p ) && return.p == TRUE ) {
      if ( convert == TRUE ) {
        return( invisible( data.frame( time = times, probs = round( 1 - pr, 4 ) ) ) )
      }
      return( invisible( data.table( time = times, probs = round( 1 - pr, 4 ) ) ) )
    }
  }
}




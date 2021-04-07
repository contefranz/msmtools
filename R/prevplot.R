if ( getRversion() >= "2.15.1" ) {
  utils::globalVariables( c( "obs", "hat" ) )
}
#' Plot observed and expected prevalences for a multi-state model
#'
#' Provides a graphical indication of goodness of fit of a multi-state model
#' computed by \code{\link[msm]{msm}} using observed and expected prevalences.
#' It also computes a rough indicator of where the data depart from the estimated
#' Markov model.
#'
#' @param x A \code{msm} object.
#' @param prev.obj A list computed by \code{\link[msm]{prevalence.msm}}.
#' It can be with or without confidence intervals. \code{prevplot} will behaves
#' accordingly.
#' @param exacttimes If \code{TRUE} (default) then transition times are known
#' and exact. This is inherited from \code{msm} and should be set the same way.
#' @param M If \code{TRUE}, then a rough indicator of deviance from the model is
#' computed (see 'Details'). Default is \code{FALSE}.
#' @param ci If \code{TRUE}, then confidence intervals, if they exist, are plotted.
#' Default is \code{FALSE}.
#' @details When \code{M = TRUE}, a rough indicator of the deviance from the
#' Markov model is computed according to Titman and Sharples (2008).
#' A comparison at a given time \eqn{t_i} of a patient \emph{k} in the state
#' \emph{s} between observed counts \eqn{O_{is}} with expected ones \eqn{E_{is}}
#' is build as follows:
#' \deqn{M_{is} = \frac{(O_{is} - E_{is})^2}{E_{is}}}{ (O_{is} - E_{is})^2 / E_{is}}
#'
#' The plot of the deviance M is returned together with the standard prevalence plot in the second
#' row. This is not editable by the user.
#'
#' @seealso \code{\link[msm]{plot.prevalence.msm}} \code{\link[msm]{msm}}
#' \code{\link[msm]{prevalence.msm}}
#' @references Titman, A. and Sharples, L.D. (2010). Model diagnostics for
#' multi-state models, \emph{Statistical Methods in Medical Research}, 19,
#' 621-651.\cr
#'
#' Titman, A. and Sharples, L.D. (2008). A general goodness-of-fit test for
#' Markov and hidden Markov models, \emph{Statistics in Medicine}, 27,
#' 2177-2195. \cr
#'
#' Gentleman RC, Lawless JF, Lindsey JC, Yan P. (1994). Multi-state Markov
#' models for analysing incomplete disease data with illustrations for HIV
#' disease. \emph{Statistics in Medicine}, 13:805-821. \cr
#'
#' Jackson, C.H. (2011). Multi-State Models for Panel Data:\cr
#' The \emph{msm} Package for R. Journal of Statistical Software, 38(8), 1-29.\cr
#' URL \url{https://www.jstatsoft.org/v38/i08/}.
#' @author Francesco Grossetti \email{francesco.grossetti@@unibocconi.it}.
#' @examples
#' \dontrun{
#' data( hosp )
#'
#' # augmenting the data
#' hosp_augmented = augment( data = hosp, data_key = subj, n_events = adm_number,
#'                           pattern = label_3, t_start = dateIN, t_end = dateOUT,
#'                           t_cens = dateCENS )
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
#' msm_model = msm( status_num ~ augmented_int, subject = subj,
#'                  data = hosp_augmented, covariates = ~ gender + age,
#'                  exacttimes = TRUE, gen.inits = TRUE, qmatrix = Qmat,
#'                  method = 'BFGS', control = list( fnscale = 6e+05, trace = 0,
#'                  REPORT = 1, maxit = 10000 ) )
#'
#' # defining the times at which compute the prevalences
#' t_min = min( hosp_augmented$augmented_int )
#' t_max = max( hosp_augmented$augmented_int )
#' steps = 100L
#'
#' # computing prevalences
#' prev = prevalence.msm( msm_model, covariates = 'mean', ci = 'normal',
#'                        times = seq( t_min, t_max, steps ) )
#'
#' # and plotting them using prevplot()
#' gof = prevplot( x = msm_model, prev.obj = prev, ci = TRUE, M = TRUE )
#' }
#'
#' @importFrom data.table as.data.table setnames setcolorder melt
#' @importFrom stats model.extract time
#' @importFrom ggplot2 ggplot aes geom_line facet_wrap scale_y_continuous xlab ylab theme_bw ggtitle theme
#' @importFrom scales percent
#' @importFrom patchwork wrap_plots
#' @export

prevplot = function( x, prev.obj, exacttimes = TRUE, M = FALSE, ci = FALSE ) {

  if ( !inherits( x, "msm" ) )
    stop( "x must be a msm model" )
  if ( !inherits( prev.obj, "list" ) )
    stop( "prev.obj must be a list computed by \"prevalence.msm\"" )
  if ( !is.logical(exacttimes) ) {
    stop( "exacttimes must be either TRUE or FALSE")
  }
  if ( !is.logical(M) ) {
    stop( "M must be either TRUE or FALSE")
  }
  if ( !is.logical(ci) ) {
    stop( "ci must be either TRUE or FALSE")
  }

  state_names = colnames( x$qmodel$imatrix )

  # extract the prevalences from prev.obj
  prev_obs = as.data.table(prev.obj$`Observed percentages`, keep.rownames = "time")
  setnames( prev_obs, c(2L:ncol(prev_obs)), state_names )
  prev_hat = as.data.table(prev.obj$`Expected percentages`$estimates, keep.rownames = "time")

  # keep.rownames is a char so I cast it back to integer
  prev_obs[ , time := as.integer( time ) ]
  prev_hat[ , time := as.integer( time ) ]
  # these are all wide, but ggplot works best when passing a long format data.frame
  # I reshape them and work my way with facet_wrap() instead of looping
  prev_obs_long = melt( prev_obs, id.vars = "time", variable.name = "state", value.name = "obs")
  prev_hat_long = melt( prev_hat, id.vars = "time", variable.name = "state", value.name = "hat")

  if ( ci ) {
    cat("Extracting confidence intervals\n")
    if ( length( prev.obj$`Expected percentages` ) > 1 ) {
      ci_lwr_hat = as.data.table(prev.obj$`Expected percentages`$ci[ , , 1L ])
      ci_upr_hat = as.data.table(prev.obj$`Expected percentages`$ci[ , , 2L ])
      setnames(ci_lwr_hat, names(ci_lwr_hat), state_names)
      setnames(ci_upr_hat, names(ci_upr_hat), state_names)
      # add "time" variable
      ci_lwr_hat[ , time := prev_hat[ , time ] ]
      ci_upr_hat[ , time := prev_hat[ , time ] ]
      # re-order columns cause we are cool
      setcolorder( ci_lwr_hat, c(ncol(ci_lwr_hat), 1L:(ncol(ci_lwr_hat)-1L)) )
      setcolorder( ci_upr_hat, c(ncol(ci_upr_hat), 1L:(ncol(ci_upr_hat)-1L)) )
      # melt the guys!
      ci_lwr_hat_long = melt( ci_lwr_hat, id.vars = "time", variable.name = "state", value.name = "lwr")
      ci_upr_hat_long = melt( ci_upr_hat, id.vars = "time", variable.name = "state", value.name = "upr")
    } else {
      stop("There are no CIs in \"prev.obj\"")
    }
  }

  if ( ci ) {
    # bind all data together
    to_plot = cbind( prev_obs_long, prev_hat_long[ , .( hat ) ],
                     ci_lwr_hat_long[ , .(lwr) ], ci_upr_hat_long[ , .(upr) ] )
    # instead of going crazy after scales::percent, I just rescale the vectors here
    to_plot[ , `:=` ( obs = obs / 100L, hat = hat / 100L, lwr = lwr / 100L, upr = upr / 100L ) ]
  } else {
    # bind all data together
    to_plot = cbind( prev_obs_long, prev_hat_long[ , .( hat ) ] )
    # instead of going crazy after scales::percent, I just rescale the vectors here
    to_plot[ , `:=` ( obs = obs / 100L, hat = hat / 100L ) ]
  }
  # this works for exact times of transitions
  if ( exacttimes ) {
    to_plot[ , time := time - min(time) ]
  }

  if ( M ) {
    cat("Computing Deviance M\n")
    prev_obs_abs = as.data.table(prev.obj$Observed)
    prev_hat_abs = prev.obj$Expected
    if ( length( prev_hat_abs ) > 1L ) {
      M_gof = ( prev_obs_abs - prev_hat_abs$estimates )^2L / prev_hat_abs$estimates
    } else {
      M_gof = ( prev_obs_abs - prev_hat_abs )^2L / prev_hat_abs
    }
    setnames( M_gof, 1L:(ncol(M_gof)-1L), state_names)
    M_gof[ , `:=` (time = prev_hat[ , time ], Total = NULL) ]
    M_gof_long = melt( M_gof, id.vars = "time",  variable.name = "state", value.name = "M")
    to_plot = cbind( to_plot, M_gof_long[ , .(M)])
    to_plot[ , M := M / 100L ]
  }

  # build the plot
  p_canvas = ggplot( to_plot ) +
    facet_wrap(. ~ state) +
    scale_y_continuous(labels = percent) +
    xlab("Time") + ylab("Prevalence") +
    theme_bw() +
    ggtitle("Prevalence Plot") +
    theme(legend.position = "bottom")

  p = p_canvas +
    geom_line(aes( x = time, y = obs, group = 1, color = "Observed" ) ) +
    geom_line(aes( x = time, y = hat, group = 1, color = "Estimated" ) ) +
    scale_color_manual( name = "", values = c( "Estimated" = "red", "Observed" = "darkblue") )

  if ( ci ) {
    p = p +
      geom_line( aes( x = time, y = lwr, group = 1, color = "Estimated" ), linetype = 3 ) +
      geom_line( aes( x = time, y = upr, group = 1, color = "Estimated" ), linetype = 3 )
  }

  if ( M ) {
    p_gof = p_canvas +
      geom_line( aes( x = time, y = M, group = 1 ) ) +
      ylab("Deviance M") +
      theme_bw() +
      ggtitle("Deviance of Markov Model")
    p_combined = wrap_plots( p, p_gof, nrow = 2L )
    print( p_combined )
    return( p_combined )
  } else {
    print( p )
    return( p )
  }
}



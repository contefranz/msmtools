if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("state", "status", "subject", "time",
                             "time_exact", "anystate", "km", ".",
                             ":=", "rowid", "surv", "lwr", "upr"))
}
#' Plot fitted survival and Kaplan-Meier curves from a multi-state model
#'
#' Plot fitted survival probabilities from an [msm::msm()] model and compare
#' them with Kaplan-Meier estimates. The function can also return the data used
#' to build each curve.
#'
#' @param x A fitted **msm** model object.
#' @param from State from which to compute the estimated survival.
#' Defaults to state 1.
#' @param to The absorbing state to which compute the estimated survival.
#' Defaults to the highest state found by [msm::absorbing.msm()].
#' @param range A numeric vector of two elements giving the time range of the
#' plot.
#' @param covariates Covariate values for which to evaluate the expected
#' probabilities. These can be `"mean"`, denoting the means of the covariates in
#' the data (default); the number 0, indicating that all covariates should be
#' set to zero; or a list of values, with optional names. For example:
#'
#' `list(75, 1)`
#'
#' The unnamed list must follow the order of the covariates in the original
#' model formula. A named list is also accepted:
#'
#' `list(age = 75, gender = "M")`.
#' @param exacttimes If `TRUE` (default), transition times are known and exact.
#' This should match the value used when fitting the model with **msm**.
#' @param times An optional numeric vector giving the times at which to compute
#' the fitted survival.
#' @param grid An integer specifying the grid points at which to compute the
#' fitted survival curve (see Details). If `times` is passed, `grid` is ignored.
#' Defaults to 100 points.
#' @param km If `TRUE`, the Kaplan-Meier curve is plotted. Default is `FALSE`.
#' @param ci A character vector with the type of confidence intervals to compute for the fitted
#' survival curve. Specify either `"none"` (default), for no confidence intervals,
#' `"normal"` or `"bootstrap"`, for confidence intervals computed with the respective
#' method in [msm::pmatrix.msm()]. This is computationally intensive,
#' since intervals must be computed at a series of times.
#' @param interp If `"start"` (default), then the entry time into the
#' absorbing state is assumed to be the time it is first observed in the data.
#' If `"midpoint"`, then the entry time into the absorbing state is assumed
#' to be halfway between the time it is first observed and the previous
#' observation time. This is generally more reasonable for "progressive"
#' models with observations at arbitrary times.
#' @param B Number of bootstrap or normal replicates for the confidence interval.
#' The default is 100 rather than the usual 1000, since these plots are for
#' rough diagnostic purposes.
#' @param ci_km A character vector with the type of confidence intervals to compute for the
#' Kaplan-Meier curve. Specify either `"none"`, `"plain"`, `"log"`, `"log-log"`,
#' `"logit"`, or `"arcsin"`, as coded in [survival::survfit()].
#' @param print_plot If `TRUE` (default), the plot is printed before being
#' returned. If `FALSE`, the plot is returned without printing.
#' @param verbosity Controls informational output. Use `"quiet"` to suppress
#' status messages and `"summary"` or `"progress"` for high-level messages.
#' @param ... Reserved for the migration trampoline. Passing the legacy
#' `out` argument here raises an informative error pointing to the new
#' `$fitted` / `$km` access pattern. The trampoline will be removed in
#' v2.3.0.
#' @details The function wraps [msm::plot.survfit.msm()] and adds support for
#' exact-time plots by resetting the time scale to follow-up time. It returns
#' a `gg/ggplot` object so the plot composes directly with [ggplot2::ggsave()],
#' [ggplot2::theme()], and other ggplot operations.
#'
#' You can pass custom evaluation times through `times`, or let `survplot()`
#' define them from `grid`. Larger `grid` values produce a finer grid and
#' increase computation time.
#' @returns A `gg/ggplot` object. The fitted and (when `km = TRUE`)
#' Kaplan-Meier data tables are attached to the returned plot as named
#' fields:
#'
#' * `$fitted` — a `data.table` with columns `time`, `surv`, and (when
#'   `ci` is not `"none"`) `lwr` / `upr`. Always present.
#' * `$km` — a `data.table` with the Kaplan-Meier curve, exposed only when
#'   `km = TRUE`.
#'
#' Access the data through the standard `$` operator:
#'
#' ```
#' p <- survplot(model, km = TRUE)
#' p           # prints the plot
#' p$fitted    # fitted survival data
#' p$km        # Kaplan-Meier data
#' ```
#'
#' `print_plot` only controls whether the plot is printed as a side effect.
#' Returned objects are unchanged: use `print_plot = FALSE` to create the plot
#' or returned data silently.
#'
#' @examplesIf interactive()
#' data(hosp)
#'
#' # augmenting the data
#' hosp_augmented = augment(data = hosp, data_key = subj, n_events = adm_number,
#'                           pattern = label_3, t_start = dateIN, t_end = dateOUT,
#'                           t_cens = dateCENS)
#'
#' # let's define the initial transition matrix for our model
#' Qmat = matrix(data = 0, nrow = 3, ncol = 3, byrow = TRUE)
#' Qmat[1, 1:3] = 1
#' Qmat[2, 1:3] = 1
#' colnames(Qmat) = c('IN', 'OUT', 'DEAD')
#' rownames(Qmat) = c('IN', 'OUT', 'DEAD')
#'
#' # fitting the model using
#' # gender and age as covariates
#' library(msm)
#' msm_model = msm(status_num ~ augmented_int, subject = subj,
#'                  data = hosp_augmented, covariates = ~ gender + age,
#'                  exacttimes = TRUE, gen.inits = TRUE, qmatrix = Qmat,
#'                  method = 'BFGS', control = list(fnscale = 6e+05, trace = 0,
#'                  REPORT = 1, maxit = 10000))
#'
#' # plotting the fitted and empirical survival from state = 1
#' theplot = survplot(x = msm_model, km = TRUE)
#'
#' # the fitted and Kaplan-Meier data tables are attached to the plot
#' head(theplot$fitted)
#' head(theplot$km)
#'
#' @references Titman, A. and Sharples, L.D. (2010). Model diagnostics for
#' multi-state models, *Statistical Methods in Medical Research*, 19, 621-651.
#'
#' Titman, A. and Sharples, L.D. (2008). A general goodness-of-fit test for
#' Markov and hidden Markov models, *Statistics in Medicine*, 27, 2177-2195.
#'
#' Jackson, C.H. (2011). Multi-State Models for Panel Data: The **msm** Package
#' for R. Journal of Statistical Software, 38(8), 1-29.
#' <https://www.jstatsoft.org/v38/i08/>.
#' @seealso [msm::plot.survfit.msm()], [msm::msm()],
#' [msm::pmatrix.msm()], [data.table::setDF()]
#' @author Francesco Grossetti <francesco.grossetti@unibocconi.it>.
#' @export

survplot = function(x, from = 1, to = NULL, range = NULL, covariates = "mean",
                     exacttimes = TRUE, times, grid = 100L, km = FALSE,
                     ci = c("none", "normal", "bootstrap"), interp = c("start", "midpoint"),
                     B = 100L,
                     ci_km = c("none", "plain", "log", "log-log", "logit", "arcsin"),
                     print_plot = TRUE,
                     verbosity = getOption("msmtools.verbosity", "quiet"),
                     ...) {

  dots = list(...)
  if ("out" %in% names(dots)) {
    stop("`out` was removed in msmtools 2.2.0. ",
         "survplot() now always returns a gg/ggplot with the fitted and ",
         "Kaplan-Meier data tables attached as named fields. Use ",
         "p$fitted and (when km = TRUE) p$km instead. ",
         "This trampoline will be removed in v2.3.0.",
         call. = FALSE)
  }

  verbosity = .msmtools_verbosity(verbosity)
  .msmtools_validate_flag(exacttimes, "exacttimes")
  .msmtools_validate_flag(km, "km")
  .msmtools_validate_flag(print_plot, "print_plot")
  .msmtools_validate_positive_scalar(from, "from")
  .msmtools_validate_positive_scalar(grid, "grid")
  .msmtools_validate_positive_scalar(B, "B")
  if (!missing(times)) {
    .msmtools_validate_plot_times(times)
  }

  if (!inherits(x, "msm"))
    stop("x must be a msm model")
  if (is.null(to)) {
    to = max(msm::absorbing.msm(x))
  } else {
    .msmtools_validate_positive_scalar(to, "to")
    if (!(to %in% msm::absorbing.msm(x)))
      stop("to must be an absorbing state")
  }
  if (is.null(range))
    rg = range(stats::model.extract(x$data$mf, "time"))
  else {
    .msmtools_validate_plot_range(range)
    rg = range
  }

  # matching arguments
  interp = match.arg(interp)
  ci = match.arg(ci)
  ci_km = match.arg(ci_km)
  states = rownames(x$qmodel$imatrix)

  if (exacttimes) {
    if (missing(times)) {
      timediff = (rg[2L] - rg[1L]) / grid
      times = seq(1L, diff(rg), timediff)
    } else {
      times = times
    }
  } else {
    if (missing(times)) {
      timediff = (rg[2L] - rg[1L]) / grid
      times = seq(rg[1L], rg[2L], timediff)
    } else {
      times = times
    }
  }

  # For each given t in times, extract the transition probabilities
  if (ci == "none") {
    .msmtools_cli_info(verbosity, "extracting transition probabilities")
  } else {
    .msmtools_cli_info(verbosity,
      "extracting transition probabilities and computing confidence intervals")
  }

  surv_probabilities = data.table::data.table(rowid = seq_along(times))
  for (t in seq_along(times)) {
    # Extract the transition prob matrix and compute CI if ci != "none"
    # I use the parlance set() for fast and efficient assignment
    P = msm::pmatrix.msm(x, times[t], t1 = times[1L],
                          covariates = covariates, ci = ci, B = B)
    if (ci != "none") {
      data.table::set(x = surv_probabilities, i = t,
                       j = c("time", "surv", "lwr", "upr"),
                       value = list(times[t],
                                     1L - P$estimates[from, to],
                                     P$L[from, to], P$U[from, to]))
    } else {
      data.table::set(x = surv_probabilities, i = t,
                       j = c("time", "surv"),
                       value = list(times[t], 1L - P[from, to]))
    }
  }
  surv_probabilities[, rowid := NULL]

  if (km) {
    # extract the necessary data to be used with survfit()
    dat = data.table::as.data.table(x$data$mf[, c("(subject)", "(time)", "(state)")])
    data.table::setnames(dat, c('subject', 'time', 'state'))
    absind = which(dat$state == to)
    if (any(dat[state == to])) {
      if (interp == 'start') {
        mintime = dat[absind, min(time), by = subject]
      } else if (interp == 'midpoint') {
        mintime = 0.5 * (dat[absind, .(time), by = subject] +
                            dat[absind - 1, .(time), by = subject])
      } else {
        mintime = dat[, max(time), by = subject]
      }
      wide = data.table::data.table(time = mintime,
        anystate = as.numeric(any(dat[state == to, .(state)])))
      data.table::setnames(wide, c('subject', 'time', 'anystate'))
    }
    # this computes the KM curve
    if (exacttimes) {
      wide[, time_exact := time - min(time)]
      p_km = survival::survfit(survival::Surv(wide$time_exact, wide$anystate) ~ 1,
        conf.type = ci_km)
    } else {
      p_km = survival::survfit(survival::Surv(wide$time, wide$anystate) ~ 1,
        conf.type = ci_km)
    }
    data.table::setorder(wide, time)
    if (ci_km != "none") {
      out_km = data.table::data.table(wide, km = p_km$surv, lwr = p_km$lower, upr = p_km$upper)
    } else {
      out_km = data.table::data.table(wide, km = p_km$surv)
    }
    data.table::setcolorder(out_km, c(1L, 2L, 4L, 3L, 5L))
  }
  # build the plot
  # ggplot integration
  if (ci != "none") {
    p = ggplot2::ggplot(data = surv_probabilities,
      ggplot2::aes(x = time, y = surv, color = "Fitted")) +
      ggplot2::scale_y_continuous(limits = c(0, 1),
                                   breaks = seq(0, 1, by = .25)) +
      ggplot2::xlab("Time") + ggplot2::ylab("Survival Probability")
    p = p + ggplot2::geom_line()
    p = p +
      ggplot2::geom_line(ggplot2::aes(x = time, y = 1 - lwr,
                                        color = "Fitted"), linetype = 4) +
      ggplot2::geom_line(ggplot2::aes(x = time, y = 1 - upr,
                                        color = "Fitted"), linetype = 4)
  } else {
    p = ggplot2::ggplot(data = surv_probabilities,
      ggplot2::aes(x = time, y = surv, color = "Fitted")) +
      ggplot2::scale_y_continuous(limits = c(0, 1),
                                   breaks = seq(0, 1, by = .25)) +
      ggplot2::xlab("Time") + ggplot2::ylab("Survival Probability")
    p = p + ggplot2::geom_line()
  }
  if (km) {
    if (exacttimes) {
      p = p +
        ggplot2::geom_line(data = out_km,
                             ggplot2::aes(x = time_exact, y = km,
                                            color = "KM"), linetype = 5) +
        ggplot2::xlab("Exact Time")
      if (ci_km != "none") {
        p = p +
          ggplot2::geom_line(data = out_km,
                               ggplot2::aes(x = time_exact, y = lwr,
                                              color = "KM"), linetype = 3) +
          ggplot2::geom_line(data = out_km,
                               ggplot2::aes(x = time_exact, y = upr,
                                              color = "KM"), linetype = 3)
      }
    } else {
      p = p +
        ggplot2::geom_line(data = out_km,
                             ggplot2::aes(x = time, y = km, color = "KM"),
                             linetype = 5) +
        ggplot2::xlab("Absolute Time")
      if (ci_km != "none") {
        p = p +
          ggplot2::geom_line(data = out_km,
                               ggplot2::aes(x = time, y = lwr,
                                              color = "KM"), linetype = 3) +
          ggplot2::geom_line(data = out_km,
                               ggplot2::aes(x = time, y = upr,
                                              color = "KM"), linetype = 3)
      }
    }
  }
  # render the plot
  p = p +
    ggplot2::scale_color_manual(name = "",
                                 values = c("Fitted" = "red",
                                             "KM" = "darkblue")) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::ggtitle(paste0("Estimation for transition ", states[from], " - ", states[to]))
  p$fitted = surv_probabilities[]
  if (km) {
    p$km = out_km[]
  }
  if (print_plot) {
    print(p)
  }
  return(p)
}

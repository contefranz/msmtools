if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(":=", ".", "time", "state", "obs", "hat", "lwr", "upr", "M",
       "Total"))
}
#' Plot observed and expected prevalences for a multi-state model
#'
#' Plot observed and expected state prevalences from a fitted multi-state model.
#' The function can also compute a rough diagnostic for where the data depart
#' from the estimated Markov model.
#'
#' @param x A fitted **msm** model object.
#' @param prev.obj A list computed by [msm::prevalence.msm()].
#' It may include confidence intervals; `prevplot()` adapts automatically.
#' @param exacttimes If `TRUE` (default), transition times are known and exact.
#' This should match the value used when fitting the model with **msm**.
#' @param M If `TRUE`, a rough indicator of deviance from the model is
#' computed (see Details). Default is `FALSE`.
#' @param ci If `TRUE`, confidence intervals are plotted when available.
#' Default is `FALSE`.
#' @param print_plot If `TRUE` (default), the plot is printed before being
#' returned. If `FALSE`, the plot is returned without printing.
#' @param verbosity Controls informational output. Use `"quiet"` to suppress
#' status messages and `"summary"` or `"progress"` for high-level messages.
#' @details When `M = TRUE`, a rough indicator of the deviance from the
#' Markov model is computed according to Titman and Sharples (2008).
#' A comparison at a given time `t_i` of a subject `k` in the state `s` between
#' observed counts `O_is` and expected counts `E_is` is built as
#' `M_is = (O_is - E_is)^2 / E_is`.
#'
#' The deviance `M` plot is returned together with the standard prevalence plot
#' in the second row. This layout is fixed.
#'
#' When `M = TRUE`, the combined layout is built with **patchwork**, which is
#' an optional dependency of **msmtools**. Install it with
#' `install.packages("patchwork")` if it is not already available; `prevplot()`
#' raises an informative error otherwise. The default `M = FALSE` path has no
#' such requirement.
#' @returns When `M = FALSE`, a `gg/ggplot` object with observed and expected
#' prevalences is returned. When `M = TRUE`, a `patchwork` object is returned
#' with the prevalence plot and the deviance `M` plot.
#'
#' The returned object also carries a `$prevalence` field with the
#' long-format `data.table` used to build the plot. It always includes
#' `time`, `state`, `obs`, and `hat`; it also includes `lwr` and `upr`
#' when `ci = TRUE`, and `M` when `M = TRUE`. Access it directly:
#'
#' ```
#' p <- prevplot(model, prev_obj)
#' p$prevalence
#' ```
#'
#' `print_plot` only controls whether the plot is printed as a side effect.
#' Returned objects are unchanged: use `print_plot = FALSE` to create the plot
#' silently.
#'
#' @seealso [msm::plot.prevalence.msm()], [msm::msm()],
#' [msm::prevalence.msm()]
#' @references Titman, A. and Sharples, L.D. (2010). Model diagnostics for
#' multi-state models, *Statistical Methods in Medical Research*, 19, 621-651.
#'
#' Titman, A. and Sharples, L.D. (2008). A general goodness-of-fit test for
#' Markov and hidden Markov models, *Statistics in Medicine*, 27, 2177-2195.
#'
#' Gentleman RC, Lawless JF, Lindsey JC, Yan P. (1994). Multi-state Markov
#' models for analysing incomplete disease data with illustrations for HIV
#' disease. *Statistics in Medicine*, 13:805-821.
#'
#' Jackson, C.H. (2011). Multi-State Models for Panel Data: The **msm** Package
#' for R. Journal of Statistical Software, 38(8), 1-29.
#' <https://www.jstatsoft.org/v38/i08/>.
#' @author Francesco Grossetti <francesco.grossetti@unibocconi.it>.
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
#' # defining the times at which compute the prevalences
#' t_min = min(hosp_augmented$augmented_int)
#' t_max = max(hosp_augmented$augmented_int)
#' steps = 100L
#'
#' # computing prevalences
#' prev = prevalence.msm(msm_model, covariates = 'mean', ci = 'normal',
#'                        times = seq(t_min, t_max, steps))
#'
#' # and plotting them using prevplot()
#' gof = prevplot(x = msm_model, prev.obj = prev, ci = TRUE, M = TRUE)
#'
#' @export

prevplot = function(x, prev.obj, exacttimes = TRUE, M = FALSE, ci = FALSE,
                     print_plot = TRUE,
                     verbosity = getOption("msmtools.verbosity", "quiet")) {

  verbosity = .msmtools_verbosity(verbosity)
  .msmtools_validate_flag(exacttimes, "exacttimes")
  .msmtools_validate_flag(M, "M")
  .msmtools_validate_flag(ci, "ci")
  .msmtools_validate_flag(print_plot, "print_plot")

  if (!inherits(x, "msm"))
    stop("x must be a msm model")
  if (!inherits(prev.obj, "list"))
    stop("prev.obj must be a list computed by \"prevalence.msm\"")

  state_names = colnames(x$qmodel$imatrix)

  # extract the prevalences from prev.obj
  prev_obs = data.table::as.data.table(prev.obj$`Observed percentages`,
    keep.rownames = "time")
  data.table::setnames(prev_obs, c(2L:ncol(prev_obs)), state_names)
  prev_hat = data.table::as.data.table(prev.obj$`Expected percentages`$estimates,
    keep.rownames = "time")

  # keep.rownames is a char so I cast it back to integer
  prev_obs[, time := as.integer(time)]
  prev_hat[, time := as.integer(time)]
  # these are all wide, but ggplot works best when passing a long format data.frame
  # I reshape them and work my way with facet_wrap() instead of looping
  prev_obs_long = data.table::melt(prev_obs, id.vars = "time",
                                    variable.name = "state",
                                    value.name = "obs")
  prev_hat_long = data.table::melt(prev_hat, id.vars = "time",
                                    variable.name = "state",
                                    value.name = "hat")

  if (ci) {
    .msmtools_cli_info(verbosity, "extracting confidence intervals")
    if (length(prev.obj$`Expected percentages`) > 1) {
      ci_lwr_hat = data.table::as.data.table(prev.obj$`Expected percentages`$ci[, , 1L])
      ci_upr_hat = data.table::as.data.table(prev.obj$`Expected percentages`$ci[, , 2L])
      data.table::setnames(ci_lwr_hat, names(ci_lwr_hat), state_names)
      data.table::setnames(ci_upr_hat, names(ci_upr_hat), state_names)
      # add "time" variable
      ci_lwr_hat[, time := prev_hat[, time]]
      ci_upr_hat[, time := prev_hat[, time]]
      # re-order columns cause we are cool
      data.table::setcolorder(ci_lwr_hat, c(ncol(ci_lwr_hat), 1L:(ncol(ci_lwr_hat)-1L)))
      data.table::setcolorder(ci_upr_hat, c(ncol(ci_upr_hat), 1L:(ncol(ci_upr_hat)-1L)))
      # melt the guys!
      ci_lwr_hat_long = data.table::melt(ci_lwr_hat, id.vars = "time", variable.name = "state",
        value.name = "lwr")
      ci_upr_hat_long = data.table::melt(ci_upr_hat, id.vars = "time", variable.name = "state",
        value.name = "upr")
    } else {
      stop("There are no CIs in \"prev.obj\"")
    }
  }

  if (ci) {
    # bind all data together
    to_plot = cbind(prev_obs_long, prev_hat_long[, .(hat)],
                     ci_lwr_hat_long[, .(lwr)], ci_upr_hat_long[, .(upr)])
    # rescale to [0,1] so the y-axis labeller can format as percent
    to_plot[, `:=`(obs = obs / 100L, hat = hat / 100L, lwr = lwr / 100L, upr = upr / 100L)]
  } else {
    # bind all data together
    to_plot = cbind(prev_obs_long, prev_hat_long[, .(hat)])
    # rescale to [0,1] so the y-axis labeller can format as percent
    to_plot[, `:=`(obs = obs / 100L, hat = hat / 100L)]
  }
  # this works for exact times of transitions
  if (exacttimes) {
    to_plot[, time := time - min(time)]
  }

  if (M) {
    .msmtools_cli_info(verbosity, "computing deviance M")
    prev_obs_abs = data.table::as.data.table(prev.obj$Observed)
    prev_hat_abs = prev.obj$Expected
    if (length(prev_hat_abs) > 1L) {
      M_gof = (prev_obs_abs - prev_hat_abs$estimates)^2L / prev_hat_abs$estimates
    } else {
      M_gof = (prev_obs_abs - prev_hat_abs)^2L / prev_hat_abs
    }
    data.table::setnames(M_gof, 1L:(ncol(M_gof)-1L), state_names)
    M_gof[, `:=`(time = prev_hat[, time], Total = NULL)]
    M_gof_long = data.table::melt(M_gof, id.vars = "time",
                                   variable.name = "state", value.name = "M")
    to_plot = cbind(to_plot, M_gof_long[, .(M)])
    to_plot[, M := M / 100L]
  }

  # build the plot
  p_canvas = ggplot2::ggplot(to_plot) +
    ggplot2::facet_wrap(. ~ state) +
    ggplot2::scale_y_continuous(
      labels = function(x) paste0(formatC(x * 100, format = "f", digits = 1), "%")
    ) +
    ggplot2::xlab("Time") + ggplot2::ylab("Prevalence") +
    ggplot2::theme_bw() +
    ggplot2::ggtitle("Prevalence Plot") +
    ggplot2::theme(legend.position = "bottom")

  p = p_canvas +
    ggplot2::geom_line(ggplot2::aes(x = time, y = obs, group = 1, color = "Observed")) +
    ggplot2::geom_line(ggplot2::aes(x = time, y = hat, group = 1, color = "Estimated")) +
    ggplot2::scale_color_manual(name = "", values = c("Estimated" = "red", "Observed" = "darkblue"))

  if (ci) {
    p = p +
      ggplot2::geom_line(ggplot2::aes(x = time, y = lwr, group = 1, color = "Estimated"),
        linetype = 3) +
      ggplot2::geom_line(ggplot2::aes(x = time, y = upr, group = 1, color = "Estimated"),
        linetype = 3)
  }

  if (M) {
    p_gof = p_canvas +
      ggplot2::geom_line(ggplot2::aes(x = time, y = M, group = 1)) +
      ggplot2::ylab("Deviance M") +
      ggplot2::theme_bw() +
      ggplot2::ggtitle("Deviance of Markov Model")
    if (!requireNamespace("patchwork", quietly = TRUE)) {
      stop("`M = TRUE` requires the 'patchwork' package. ",
           "Install it with install.packages(\"patchwork\").",
           call. = FALSE)
    }
    p_combined = patchwork::wrap_plots(p, p_gof, nrow = 2L)
    p_combined$prevalence = to_plot[]
    if (print_plot) {
      print(p_combined)
    }
    return(p_combined)
  } else {
    p$prevalence = to_plot[]
    if (print_plot) {
      print(p)
    }
    return(p)
  }
}

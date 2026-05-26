#' Validate positive scalar numeric plotting arguments
#'
#' Used by plotting functions for arguments that control state selection,
#' grids, and bootstrap sizes.
#'
#' @keywords internal
#' @noRd
.msmtools_validate_positive_scalar = function(x, name) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) ||
      !is.finite(x) || x <= 0) {
    stop(name, " must be a positive scalar numeric")
  }
}

#' Validate finite two-value plotting ranges
#'
#' Checks that a user-supplied time range is finite and has exactly two numeric
#' endpoints.
#'
#' @keywords internal
#' @noRd
.msmtools_validate_plot_range = function(x) {
  if (!is.numeric(x) || length(x) != 2L || anyNA(x) ||
      any(!is.finite(x))) {
    stop("range must be a finite numeric vector of two elements")
  }
}

#' Validate user-supplied plotting times
#'
#' Checks that custom evaluation times are finite, numeric, and non-empty.
#'
#' @keywords internal
#' @noRd
.msmtools_validate_plot_times = function(x) {
  if (!is.numeric(x) || length(x) == 0L || anyNA(x) ||
      any(!is.finite(x))) {
    stop("times must be a finite non-empty numeric vector")
  }
}

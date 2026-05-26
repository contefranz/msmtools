if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("status", "status_num", "n_status",
                             "status_exp", "status_exp_num", "n_status_exp",
                             ":=", ".", ".I", ".N", ".SD", "N", "V2",
                             "t_end", "t_cens", "t_death"))
}
#' Build augmented transition data
#'
#' Reshape standard longitudinal data into augmented transition data suitable
#' for multi-state models fitted with **msm**.
#'
#' @param data A `data.table` or `data.frame` object in longitudinal
#' format where each row represents an observation with known start and end
#' times. If `data` is a `data.frame`, `augment()` internally casts it to a
#' `data.table`.
#' @param data_key A keying variable used to identify subjects and define a key
#' for `data` (see [data.table::setkey()]).
#' @param n_events An integer variable indicating the progressive (monotonic)
#' event number for each subject. `augment()` checks whether `n_events` is
#' monotonically increasing within each `data_key` and stops if the check fails
#' (see Details). If missing, `augment()` creates a variable named `"n_events"`.
#' @param pattern Either an integer, a factor, or a character variable with 2 or
#' 3 unique values that gives each subject's terminal outcome schema. When 2
#' values are detected, they must be in the format: 0 = "alive", 1 = "dead".
#' When 3 values are detected, they must be: 0 = "alive",
#' 1 = "dead during a transition", 2 = "dead after a transition has ended"
#' (see Details).
#' @param state A character vector of exactly three unique, non-missing,
#' non-empty labels used as the generated transition-state vocabulary.
#' Defaults to `c("IN", "OUT", "DEAD")` (see Details).
#' @param t_start The starting time of an observation. It can be passed as date,
#' integer, or numeric format.
#' @param t_end The ending time of an observation. It can be passed as date,
#' integer, or numeric format.
#' @param t_cens The censoring time of the study. This is the date until each
#' ID is observed, if still active in the cohort.
#' @param t_death The exact death time of a subject ID. If `t_death` is
#' missing, `t_cens` is assumed to contain both censoring and death times
#' and a warning is raised.
#' @param t_augmented A variable indicating the name of the new time variable
#' in the augmented format. If `t_augmented` is missing, the default name
#' `"augmented"` is used and the new variable is added to `data`. When
#' `t_start` is a date or difftime, `augment()` also creates an integer or
#' numeric companion variable. The suffix `"_int"` or `"_num"` is added to
#' `t_augmented` accordingly. This is needed because **msm** does not handle
#' date or difftime variables directly. Both variables are positioned before
#' `t_start`.
#' @param more_status A variable that marks further transitions beyond the
#' default ones given by `state`. `more_status` can be a factor or character
#' (see Details). If `NULL` (default), `augment()` ignores it.
#' @param check_NA If `TRUE`, `data_key`, `n_events`, `pattern`, `t_start`, and
#' `t_end` are checked for missing values. If any missing values are found, the
#' function stops with an error. Default is `FALSE` because `augment()` is not
#' intended for general consistency checks and the scan can add memory overhead
#' on very large datasets. `more_status` is always checked for missing values
#' when supplied.
#' @param copy If `FALSE` (default), `augment()` keeps the historical
#' memory-efficient behavior and may modify caller-owned `data` by reference.
#' If `TRUE`, `data` is copied before any `data.table` operation so the input
#' object remains unchanged.
#' @param verbosity Controls informational output. Use `"quiet"` to suppress
#' status messages, `"summary"` for high-level phase messages and timing, and
#' `"progress"` for phase messages plus progress bars in long status-building
#' loops. The default is `getOption("msmtools.verbosity", "quiet")`.
#' @details `augment()` requires a monotonic event sequence within each subject.
#' The data are ordered with [data.table::setkey()] using `data_key` as the
#' primary key and `t_start` as the secondary key. The function then checks the
#' monotonicity of `n_events`; if the check fails, it stops and reports the
#' subjects that violate the condition. If `n_events` is missing, `augment()`
#' first computes a progression number named *n_events* and then runs the same
#' check.
#'
#' Argument `pattern` describes the terminal outcome schema and must follow the
#' expected ordering. With two statuses, values must correspond to
#' `0 = "alive"` and `1 = "dead"`. With three statuses, integer values must
#' correspond to `0 = "alive"`, `1 = "dead inside a transition"`, and
#' `2 = "dead outside a transition"`. Character and factor values must follow
#' the same order. For example, `0` cannot be used to indicate death.
#'
#' Argument `state` describes the generated transition-state vocabulary. Its
#' order also matters. The first element is the state at `t_start` (for example,
#' `"IN"`), the second element is the state at `t_end` (for example, `"OUT"`),
#' and the third element is the absorbing state (for example, `"DEAD"`). A
#' two-value `pattern` still requires three `state` labels because `augment()`
#' infers whether death maps to the absorbing state inside or outside the
#' transition window.
#'
#' `more_status` lets `augment()` represent transitions beyond the defaults in
#' `state`. Standard observations that add no extra information should use
#' `"df"` for "default" (see Examples, or run `?hosp` and inspect `rehab_it`).
#' More complex transitions should use concise, self-explanatory labels.
#'
#' By default, `augment()` follows **data.table** by-reference semantics to avoid
#' unnecessary copies of large longitudinal datasets. This means the input may
#' have its key changed, and `n_events` may be added when the argument is
#' omitted. Set `copy = TRUE` when the original input object must remain
#' unchanged.
#'
#' The function always returns a `data.table`. Use [as.data.frame()] on the
#' result if a plain `data.frame` is needed by downstream code.
#'
#' @return An augmented dataset of class `data.table`. Each row represents a
#' specific transition for a given subject. `augment()` computes the following
#' key variables:
#'
#' * `augmented`: The transition time variable. If `t_augmented` is missing,
#'   `augment()` creates *augmented* by default. The variable is built from
#'   `t_start` and `t_end` and inherits their class. If `t_start` is a date,
#'   `augment()` also creates an integer variable named *augmented_int*. If
#'   `t_start` is a difftime, it creates a numeric variable named
#'   *augmented_num*.
#' * `status`: A status flag that contains the states as specified in `state`.
#'   `augment()` automatically checks whether argument `pattern` has 2 or 3
#'   unique values and computes the correct structure of a given subject as
#'   reported in the vignette. The variable is cast as character.
#' * `status_num`: The corresponding integer version of *status*.
#' * `n_status`: A mix of `status` and `n_events` cast as character. This is
#'   useful when modelling process progression.
#'
#' If `more_status` is passed, `augment()` computes additional variables.
#' They mirror the meaning of *status*, *status_num*, and *n_status* but they
#' account for the more complex structure defined. They are: `status_exp`,
#' `status_exp_num`, and `n_status_exp`.
#'
#' @examples
#' # loading data
#' data(hosp)
#'
#' # augmenting hosp
#' hosp_augmented = augment(data = hosp, data_key = subj, n_events = adm_number,
#'                           pattern = label_3, t_start = dateIN, t_end = dateOUT,
#'                           t_cens = dateCENS)
#'
#' # augmenting hosp by passing more information regarding transitions
#' # with argument more_status
#' hosp_augmented_more = augment(data = hosp, data_key = subj, n_events = adm_number,
#'                                pattern = label_3, t_start = dateIN, t_end = dateOUT,
#'                                t_cens = dateCENS, more_status = rehab_it)
#'
#' # requesting progress output
#' hosp_augmented = augment(data = hosp, data_key = subj, n_events = adm_number,
#'                           pattern = label_3, t_start = dateIN, t_end = dateOUT,
#'                           t_cens = dateCENS, verbosity = "summary")
#'
#' @references Grossetti, F., Ieva, F., and Paganoni, A.M. (2018).
#' A multi-state approach to patients affected by chronic heart failure.
#' *Health Care Management Science*, 21, 281-291.
#' \doi{10.1007/s10729-017-9400-z}.
#'
#' Jackson, C.H. (2011). Multi-State Models for Panel Data: The
#' **msm** Package for R. Journal of Statistical Software, 38(8), 1-29.
#' <https://www.jstatsoft.org/v38/i08/>.
#'
#' M. Dowle, A. Srinivasan, T. Short, S. Lianoglou with contributions from
#' R. Saporta and E. Antonyan (2016): **data.table**: Extension of `data.frame`.
#' R package version 1.9.6. <https://github.com/Rdatatable/data.table/wiki>
#'
#' @seealso [data.table::data.table()], [data.table::setkey()]
#' @author Francesco Grossetti <francesco.grossetti@unibocconi.it>.
#' @importFrom data.table setDT setkey setkeyv rbindlist uniqueN setcolorder setnames
#' @export

augment = function(data, data_key, n_events, pattern,
                    state = c("IN", "OUT", "DEAD"),
                    t_start, t_end, t_cens, t_death, t_augmented,
                    more_status = NULL, check_NA = FALSE, copy = FALSE,
                    verbosity = getOption("msmtools.verbosity", "quiet")) {

  tic = proc.time()
  verbosity = .msmtools_verbosity(verbosity)
  .msmtools_validate_flag(copy, "copy")
  .msmtools_validate_flag(check_NA, "check_NA")
  .msmtools_cli_rule(verbosity, "setting everything up")

  .augment_validate_inputs(
    data = data,
    state = state,
    missing_data = missing(data),
    missing_data_key = missing(data_key),
    missing_pattern = missing(pattern),
    missing_t_start = missing(t_start),
    missing_t_end = missing(t_end),
    missing_t_cens = missing(t_cens)
 )

  if (copy) {
    data = data.table::copy(data)
  }
  if (inherits(data, "data.frame")) {
    setDT(data)
  }

  data_key = as.character(substitute(data_key))
  pattern = as.character(substitute(pattern))
  t_start = as.character(substitute(t_start))
  t_end = as.character(substitute(t_end))
  t_cens = as.character(substitute(t_cens))
  n_events = if (missing(n_events)) NULL else as.character(substitute(n_events))
  t_death = if (missing(t_death)) NULL else as.character(substitute(t_death))
  t_augmented = if (missing(t_augmented)) {
    "augmented"
  } else {
    as.character(substitute(t_augmented))
  }
  more_status_arg = substitute(more_status)
  more_status = if (missing(more_status) || is.null(more_status_arg)) {
    NULL
  } else {
    as.character(more_status_arg)
  }

  if (is.null(t_death)) {
    warning("no t_death has been passed. Assuming that ", t_cens,
             " contains both censoring and death times")
  }

  .augment_check_time_classes(data, t_start, t_end, t_cens, t_death)
  cols = .augment_prepare_events(data, data_key, n_events, t_start, verbosity)

  if (isTRUE(check_NA)) {
    .msmtools_cli_info(verbosity, "checking for missing values")
    .augment_check_missing_values(
      data,
      c(cols, pattern, t_start, t_end),
      "function arguments"
   )
    .msmtools_cli_success(verbosity, "no missing values detected")
  }

  if (!is.null(more_status)) {
    .augment_check_missing_values(data, more_status, more_status)
  }

  values = .augment_pattern_values(data, pattern, verbosity)
  matches = .augment_pattern_matches(
    data, pattern, values, cols, t_end, t_cens, t_death
 )
  final = .augment_bind_rows(data, matches, cols, verbosity)
  maker = .augment_make_dimensions(
    data, cols, pattern, values, t_end, t_cens, t_death, verbosity
 )
  final = .augment_add_status(
    final, maker, cols, pattern, values, state, t_death, verbosity
 )
  final = .augment_add_numeric_status(final, "status", "status_num", verbosity)
  final = .augment_add_sequential_status(
    final, cols, state, "status", "n_status", verbosity
 )
  final = .augment_add_time_columns(
    final, data, state, t_start, t_end, t_cens, t_death, t_augmented,
    cols, pattern, values, verbosity
 )
  if (!is.null(more_status)) {
    final = .augment_add_expanded_status(
      final, data, more_status, cols, state, verbosity
   )
  }

  time = proc.time() - tic
  .msmtools_cli_rule(verbosity, paste0("augment() took: ", time[3], " sec."))
  final[]
  return(final)
}

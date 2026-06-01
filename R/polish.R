if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(":=", ".I", ".N", "index"))
}

#' Remove observations with different states occurring at the same time
#'
#' Remove subjects with transitions to different states occurring at the same
#' exact time in an augmented dataset produced by `augment()`.
#'
#' @inheritParams augment
#' @param time The time variable used to identify duplicate transition times.
#' If omitted or set to `NULL`, `polish()` uses `"augmented_int"` when it is
#' available, then `"augmented_num"`. If neither column exists, `time` must be
#' supplied explicitly.
#' @param check_NA If `TRUE`, `data_key`, `pattern`, and `time` are checked for
#' missing values. If any missing values are found, the function stops with an
#' error. Default is `FALSE`.
#' @param copy If `FALSE` (default), `polish()` keeps the historical
#' memory-efficient behavior and may modify caller-owned `data` by reference.
#' If `TRUE`, `data` is copied before any `data.table` operation so the input
#' object remains unchanged.
#'
#' @details The function searches for cases where two subsequent events for the
#' same subject land on different states but occur at the same time. When this
#' happens, the whole subject, as identified by `data_key`, is removed from the
#' data. The function reports how many subjects were removed.
#'
#' By default, `polish()` follows **data.table** by-reference semantics to avoid
#' unnecessary copies of large augmented datasets. This means the input may have
#' its key changed while duplicate subjects are identified. Set `copy = TRUE`
#' when the original input object must remain unchanged.
#'
#' The function always returns a `data.table`. Use [as.data.frame()] on the
#' result if a plain `data.frame` is needed by downstream code.
#'
#' @return A `data.table` with the same columns as the input `data`. Subjects
#' whose pattern transitions occur at the same `time` on different states are
#' removed in full (every row sharing the same `data_key`); rows from
#' unaffected subjects are kept as-is. When no duplicated transitions are
#' found, the input `data` is returned unchanged.
#'
#' @seealso [augment()]
#'
#' @examples
#'
#' # loading data
#' data(hosp)
#'
#' # augmenting longitudinal data
#' hosp_aug = augment(data = hosp, data_key = subj, n_events = adm_number,
#'                    pattern = label_3, t_start = dateIN, t_end = dateOUT,
#'                    t_cens = dateCENS)
#'
#' # cleaning targeted duplicate transitions
#' hosp_aug_clean = polish(data = hosp_aug, data_key = subj, pattern = label_3)
#'
#' @author Francesco Grossetti <francesco.grossetti@unibocconi.it>.
#' @export

polish = function(data, data_key, pattern, time = NULL, check_NA = FALSE,
                  copy = FALSE,
                  verbosity = getOption("msmtools.verbosity", "quiet")) {

  tic = proc.time()
  verbosity = .msmtools_verbosity(verbosity)
  .msmtools_validate_flag(copy, "copy")
  .msmtools_validate_flag(check_NA, "check_NA")

  if (missing(data)) {
    stop('a dataset of class data.table or data.frame must be provided')
  }
  if (!inherits(data, "data.table") && !inherits(data, "data.frame")) {
    stop("a dataset of class data.table or data.frame must be provided")
  }
  if (missing(data_key)) {
    stop('a variable of keying must be provided')
  }
  if (missing(pattern)) {
    stop("a pattern must be provided")
  }
  if (copy) {
    data = data.table::copy(data)
  }
  if (inherits(data, 'data.frame')) {
    data.table::setDT(data)
  }
  .msmtools_cli_rule(verbosity, "setting everything up")

  data.table::setkey(data, NULL)
  cols = as.character(substitute(data_key))
  if (!length(cols)) {
    cols = colnames(data)
  }
  pattern = as.character(substitute(pattern))
  time_arg = substitute(time)
  if (missing(time) || is.null(time_arg)) {
    time = .polish_resolve_time(data)
    .msmtools_cli_info(verbosity, paste0(time, " set as time variable"))
  } else {
    time = as.character(time_arg)
  }
  .polish_check_columns(data, c(cols, pattern, time))
  data.table::setkeyv(data, cols)

  if (isTRUE(check_NA)) {
    .msmtools_cli_info(
      verbosity,
      "checking for any missing values in function arguments"
    )
    checks = c(cols, pattern, time)
    test = apply(data[, checks, with = FALSE], 2,
                 function(x) any(sum(is.na(x)) > 0))
    if (any(test)) {
      missing_values = paste(names(test[test == TRUE]), collapse = ", ")
      stop(paste0("missing values detected in: ", missing_values))
    } else {
      .msmtools_cli_success(verbosity, "no missing values detected")
    }
  }

  data[, index := sequence(.N)]
  n_patients = data.table::uniqueN(data[[cols[[1]]]])
  values = .polish_pattern_values(data, pattern, verbosity)

  alive = data[get(pattern) == values[1]]
  alive.last = alive[alive[, .I[.N], by = eval(cols)]$V1]
  data.table::setkey(alive.last, index)
  data.table::setkey(alive, index)
  alive.no.last = alive[!alive.last]

  if (length(values) == 2) {
    dead = data[get(pattern) == values[2]]
  } else if (length(values) == 3) {
    dead = data[get(pattern) != values[1]]
  }

  l = list(alive.no.last, dead)
  data.no.last.event = data.table::rbindlist(l)
  row.duplicated = duplicated(data.no.last.event,
                              by = c(eval(cols), eval(time)))
  duplicated = data.no.last.event[row.duplicated == TRUE]
  n_duplicated = data.table::uniqueN(duplicated[[cols[[1]]]])
  data.table::setkeyv(duplicated, cols)

  if (n_duplicated == 0) {
    .msmtools_cli_success(
      verbosity,
      paste0("no duplicated occurrences found according to ", time)
    )
  } else {
    .msmtools_cli_info(
      verbosity,
      paste0(
        "spotted ", n_duplicated,
        " subjects with at least one duplicated occurrence according to ", time
      )
    )
    data.clean = data[!duplicated]
    n_patients.to.keep = data.table::uniqueN(data.clean[[cols[[1]]]])
    .msmtools_cli_success(
      verbosity,
      paste0(
        n_patients.to.keep, " subjects retained, corresponding to ",
        round(100 * (n_patients.to.keep / n_patients), 2), "%"
      )
    )
    .msmtools_cli_success(
      verbosity,
      "duplicated subjects have been successfully removed"
    )
  }

  data[, index := NULL]
  if (n_duplicated > 0) {
    data.clean[, index := NULL]
  }
  elapsed = proc.time() - tic
  .msmtools_cli_rule(
    verbosity,
    paste0("polish() took: ", elapsed[3], " sec.")
  )

  if (n_duplicated == 0) {
    data[]
    return(data)
  }
  data.clean[]
  return(data.clean)
}

#' Resolve the default duplicate-time column for `polish()`
#'
#' Selects `augmented_int` when available, falls back to `augmented_num`, and
#' errors when no default augmented time column exists.
#'
#' @keywords internal
#' @noRd
.polish_resolve_time = function(data) {
  if ("augmented_int" %in% names(data)) {
    return("augmented_int")
  }
  if ("augmented_num" %in% names(data)) {
    return("augmented_num")
  }
  stop("time must be provided when data does not contain augmented_int or augmented_num")
}

#' Check that required `polish()` columns exist
#'
#' Verifies that captured subject, pattern, and time columns are present before
#' any keying or duplicate-detection work starts.
#'
#' @keywords internal
#' @noRd
.polish_check_columns = function(data, columns) {
  missing_columns = setdiff(columns, names(data))
  if (length(missing_columns)) {
    stop(
      paste0(
        "the following columns are not present in data: ",
        paste(missing_columns, collapse = ", ")
      )
    )
  }
}

#' Extract and validate terminal pattern values for `polish()`
#'
#' Computes the unique pattern values and requires the same two- or three-value
#' terminal outcome schema used by `augment()`.
#'
#' @keywords internal
#' @noRd
.polish_pattern_values = function(data, pattern, verbosity) {
  values = sort(unique(data[[pattern]]))
  if (!length(values) %in% 2:3) {
    stop("pattern must have 2 or 3 unique values")
  }
  .msmtools_cli_info(
    verbosity,
    paste0("checking ", pattern, " and defining patterns")
  )
  .msmtools_cli_success(
    verbosity,
    paste0("detected ", length(values), " values in ", pattern)
  )
  values
}

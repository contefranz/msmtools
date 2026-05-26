.augment_validate_inputs = function(data, state, missing_data, missing_data_key,
                                     missing_pattern, missing_t_start,
                                     missing_t_end, missing_t_cens) {
  if (missing_data) {
    stop("a dataset of class data.table or data.frame must be provided")
  }
  if (!inherits(data, "data.table") && !inherits(data, "data.frame")) {
    stop("a dataset of class data.table or data.frame must be provided")
  }
  if (missing_data_key) {
    stop("a variable of keying must be provided")
  }
  if (missing_pattern) {
    stop("a pattern must be provided")
  }
  if (!is.character(state) || length(state) != 3 ||
       anyNA(state) || any(!nzchar(state)) || anyDuplicated(state)) {
    stop("state must be a character vector of 3 unique non-missing non-empty labels")
  }
  if (missing_t_start || missing_t_end) {
    stop("a starting and an ending event times must be provided")
  }
  if (missing_t_cens) {
    stop("a censoring time must be provided")
  }
}

.augment_check_time_classes = function(data, t_start, t_end, t_cens, t_death) {
  if (!identical(class(data[[t_start]]), class(data[[t_end]]))) {
    stop("the starting and the ending event times must be of the same class")
  }
  if (!identical(class(data[[t_start]]), class(data[[t_cens]]))) {
    stop("the starting and the censoring event times must be of the same class")
  }
  if (!is.null(t_death) &&
       !identical(class(data[[t_cens]]), class(data[[t_death]]))) {
    stop("the censoring and the death event times must be of the same class")
  }
}

.augment_prepare_events = function(data, data_key, n_events, t_start,
                                    verbosity) {
  setkey(data, NULL)
  if (!is.null(n_events)) {
    cols = c(data_key, n_events)
    if (length(n_events) != 1L || !n_events %in% names(data) ||
         !inherits(data[[n_events]], "integer")) {
      stop("n_events must be an integer")
    }
  } else {
    cols = data_key
    setkeyv(data, c(data_key, t_start))
    data[, n_events := seq(.N), by = eval(data_key)]
    cols = c(cols, "n_events")
  }

  .msmtools_cli_info(verbosity, paste0("checking monotonicity of ", cols[[2]]))
  ev = data[, .(ev = all(get(cols[[2]]) == cummax(get(cols[[2]])))),
             by = eval(cols[[1]])]
  setkeyv(data, c(cols[[1]], t_start))
  if (!all(ev$ev)) {
    problem_subjects = ev[ev == FALSE][, get(cols[[1]])]
    .msmtools_cli_info(
      verbosity,
      paste0(cols[[2]], " is not monotonic increasing within ", cols[[1]])
   )
    .msmtools_cli_info(
      verbosity,
      paste0("the corresponding subjects are: ",
              paste(problem_subjects, collapse = "; "))
   )
    stop("Please, fix the issues and relaunch augment()")
  }
  .msmtools_cli_success(verbosity, paste0(cols[[2]], " is monotonic"))
  setkeyv(data, cols)
  cols
}

.augment_check_missing_values = function(data, cols, what) {
  missing_cols = cols[vapply(cols, function(x) anyNA(data[[x]]),
                               logical(1L))]
  if (length(missing_cols) > 0) {
    stop(
      "detected missing values in ", what, ": ",
      paste(missing_cols, collapse = ", "),
      ". Please, fix the issues and relaunch augment()"
   )
  }
}

.augment_pattern_values = function(data, pattern, verbosity) {
  values = sort(unique(data[[pattern]]))
  .msmtools_cli_info(verbosity, paste0("checking ", pattern,
                                         " and defining patterns"))
  if (length(values) < 2) {
    stop(paste0("unit identification label must be an integer, a factor or ",
                  "a character with at least 2 elements"))
  }
  if (length(values) > 3) {
    stop("pattern must have 2 or 3 unique values")
  }
  .msmtools_cli_success(
    verbosity,
    paste0("detected ", length(values), " values in ", pattern)
 )
  values
}

.augment_last_pattern_index = function(data, pattern, data_key, code, value) {
  if (inherits(data[[pattern]], c("integer", "numeric"))) {
    data[get(pattern) == code, .I[.N], by = eval(data_key)]$V1
  } else if (inherits(data[[pattern]], "factor")) {
    data[as.integer(get(pattern)) - 1 == code,
          .I[.N], by = eval(data_key)]$V1
  } else if (inherits(data[[pattern]], "character")) {
    data[get(pattern) == value, .I[.N], by = eval(data_key)]$V1
  } else {
    stop("pattern must be an integer, a factor or a character")
  }
}

.augment_pattern_matches = function(data, pattern, values, cols, t_end,
                                     t_cens, t_death) {
  data_key = cols[[1]]
  if (length(values) == 2) {
    match1 = data[
      .augment_last_pattern_index(data, pattern, data_key, 0, values[1])
   ]
    match3 = data[
      .augment_last_pattern_index(data, pattern, data_key, 1, values[2])
   ]
    death_time = if (is.null(t_death)) t_cens else t_death
    match3 = match3[get(t_end) != get(death_time)]
  } else {
    match1 = data[
      .augment_last_pattern_index(data, pattern, data_key, 0, values[1])
   ]
    match3 = data[
      .augment_last_pattern_index(data, pattern, data_key, 2, values[3])
   ]
  }
  list(match1 = match1, match3 = match3)
}

.augment_bind_rows = function(data, matches, cols, verbosity) {
  .msmtools_cli_info(verbosity, "augmenting data")
  final = rbindlist(list(data, data, matches$match1, matches$match3))
  setkeyv(final, cols)
  .msmtools_cli_success(verbosity, "data have been augmented")
  final
}

.augment_make_dimensions = function(data, cols, pattern, values, t_end, t_cens,
                                     t_death, verbosity) {
  .msmtools_cli_info(verbosity, "defining dimensions")
  if (length(values) == 2) {
    if (is.null(t_death)) {
      t1 = data[, .(.N,
                      t_end = max(get(t_end)),
                      t_cens = max(get(t_cens))), by = eval(cols[[1]])]
    } else {
      t1 = data[, .(.N,
                      t_end = max(get(t_end)),
                      t_death = max(get(t_death))), by = eval(cols[[1]])]
    }
  } else {
    t1 = data[, .N, by = eval(cols[[1]])]
  }
  setkeyv(t1, cols[[1]])
  t2 = unique(data[, .(get(cols[[1]]), get(pattern))])
  setnames(t2, c(cols[[1]], "V2"))
  setkeyv(t2, cols[[1]])
  maker = t1[t2]
  setkeyv(data, cols[[1]])
  .msmtools_cli_success(verbosity, "dimensions computed")
  maker
}

.augment_status_sequence = function(n, kind, state) {
  if (identical(kind, "alive")) {
    return(c(rep(c(state[[1]], state[[2]]), n), state[[2]]))
  }
  if (identical(kind, "dead_in")) {
    return(c(rep(c(state[[1]], state[[2]]), n - 1L),
               state[[1]], state[[3]]))
  }
  c(rep(c(state[[1]], state[[2]]), n), state[[3]])
}

.augment_build_status_list = function(n, kind, state, verbosity, name) {
  out = vector(mode = "list", length(n))
  progress = .msmtools_cli_progress(verbosity, name, length(n))
  for (i in seq_along(n)) {
    out[[i]] = .augment_status_sequence(n[[i]], kind, state)
    .msmtools_cli_progress_update(progress, i, length(n))
  }
  .msmtools_cli_progress_done(progress)
  out
}

.augment_add_status_two_value = function(final, maker, cols, pattern, values,
                                          state, t_death, verbosity) {
  a = maker[V2 == values[1]]
  if (is.null(t_death)) {
    din  = maker[V2 == values[2] & t_end == t_cens]
    dout = maker[V2 == values[2] & t_end != t_cens]
  } else {
    din  = maker[V2 == values[2] & t_end == t_death]
    dout = maker[V2 == values[2] & t_end != t_death]
  }

  temp1 = din[, .SD, .SDcols = cols[[1]]]
  temp2 = dout[, .SD, .SDcols = cols[[1]]]
  setkeyv(temp1, cols[[1]])
  setkeyv(temp2, cols[[1]])
  setkeyv(final, cols[[1]])
  din_long  = final[temp1]
  dout_long = final[temp2]
  a_long = final[get(pattern) == values[1]]

  .msmtools_cli_info(verbosity, "processing alive units")
  flag_a = unlist(
    .augment_build_status_list(a$N, "alive", state, verbosity, "alive units"),
    recursive = FALSE
 )
  .msmtools_cli_info(verbosity, "processing units dead inside a transition")
  flag_din = unlist(
    .augment_build_status_list(din$N, "dead_in", state, verbosity,
                                "dead inside transition"),
    recursive = FALSE
 )
  .msmtools_cli_info(verbosity, "processing units dead outside a transition")
  flag_dout = unlist(
    .augment_build_status_list(dout$N, "dead_out", state, verbosity,
                                "dead outside transition"),
    recursive = FALSE
 )

  a_long[, status := flag_a]
  din_long[, status := flag_din]
  dout_long[, status := flag_dout]
  final = rbindlist(list(a_long, din_long, dout_long))
  setkeyv(final, cols)
  final
}

.augment_add_status_three_value = function(final, maker, values, state,
                                            verbosity) {
  flag_temp = vector(mode = "list", nrow(maker))
  progress = .msmtools_cli_progress(verbosity, "status patterns", nrow(maker))
  for (i in seq_along(maker$N)) {
    if (maker$V2[i] == values[1]) {
      flag_temp[[i]] = .augment_status_sequence(maker$N[i], "alive", state)
    } else if (maker$V2[i] == values[2]) {
      flag_temp[[i]] = .augment_status_sequence(maker$N[i], "dead_in", state)
    } else if (maker$V2[i] == values[3]) {
      flag_temp[[i]] = .augment_status_sequence(maker$N[i], "dead_out", state)
    }
    .msmtools_cli_progress_update(progress, i, length(maker$N))
  }
  .msmtools_cli_progress_done(progress)
  final[, status := unlist(flag_temp, recursive = FALSE)]
  final
}

.augment_add_status = function(final, maker, cols, pattern, values, state,
                                t_death, verbosity) {
  .msmtools_cli_info(verbosity, "adding status flag")
  if (length(values) == 2) {
    final = .augment_add_status_two_value(final, maker, cols, pattern, values,
                                           state, t_death, verbosity)
  } else {
    final = .augment_add_status_three_value(final, maker, values, state,
                                             verbosity)
  }
  if (anyNA(final$status)) {
    stop("status flag has not been built correctly")
  }
  .msmtools_cli_success(verbosity, "status flag has been added successfully")
  final
}

.augment_add_numeric_status = function(final, status_col, status_num_col,
                                        verbosity) {
  .msmtools_cli_info(verbosity, paste0("adding numeric ", status_col, " flag"))
  lev = unique(final[[status_col]])
  for (i in seq_along(lev)) {
    final[get(status_col) == lev[i], (status_num_col) := i]
  }
  if (anyNA(final[[status_num_col]])) {
    stop(paste0("numeric ", status_col, " has not been built correctly"))
  }
  .msmtools_cli_success(
    verbosity,
    paste0("numeric ", status_col, " has been added successfully")
 )
  final
}

.augment_add_sequential_status = function(final, cols, state, status_col,
                                           n_status_col, verbosity) {
  .msmtools_cli_info(verbosity, paste0("adding sequential ", status_col, " flag"))
  final[get(status_col) != state[[3]],
         (n_status_col) := paste(get(cols[[2]]), " ",
                                    get(status_col), sep = "")]
  final[get(status_col) == state[[3]], (n_status_col) := state[[3]]]
  if (anyNA(final[[n_status_col]])) {
    stop(paste0("sequential ", status_col, " flag has not been built correctly"))
  }
  .msmtools_cli_success(
    verbosity,
    paste0("sequential ", status_col, " flag has been added successfully")
 )
  final
}

.augment_set_time_order = function(final, data, t_start, t_augmented, suffix) {
  id_col = which(names(data) == t_start)
  before = if (id_col > 1L) seq_len(id_col - 1L) else integer()
  if (is.null(suffix)) {
    setcolorder(final, c(before, ncol(final), id_col:(ncol(final) - 1L)))
  } else {
    setcolorder(
      final,
      c(before, ncol(final) - 1L, ncol(final),
         id_col:(ncol(final) - 2L))
   )
  }
  final
}

.augment_add_time_columns = function(final, data, state, t_start, t_end,
                                      t_cens, t_death, t_augmented,
                                      cols, pattern, values,
                                      verbosity) {
  .msmtools_cli_info(
    verbosity,
    paste0("adding variable ", t_augmented, " as new time variable")
 )
  final[status == state[[1]], (t_augmented) := get(t_start)]
  final[status == state[[2]], (t_augmented) := get(t_end)]
  # The status sequence for alive subjects ends with a trailing OUT row that
  # represents the post-discharge observation window up to t_cens. Without
  # this override that row would inherit t_end (the last dateOUT), collapsing
  # the censoring window to zero and biasing transition-rate estimates
  # downward when the augmented data is fed to msm::msm(). Closes #7.
  last_out_alive = final[
    status == state[[2]] & get(pattern) == values[[1]],
    .I[.N],
    by = eval(cols[[1]])
  ]$V1
  if (length(last_out_alive)) {
    final[last_out_alive, (t_augmented) := get(t_cens)]
  }
  death_time = if (is.null(t_death)) t_cens else t_death
  final[status == state[[3]], (t_augmented) := get(death_time)]

  if (inherits(data[[t_start]], "Date")) {
    int_col = paste0(t_augmented, "_int")
    final[, (int_col) := as.integer(get(t_augmented))]
    final = .augment_set_time_order(final, data, t_start, t_augmented, "_int")
    .msmtools_cli_success(
      verbosity,
      paste0("variables ", t_augmented, " and ", int_col,
              " successfully added and repositioned")
   )
  } else if (inherits(data[[t_start]], "difftime")) {
    num_col = paste0(t_augmented, "_num")
    final[, (num_col) := as.numeric(get(t_augmented))]
    final = .augment_set_time_order(final, data, t_start, t_augmented, "_num")
    .msmtools_cli_success(
      verbosity,
      paste0("variables ", t_augmented, " and ", num_col,
              " successfully added and repositioned")
   )
  } else if (inherits(data[[t_start]], "integer") ||
              inherits(data[[t_start]], "numeric")) {
    final = .augment_set_time_order(final, data, t_start, t_augmented, NULL)
    .msmtools_cli_success(
      verbosity,
      paste0("variable ", t_augmented, " successfully added and repositioned")
   )
  }
  final
}

.augment_add_expanded_status = function(final, data, more_status, cols, state,
                                         verbosity) {
  .msmtools_cli_info(
    verbosity,
    paste0("detected a more complex status given by variable ", more_status)
 )
  .msmtools_cli_info(verbosity, "adding expanded status flag")
  values = unique(data[[more_status]])
  final[status == state[[3]], status_exp := state[[3]]]
  for (i in seq_along(values)) {
    final[status != state[[3]] & get(more_status) == values[i],
           status_exp := paste(values[i], "_", status, sep = "")]
  }
  if (anyNA(final$status_exp)) {
    stop("expanded status flag has not been built correctly")
  }
  .msmtools_cli_success(verbosity, "expanded status flag has been added successfully")

  final = .augment_add_numeric_status(final, "status_exp", "status_exp_num",
                                       verbosity)
  .augment_add_sequential_status(final, cols, state, "status_exp",
                                  "n_status_exp", verbosity)
}

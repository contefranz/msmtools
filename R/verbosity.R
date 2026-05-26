.msmtools_verbosity = function(verbosity) {
  verbosity = match.arg(verbosity, c("quiet", "summary", "progress"))
  verbosity
}

.msmtools_is_summary = function(verbosity) {
  verbosity %in% c("summary", "progress")
}

.msmtools_cli_rule = function(verbosity, text) {
  if (.msmtools_is_summary(verbosity)) {
    cli::cli_rule(text)
  }
}

.msmtools_cli_info = function(verbosity, text) {
  if (.msmtools_is_summary(verbosity)) {
    cli::cli_alert_info(text)
  }
}

.msmtools_cli_success = function(verbosity, text) {
  if (.msmtools_is_summary(verbosity)) {
    cli::cli_alert_success(text)
  }
}

.msmtools_cli_progress = function(verbosity, name, total) {
  if (identical(verbosity, "progress") && total > 0) {
    return(
      cli::cli_progress_bar(
        name,
        total = total,
        auto_terminate = FALSE,
        .envir = parent.frame()
     )
   )
  }
  NULL
}

.msmtools_cli_progress_update = function(id, i, total) {
  if (!is.null(id)) {
    step = max(1L, floor(total / 100L))
    if (i == total || i %% step == 0L) {
      cli::cli_progress_update(id = id, set = i)
    }
  }
}

.msmtools_cli_progress_done = function(id) {
  if (!is.null(id)) {
    cli::cli_progress_done(id = id)
  }
}

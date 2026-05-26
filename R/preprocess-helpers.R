.msmtools_validate_flag = function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop(name, " must be either TRUE or FALSE")
  }
}

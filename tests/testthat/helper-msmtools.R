test_hosp = function() {
  utils::data("hosp", package = "msmtools", envir = environment())
  data.table::copy(hosp)
}

augment_hosp = function(data = test_hosp(), pattern = c("label_3", "label_2"),
                         ...) {
  pattern = match.arg(pattern)
  if (pattern == "label_3") {
    suppressWarnings(
      augment(data = data, data_key = subj, n_events = adm_number,
               pattern = label_3, t_start = dateIN, t_end = dateOUT,
               t_cens = dateCENS, ...)
   )
  } else {
    suppressWarnings(
      augment(data = data, data_key = subj, n_events = adm_number,
               pattern = label_2, t_start = dateIN, t_end = dateOUT,
               t_cens = dateCENS, ...)
   )
  }
}

test_qmatrix = function() {
  qmat = matrix(data = 0, nrow = 3, ncol = 3, byrow = TRUE)
  qmat[1, 1:3] = 1
  qmat[2, 1:3] = 1
  colnames(qmat) = c("IN", "OUT", "DEAD")
  rownames(qmat) = c("IN", "OUT", "DEAD")
  qmat
}

test_msm_fit = function() {
  hosp_augmented = augment_hosp()
  fit = msm::msm(status_num ~ augmented_int, subject = subj,
                  data = hosp_augmented, exacttimes = TRUE, gen.inits = TRUE,
                  qmatrix = test_qmatrix(), method = "BFGS",
                  control = list(fnscale = 6e+05, trace = 0, REPORT = 1,
                                  maxit = 10000))
  attr(fit, "msmtools_data") = hosp_augmented
  fit
}

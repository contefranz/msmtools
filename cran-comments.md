# msmtools 2.1.3

### Release summary

This is a CRAN-hardening release. It demotes `patchwork` from `Imports` to
`Suggests` (now required only by `prevplot(M = TRUE)`) and removes `scales`
entirely by replacing its single use with an inline percent labeller. The
package's framing has also been generalised away from a hospital-only narrative;
the bundled example dataset and worked example are unchanged. There are no
user-facing API changes.

The 2.0.3 archival/resubmission story is carried forward unchanged: msmtools
was archived on 2024-09-27 because compatibility issues were not corrected
despite reminders. Since 2.0.3 the package has been actively maintained against
modern releases of its runtime dependencies.

### Package development

* macOS Tahoe 26.5 with R 4.5.1

### R CMD build

* local macOS build with `R CMD build`
* vignette built locally with pandoc discovered through Quarto
* win-builder R-release and R-devel submissions pending at the time of writing
* GitHub Actions matrix (macOS R-release, Windows R-release, Ubuntu R-devel,
  R-release, R-oldrel-1) green on the release branch

### R CMD check results

* Target: 0 errors, 0 warnings, 0 notes on local `R CMD check --as-cran`.
* The `--no-suggests` path was verified locally via
  `_R_CHECK_DEPENDS_ONLY_=true R CMD check --as-cran`: the conditional
  patchwork test is skipped via `testthat::skip_if_not_installed("patchwork")`
  and `prevplot(M = FALSE)` continues to work without `patchwork` installed.
* The only expected NOTE in restricted environments is the offline URL/DOI
  check (JSS DOI and CRAN incoming feasibility). These resolve correctly when
  the check has network access.

***

# msmtools 2.2.1

### Resubmission after archival

This is the first CRAN submission of **msmtools** since the package was
archived on **2024-09-27**. The archival was caused by uncorrected
compatibility issues with newer **data.table** releases: `substitute()`
calls on the left-hand side of `:=` assignments (a non-standard
evaluation pattern that **data.table** deprecated), plus
`eval(substitute(data$var))` constructs throughout `augment()` and
`polish()`. Those constructs produced errors against current
**data.table** versions on CRAN.

The non-standard evaluation cleanup landed in **2.0.2** (column
assignment now uses the recommended `(var) :=` idiom and per-column
access goes through `data[[var]]`). That fix was bundled into the
**2.0.3** resubmission, but the resubmission was not completed
end-to-end because rhub checks could not run locally at the time and
win-builder feedback was still pending. Subsequent maintenance work has
since continued on GitHub without another submission attempt.

`R CMD check --as-cran` will emit the `New submission / Package was
archived on CRAN` NOTE; that NOTE is expected and acknowledged here.

### Maintenance arc since 2.0.3

Between the never-completed 2.0.3 resubmission and this 2.2.0
submission the package has gone through nine point releases. The main
milestones are:

* **2.0.4 – 2.0.10**: raised the supported baseline to R 4.1 and
  current CRAN releases of the runtime imports; split `augment()` into
  private helpers for validation, event preparation, pattern matching,
  status construction, time-column creation, and expanded-status
  handling; `augment()` and `polish()` now always return `data.table`
  objects.
* **2.1.0 – 2.1.2**: replaced the `verbose` argument (which manipulated
  `sink()` and warning options) with a `verbosity` argument routed
  through **cli**. Levels are `"quiet"`, `"summary"`, and `"progress"`,
  with the latter producing optional progress bars through
  `cli::cli_progress_bar()`. The same refactor was applied to
  `polish()` and to the plotting functions.
* **2.1.3**: dependency tightening. `scales` was removed entirely (its
  single percent-formatter use replaced by an inline labeller).
  `patchwork` was moved from `Imports` to `Suggests`, since it is only
  required by `prevplot(M = TRUE)`; the call site now guards with
  `requireNamespace()` and raises an informative error if the package
  is not installed. Documentation framing was generalised away from
  hospital-only language.
* **2.1.4**: **bug fix with statistical implications.**
  [Issue #7](https://github.com/contefranz/msmtools/issues/7) reported
  that `augment()` was collapsing the post-discharge at-risk window for
  alive subjects — the final transition row used the last `t_end`
  instead of `t_cens`, so the at-risk window for any subject who
  survived to the censoring date was truncated to zero duration. Any
  `msm` model fitted on the augmented output therefore biased
  transition-rate estimates downward. The fix routes the trailing OUT
  row through `t_cens` for alive subjects only; the behaviour for
  subjects who died is unchanged. This is a deliberate behavioural
  change. Models refit on output produced by `augment()` from
  msmtools 2.1.4 and later will give different (and more correct)
  parameter estimates than models fitted on output from earlier
  versions. The change is documented prominently in `NEWS.md` and
  the internal regression fixture was regenerated against the
  corrected output.

### Current release (2.2.0)

`survplot()` and `prevplot()` are now brought to a single return
contract. The `out = c("none", "fitted", "km", "all")` argument on
`survplot()` has been removed; the function always returns a
`gg/ggplot` object, with the fitted survival and Kaplan-Meier data
tables exposed as named fields on that plot (`p$fitted`, `p$km`).
`prevplot()` exposes the underlying long-format prevalence data via
`p$prevalence` for parity. Closes
[issue #4](https://github.com/contefranz/msmtools/issues/4).

This is an intentional breaking change. Calls passing `out = ...` raise
a clear migration error pointing to the new pattern via a one-release
trampoline (the `...` capture inside `survplot()`), which will itself
be removed in v2.3.0. The version bump is to 2.2.0 rather than 3.0.0:
msmtools has a narrow plotting API and a small user base, and the
prominent NEWS entry plus the migration trampoline are sufficient to
keep the upgrade path smooth without inflating the major version.

### Resubmission 2.2.1

This is a documentation-only patch in response to Benjamin Altmann's
feedback on the 2.2.0 submission. Two items were flagged:

* `man/polish.Rd` was missing a `\value{}` tag. `R/polish.R` now
  carries an `@return` roxygen block, and `man/polish.Rd` was
  regenerated through `roxygen2::roxygenise()`. The other exported
  Rd files (`augment.Rd`, `prevplot.Rd`, `survplot.Rd`) already
  document `\value{}`; `hosp.Rd` is a dataset Rd and
  `msmtools-package.Rd` is the package-level Rd.
* The DESCRIPTION's Description field did not cite the underlying
  method. Grossetti, Ieva and Paganoni (2018)
  <doi:10.1007/s10729-017-9400-z> was added in the
  `authors (year) <doi:...>` form required by the CRAN cookbook. The
  same DOI is already cited under `@references` in `augment()`.

No source-code behaviour changed between 2.2.0 and 2.2.1; the prior
archival cause (data.table NSE in `augment()` and `polish()`) remains
fixed since 2.0.2 as described above.

### Package development environment

* macOS Tahoe 26.5 with R 4.5.1.

### R CMD build

* Local macOS build with `R CMD build .`; pandoc is discovered through
  Quarto for vignette rendering.
* win-builder R-release and R-devel submissions pending at the time of
  writing.
* GitHub Actions matrix — macOS R-release, Windows R-release, Ubuntu
  R-devel, Ubuntu R-release, Ubuntu R-oldrel-1 — green on the release
  branch.

### R CMD check results

* Target: 0 errors, 0 warnings, 0 notes on `R CMD check --as-cran`.
* The `--no-suggests` path is verified locally via
  `_R_CHECK_DEPENDS_ONLY_=true R CMD check --as-cran`: the conditional
  `patchwork` test in `tests/testthat/test-plots.R` skips correctly
  through `testthat::skip_if_not_installed("patchwork")`, and
  `prevplot(M = FALSE)` continues to work without `patchwork`
  installed.
* The expected NOTE in restricted environments is the offline URL/DOI
  check (JSS DOI on `prevplot()`'s `@references` block, plus the
  CRAN-incoming feasibility check). These resolve correctly when the
  check has network access. The `New submission / Package was archived
  on CRAN` NOTE is also expected and is acknowledged in the
  resubmission framing above.

***

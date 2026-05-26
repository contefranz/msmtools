# msmtools 2.1.4

### Release summary

This release fixes a correctness bug in `augment()`. For subjects who
survived to the study censoring date, the final transition row inherited
the time of the previous row (last `t_end`) instead of `t_cens`. As a
result, the post-discharge observation window was collapsed to zero
duration and any `msm` model fitted on the augmented data systematically
under-estimated transition rates. The fix routes the trailing OUT row
through `t_cens`, which is what the package documentation and reporter
[#7](https://github.com/contefranz/msmtools/issues/7) expected.

Behaviour for subjects who died is unchanged. There are no other
user-facing API changes. The internal regression fixture has been
regenerated against the corrected output; the bracket-spacing style
sweep started in 2.1.3 is also completed in this release.

### Package development

* macOS Tahoe 26.5 with R 4.5.1

### R CMD build

* local macOS build with `R CMD build`
* vignette built locally with pandoc discovered through Quarto
* win-builder R-release and R-devel submissions pending at the time of
  writing
* GitHub Actions matrix (macOS R-release, Windows R-release, Ubuntu
  R-devel, R-release, R-oldrel-1) green on the release branch

### R CMD check results

* Target: 0 errors, 0 warnings, 0 notes on local `R CMD check --as-cran`.
* The `--no-suggests` path is verified locally via
  `_R_CHECK_DEPENDS_ONLY_=true R CMD check --as-cran`: the conditional
  `patchwork` test is skipped through `testthat::skip_if_not_installed`
  and `prevplot(M = FALSE)` continues to work without `patchwork`
  installed.
* The only expected NOTE in restricted environments is the offline
  URL/DOI check (JSS DOI and CRAN incoming feasibility). These resolve
  correctly when the check has network access.

***

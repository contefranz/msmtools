# msmtools 2.2.1
***

### DOCUMENTATION

* Added a `\value{}` section to `polish()` via a new `@return` roxygen
  block, addressing CRAN feedback that `man/polish.Rd` was missing the
  `\value` tag.
* Added the method citation to the `DESCRIPTION` Description field in
  the `authors (year) <doi:...>` form required by CRAN: Grossetti,
  Ieva and Paganoni (2018) <doi:10.1007/s10729-017-9400-z>. The same
  reference was already documented under `@references` in `augment()`.

***

# msmtools 2.2.0
***

### BREAKING CHANGES

* `survplot()` no longer accepts the `out` argument and now always
  returns a `gg/ggplot` object. The fitted survival curve and the
  Kaplan-Meier curve are exposed as named fields on the returned plot
  for parity with `prevplot()`:

  ```r
  p <- survplot(model, km = TRUE)
  p           # prints the plot
  p$fitted    # data.table — survival probabilities
  p$km        # data.table — Kaplan-Meier curve
  ```

  Closes [#4](https://github.com/contefranz/msmtools/issues/4).

  Code that previously did `out <- survplot(..., out = "all")` and then
  unwrapped through `out$p`, `out$fitted`, `out$km` should drop the
  `out` argument and access the data tables directly on the plot. The
  return type collapsing from a wrapper list to a ggplot means that
  ggplot operations now compose without unwrapping: `p + theme_bw()`,
  `ggsave("plot.pdf", p)`, and `patchwork`-style combinations all work
  on the returned plot directly.

  Calls passing `out = ...` raise a clear migration error pointing to
  the new pattern. The trampoline that catches the legacy argument
  will itself be removed in v2.3.0.

### NEW

* `prevplot()` exposes the long-format prevalence data used to build
  the plot via `p$prevalence`, so both plotting functions now expose
  their data through the same `$` idiom. The field is populated on
  both the `M = FALSE` (`ggplot`) and `M = TRUE` (`patchwork`) return
  paths.

### TESTS

* Replaced the test suite for `survplot()` `out = ...` shapes with
  attribute-field assertions on the returned plot, plus a regression
  test for the legacy-argument trampoline.
* Added a regression test asserting `prevplot()` populates
  `$prevalence` on the `M = FALSE`, `ci = TRUE`, and `M = TRUE` paths.

### DOCUMENTATION

* Rewrote the `survplot()` roxygen `@returns` and `@details` sections
  around the new contract. The vignette and worked examples in
  `man/survplot.Rd` now demonstrate the `$fitted` / `$km` access
  pattern.
* Documented the `$prevalence` field on `prevplot()`'s `@returns`.

# msmtools 2.1.4
***

### BUG FIX

* `augment()` now closes the post-discharge observation window of alive
  subjects at `t_cens` instead of repeating the last `t_end`. Previously,
  the final transition row for any subject who survived to the censoring
  date carried the same time as the preceding row, silently truncating
  the at-risk window. Any `msm` model fitted on the augmented data was
  therefore biased: transition-rate estimates were systematically pulled
  downward because every alive subject's post-discharge follow-up was
  collapsed to zero duration. Closes
  [#7](https://github.com/contefranz/msmtools/issues/7).

  Models refit on `augment()` output produced with **msmtools 2.1.4** or
  later will give different (and more correct) parameter estimates than
  models fitted on output from earlier versions. The behaviour of
  subjects who died (`pattern` values 1 or 2) is unchanged.

### TESTS

* The internal regression fixture
  `tests/testthat/fixtures/augment-hosp-date.rds` has been regenerated
  against the corrected `augment()` output.

* Added regression coverage for the at-risk window of alive subjects and
  for the unchanged time semantics of subjects who died.

### CODE STYLE

* Completed the data.table bracket-spacing normalisation started in the
  2.1.3 audit. `R/augment.R`, `R/augment-helpers.R`, and the testthat
  files now share the unspaced `dt[, col := value]` convention already
  used by the plotting and polish sources.

* `.onAttach()` is excluded from coverage reports via `# nocov` markers;
  startup messages add nothing to test signal.

# msmtools 2.1.3
***

### DEPENDENCIES

* `patchwork` moved from `Imports` to `Suggests`. It is only required by
  `prevplot(M = TRUE)`, which now issues an informative error when the package
  is not installed.

* `scales` removed entirely. The single percent formatter used by `prevplot()`
  is now an inline labeller, eliminating a hard runtime dependency.

### DOCUMENTATION

* Generalised the package framing to longitudinal observational data of any
  domain; the bundled hospital dataset remains the worked example but is no
  longer presented as the only use case.

* Minor wording tweaks in `augment()` and `prevplot()` Rd to drop
  clinical-specific terminology where it did not add precision.

### TESTS

* Added a conditional regression test for `prevplot(M = TRUE)` guarded by
  `testthat::skip_if_not_installed("patchwork")`, so the suite remains green on
  CRAN's `--no-suggests` runs.

### BUILD

* Extended `.Rbuildignore` to exclude generated tarballs, PDFs, and any local
  planning scratch directories.

# msmtools 2.1.2
***

### CHANGES

* Added `print_plot` to `survplot()` and `prevplot()`. The default
  `print_plot = TRUE` preserves the previous interactive behavior, while
  `print_plot = FALSE` returns plot objects without printing them.

* Added `verbosity` to `survplot()` and `prevplot()` and replaced direct console
  output with the shared `cli` verbosity helpers.

* Added clearer argument validation for plotting flags, grid sizes, bootstrap
  sizes, time ranges, custom times, and state selectors.

* Qualified plotting calls to external package functions explicitly instead of
  relying on roxygen imports.

### DOCUMENTATION

* Documented plotting print controls and verbosity in function help and README.

### TESTS

* Added regression coverage for silent plotting returns, summary verbosity, and
  invalid plotting arguments.

# msmtools 2.1.1
***

### CHANGES

* Made `polish()` time-column handling explicit. Omitting `time` or setting
  `time = NULL` now auto-detects `augmented_int`, then `augmented_num`; calls
  error clearly when neither column is available.

* Replaced remaining `polish()` console output with the shared `cli` verbosity
  helpers used by `augment()`.

* Qualified `polish()` calls to external **data.table** functions explicitly
  instead of relying on roxygen imports.

* Added clearer validation for `polish()` inputs, including missing columns and
  terminal outcome schemas with anything other than 2 or 3 unique values.

### DOCUMENTATION

* Documented the `polish()` time-column auto-detection behavior.

### TESTS

* Added regression coverage for `polish()` time resolution, missing columns,
  invalid pattern schemas, missing-value checks, and summary verbosity.

# msmtools 2.1.0
***

### CHANGES

* Added a `copy` argument to `augment()` and `polish()`. The default
  `copy = FALSE` preserves the historical memory-efficient by-reference
  behavior, while `copy = TRUE` protects caller-owned data before preprocessing.

* Clarified the preprocessing contract around **data.table** by-reference
  semantics. `augment()` may add `n_events` and change the input key when
  `copy = FALSE`; `polish()` may change the input key while identifying
  duplicate transitions.

* Added shared validation for scalar logical flags used by preprocessing
  functions.

* Made the `augment()` state vocabulary stricter and clearer: custom `state`
  values must now be supplied as a character vector of three unique labels, not
  as a list.

* Made `more_status = NULL` explicit in `augment()` so the optional expanded
  status path is visible in the function signature.

### DOCUMENTATION

* Documented the performance and safety tradeoff behind `copy = FALSE` and
  `copy = TRUE` in the function help, README, and vignette.

* Clarified that `pattern` describes the terminal outcome schema, while `state`
  describes the generated transition-state labels.

### TESTS

* Added tests covering by-reference preservation with `copy = FALSE`, input
  protection with `copy = TRUE`, and validation of logical flags.

* Added tests for custom state vectors, invalid state specifications, and
  explicit `more_status = NULL`.

# msmtools 2.0.10
***

### CHANGES

* Split `augment()` into private helpers for validation, event preparation,
  pattern matching, status construction, time-column creation, and expanded
  status handling.

* Replaced the `verbose` flag in `augment()` and `polish()` with a
  `verbosity` argument. The accepted levels are `"quiet"`, `"summary"`, and
  `"progress"`, with a global default available through
  `options(msmtools.verbosity = ...)`.

* `augment()` and `polish()` now always return `data.table` objects. The
  previous `convert` argument has been removed; use `as.data.frame()` on the
  result when a plain `data.frame` is required.

### TESTS

* Updated preprocessing tests for the `data.table`-only return contract and
  added coverage for the new `verbosity` argument.

# msmtools 2.0.9
***

### CHANGES

* Added dev-only performance baselines for `augment()` and `polish()` ahead of
  the planned internal refactor.

### TESTS

* Added invariant tests for output structure, by-reference behavior, and
  generated columns that future refactors must preserve.

# msmtools 2.0.8
***

### DOCUMENTATION

* Converted package documentation sources to **roxygen2** markdown style.

* Kept the vignette on the lightweight HTML vignette workflow while cleaning
  old LaTeX-style bibliography markup.

# msmtools 2.0.7
***

### DOCUMENTATION

* Refreshed the README and vignette around the current 2.x workflow and modern
  dependency baseline.

* Replaced heavier multi-state model examples with guarded examples so CRAN
  checks do not execute model-fitting workflows.

### CHANGES

* Standardized recent NEWS headings around explicit maintenance categories.

# msmtools 2.0.6
***

### TESTS

* Reworked the existing tests so expected warnings are asserted inside
  `test_that()` blocks instead of being emitted during test setup.

* Added regression coverage for `augment()` across two-state and three-state
  patterns, date, numeric, and difftime time columns, supplied `t_death`,
  expanded statuses, conversion behavior, and generated augmented time columns.

* Added regression coverage for `polish()` duplicate-removal and no-duplicate
  behavior.

# msmtools 2.0.5
***

### CI

* Added GitHub Actions for R CMD check across Windows, macOS, and Ubuntu, plus
  roxygen consistency and informational coverage workflows.

### CHANGES

* Replaced broad **data.table** namespace imports with explicit imports for the
  functions used by **msmtools**.

### DOCUMENTATION

* Added contributor notes describing the local test, documentation, coverage,
  and package-check workflow.

# msmtools 2.0.4
***

### CHANGES

* Raised the supported baseline to R 4.1 and current CRAN releases of the main
  runtime dependencies used by **msmtools**.

* Removed the unused development helper **usethis** from `Suggests`.

### DOCUMENTATION

* Migrated package documentation metadata to **roxygen2** 8.0.0 with markdown
  documentation enabled.

### TESTS

* Replaced deprecated `testthat::expect_is()` calls with
  `testthat::expect_s3_class()` for compatibility with **testthat** 3rd
  edition.

# msmtools 2.0.3
***

This is a CRAN resubmission after **msmtools** was archived because compatibility
issues with newer **data.table** releases were not corrected in time. No
user-facing behaviour has changed.

### Bug fixes

* Kept the compatibility fixes for current **data.table** releases by avoiding
  deprecated `substitute()` calls on the left-hand side of `:=` assignments.

* Retained the focused non-standard evaluation cleanup in `augment()` and
  `polish()` needed for CRAN checks, without introducing the broader refactor
  attempted on the historical `substitute_fix` branch.

### Documentation

* Updated release metadata for the CRAN resubmission.

# msmtools 2.0.2
***

This is a maintenance release addressing compatibility issues that caused the package to be
removed from CRAN. No user-facing behaviour has changed.

### Bug fixes

* Removed all uses of `substitute()` on the left-hand side of `:=` in **data.table** operations
  inside `augment()` and `polish()`. This pattern was deprecated by **data.table** and produced
  errors on current CRAN versions. Column assignment now uses the recommended `(var) :=` idiom.

* Replaced every remaining `eval(substitute(data$var))` and `eval(substitute(class(data$var)))`
  construct with direct `data[[var]]` and `class(data[[var]])` equivalents across `augment()` and
  `polish()`. The previous forms were fragile and harder to reason about.

* Replaced `class(...) == "type"` comparisons with `inherits()` throughout `augment()`, as
  required by current `R CMD CHECK`.

### Documentation

* Replaced the deprecated `@docType "package"` / `NULL` pattern in `msmtools.R` with the
  current `"_PACKAGE"` sentinel required by **roxygen2** >= 7.2.

# msmtools 2.0.1
***

This is a maintenance update. There are no major updates worth noting apart
from a few tweaks to the vignette, which was not rendered appropriately.

### Minor changes

* Fixed few issues in the vignette where plots were not rendered nicely.

# msmtools 2.0.0
***

**msmtools** sees several updates which come with potential breaking changes due to the dropping
of several arguments in some functions. The most relevant feature being shipped with v2.0.0 is
that both `survplot()` and `prevplot()` now support **ggplot2**. All in all, this justifies the 
jump in versions thus bringing **msmtools** to version 2.0.0.

### Breaking changes

* `survplot()` requires much less arguments now but at the same time is a bit less flexible. This 
is particularly reflected in plot customization which is now "self-imposed". I summarize what's different w.r.t. v1.3 below:
  - There are no more arguments related to plot rendering, like colors and linetype. The function always
  returns the same type of plot as a `gg` and `ggplot` object. 
  - Also, `survplot()` returns nothing but the rendered plot by default. The user can tell the function
  to return additional objects like the `fitted` data, the Kaplan-Meier data `km`, 
  or one of them with the argument `out`. 
  - Conversions are gone forever. `survplot()` always returns a `data.table` when `out` requires
  such object. 
  - There are no more controls over the device being used when rendering a plot. The function now
  just uses the default for simplicity and to avoid confusion due to different OSes. There is no
  possibility anymore to add lines (i.e. additional plots) to an already rendered plot. Just use
  `survplot()` twice with different input parameters and the combine the plots afterwards. The 
  underlying data structures for the plot are always returned and made available through a `gg/ggplot`
  object.
  - There are no more controls over what is printed at console. You will get few messages and that's it.

* similarly, `prevplot()` has been greatly improved. There are no more loops in the function and
everything is done by reference so it is very efficient. I briefly summarize what's different w.r.t.
v1.3 below:
  - There are no more arguments related to plot rendering. `prevplot()` always returns a `gg/ggplot`
  object. 
  - All arguments related to the grid or time steps are gone. Everything is managed consistently
  by extracting the correct model frame from the prevalence object computed with `prevalence.msm()`.
  - For loops are gone too since plots make use of `facet_wrap()` to automatically decide how many
  grobs to render. For now, there is no customization possible since I expect that for most cases 
  the number of states is limited in number anyway. There might be some flexibility added in future
  releases.
  - When `M = TRUE`, `prevplot()` uses **patchwork** to wrap the two plots in two different rows.
  The first row has the standard prevalence plot, the second row has the Deviance M.
  - There are no more controls over what is printed at console. You will get few messages and that's it.
  
### Major changes

* `survplot()` lets you specify the type of confidence intervals for the Kaplan-Meier in addition to
the already available types for the fitted curve. The argument is `ci_km`. 
* Specify what is the desired output through the argument `out` which takes a vector of characters.
* By default, `survplot()` always renders the plot and returns it.
* `prevplot()` drops plot layering in favor of direct **ggplot2** support.
* `prevplot()` has become faster due to smarter data extraction and binding.

### Minor changes

* Updated vignette.

# msmtools 1.3
***

### Major changes

* The new function `polish()` is introduced. This adds support in the preprocessing 
part of the analysis. `polish()` addresses the specific problem of different 
transitions occurring at the same exact time within a given subject. 
This is a case for which a multi-state model fails to estimate the probability 
associated with the two transitions. At the moment, the whole subject specified 
by `data_key` is deleted.

### Minor changes

* Global variables are now correctly declared on top of functions using 
`utils::globalVariables()`. This prevents the assignment to `NULL` in the preamble
of functions which decreases the elegance of the code.

* The printing of information is now way more simpler and is not based 
on `sink()` anymore. This has been done to be less intrusive into the OS 
when redirecting on console messages. Also, no more OS type check is done so that
the control with argument `verbose` is the most general possible. Warnings are
still controlled as usual, so that they get printed, if any, just after the 
function call.

### Other changes

* The vignette has been updated to include new features and it is now
in HTML format which provides a faster and lighter access.

* Improved the documentation's readability in the 'Value' section for 
`augment()` and `survplot()`.

* In the documentation of `augment()` now there is an explicit example which 
returns a `data.frame`.

* Windows builds of **msmtools** are now constantly checked through 
[Appveyor](https://www.appveyor.com).

* The author/maintainer e-mail has changed to match his new affiliation and now is
francesco.grossetti@unibocconi.it.

### Bug fixes

* After `augment()` has been run, results are now visible at the very
first call. This means that you can finally print on console the augmented dataset
right away.

* `pandoc` versions prior to 1.17 do not fully support spaces in file names and
caused a warning when compiling **msmtools** under Fedora using both `clang`
and `gcc`. Now all file names are without spaces. **msmtools** 1.3 has been built
using `pandoc` 1.19.2 and `pandoc-citeproc` 0.10.4.1

*** ***

# msmtools 1.2
***

### Major changes

* **msmtools** can now run with R 3.0.0 and above for retro compatibility reasons.

* `augment()` gains the new argument `check_NA` which allows the user to decide 
if the function should run some checks to find missing data in the following 
arguments: `data_key`, `n_events`, `pattern`, `t_start` and `t_end`. Default is 
`FALSE`. Missing data checks are always carried out on `more_status`.

* `augment()` gains the new argument `convert` which if set to `TRUE` 
efficiently converts the output to the old school `data.frame` class. 

* `survplot()` gains the new argument `return.all` which saves you some typing 
time when requesting both the data of the Kaplan-Meier and the fitted survival. 
Arguments `return.km` and `return.p` now are set to `NULL` by default instead 
of `FALSE`. 

* `survplot()` gains the new argument `convert` which if set to `TRUE` 
efficiently converts any object returned to the old school `data.frame` class.

### Changes in functions

* `augment()` now also accepts an object of class `data.frame` as input. 
If so, the function internally converts it to a `data.table`.

* `augment()` now accepts `t_augmented` without quotes too. Default name is 
still "augmented".

* `augment()` gets a whole new implementation which comes into play when 
`pattern` has only 2 values ('alive' and 'dead'). Now the procedure runs with 
computational time only slightly longer than the standard 3 values in `pattern`. 
This is due thanks to the fast joins method adopted.

* `augment()` now is much faster when defining the target size for the reshaping. 
This was a bottleneck which caused memory issues and wasted time. 

* General memory optimization in the function `augment()`. 
Now the function uses ~ 30% less memory.

### Other changes

* All the functions now have more detailed and better written helps.

* Some minor changes in the vignette to encapsulate new functionalities.

### Bug fixes

* In `augment()`, the sequential status is now correctly computed. 
There was a wrong call which blocked the object defined by `n_events`.

* In `augment()`, when `pattern` was detected with two unique values, 
inconsistent results were produced during the status flag assignment. 
This was due to a wrong rounding of the amount of augmenting factor for each unit.

***
***

# msmtools 1.1
***

### Changes in functions

* `augment()` now is way faster then in v1.0 thanks to a new implementation 
when defining patterns.

* `augment()` now uses the faster `uniqueN()` to extract the number of unique 
values in a vector.

### Bug fixes

* `augment()` now correctly positions new created variables.

### Other changes

* `augment()` in-line help now provides correct information on what it returns.

***

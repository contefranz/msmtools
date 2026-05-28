# **msmtools**: Augmented Data for Multi-State Models

[![lifecycle](https://lifecycle.r-lib.org/articles/figures/lifecycle-stable.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![R-CMD-check](https://github.com/contefranz/msmtools/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/contefranz/msmtools/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/contefranz/msmtools/branch/main/graph/badge.svg?token=wDcJP6mRRY)](https://app.codecov.io/gh/contefranz/msmtools)
[![release](https://img.shields.io/badge/dev.%20version-2.2.0-blue)](https://github.com/contefranz/msmtools)
[![CRAN Status Badge](https://www.r-pkg.org/badges/version/msmtools)](https://cran.r-project.org/package=msmtools)
[![license](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://en.wikipedia.org/wiki/GNU_General_Public_License)


**msmtools** restructures longitudinal observational data into augmented
transition data for multi-state models fitted with **msm**. It works for any
domain where subjects accumulate repeated observations with start and end times
and an optional terminal outcome — clinical follow-ups, customer journeys,
machine usage spells, employment histories, and so on. The package returns
transition-level rows, numeric state indicators, and diagnostic plots.

From version 2.0.4, **msmtools** targets a modern CRAN baseline: R 4.1 or newer
and current releases of **data.table**, **msm**, **survival**, **ggplot2**, and
**cli**. Since 2.1.3, **patchwork** is an optional dependency required only by
`prevplot(M = TRUE)`.

### Installation

```r
install.packages("msmtools")

# development version
remotes::install_github("contefranz/msmtools")
```

### Core Workflow

* `augment()` builds the augmented transition data used by multi-state models.
* `polish()` removes subjects with conflicting transitions at the same time.
* `survplot()` compares fitted and empirical survival curves from a fitted
  **msm** model.
* `prevplot()` compares observed and expected prevalences from a fitted
  **msm** model.


```r
library(msmtools)
library(data.table)

data(hosp)

hosp_augmented <- augment(
  data = copy(hosp),
  data_key = subj,
  n_events = adm_number,
  pattern = label_3,
  t_start = dateIN,
  t_end = dateOUT,
  t_cens = dateCENS
)

hosp_augmented[
  1:6,
  .(subj, adm_number, label_3, augmented, augmented_int, status, status_num)
]
```

`augment()` returns a `data.table`. Use `as.data.frame()` explicitly if a
downstream workflow requires a plain `data.frame`.

`pattern` describes the terminal outcome schema in the input data. `state`
describes the generated transition-state labels and must contain three labels:
the state at `t_start`, the state at `t_end`, and the absorbing state.

`augment()` and `polish()` use `copy = FALSE` by default to preserve the
memory-efficient **data.table** workflow. This means input objects can be
modified by reference. Set `copy = TRUE` when the original object must remain
unchanged.


#### Duplicate Transition Cleanup

`polish()` uses `augmented_int` as the duplicate-time column by default. Set
`time = NULL` for the same auto-detection behavior, or pass another time column
explicitly.

```r
hosp_clean <- polish(
  data = copy(hosp_augmented),
  data_key = subj,
  pattern = label_3
)
```

#### Diagnostic Plots

`survplot()` and `prevplot()` operate on fitted **msm** objects. See the vignette
for a compact end-to-end example that augments the bundled data, fits a small
model, and builds both diagnostic plots.

Both plotting functions print plots by default. Set `print_plot = FALSE` when
you need the returned plot object or data without rendering it.

```r
vignette("msmtools")
```


### Author

[Francesco Grossetti](https://accounting.unibocconi.eu/faculty/francesco-grossetti)

_Assistant Professor of Accounting Analytics and Data Science_<br>
Department of Accounting, Bocconi University<br>
Fellow at Bocconi Institute for Data Science and Analytics ([BIDSA](https://bidsa.unibocconi.eu/))<br>
Contact: francesco.grossetti@unibocconi.it

---

Bugs and issues can be reported at
[https://github.com/contefranz/msmtools/issues](https://github.com/contefranz/msmtools/issues).

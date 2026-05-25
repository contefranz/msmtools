# Building augmented data for multi-state models: the `msmtools` package

[![lifecycle](https://lifecycle.r-lib.org/articles/figures/lifecycle-stable.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![R-CMD-check](https://github.com/contefranz/msmtools/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/contefranz/msmtools/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/contefranz/msmtools/branch/main/graph/badge.svg?token=wDcJP6mRRY)](https://codecov.io/gh/contefranz/msmtools)
[![release](https://img.shields.io/badge/dev.%20version-2.0.8-blue)](https://github.com/contefranz/msmtools)
[![CRAN Status Badge](https://www.r-pkg.org/badges/version/msmtools)](https://cran.r-project.org/package=msmtools)
[![license](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://en.wikipedia.org/wiki/GNU_General_Public_License)

***

**msmtools** restructures longitudinal data into augmented transition data for
multi-state models fitted with **msm**. The package focuses on the common
workflow where each subject has repeated observations with exact start and end
times, and the analyst needs transition-level rows, numeric state indicators,
and diagnostic plots.

From version 2.0.4, **msmtools** targets a modern CRAN baseline: R 4.1 or newer
and current releases of **data.table**, **msm**, **survival**, **ggplot2**,
**patchwork**, and **scales**.

## Installation

```r
install.packages("msmtools")

# development version
remotes::install_github("contefranz/msmtools")
```

## Core Workflow

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
  t_cens = dateCENS,
  verbose = FALSE
)

hosp_augmented[
  1:6,
  .(subj, adm_number, label_3, augmented, augmented_int, status, status_num)
]
```

`augment()` returns a `data.table` by default. Set `convert = TRUE` to return a
plain `data.frame`.

## Functions

* `augment()` builds the augmented transition data used by multi-state models.
* `polish()` removes subjects with conflicting transitions at the same time.
* `survplot()` compares fitted and empirical survival curves from an `msm`
  model.
* `prevplot()` compares observed and expected prevalences from an `msm` model.

## Duplicate Transition Cleanup

```r
hosp_clean <- polish(
  data = copy(hosp_augmented),
  data_key = subj,
  pattern = label_3,
  verbose = FALSE
)
```

## Diagnostic Plots

`survplot()` and `prevplot()` operate on fitted **msm** objects. See the vignette
for a compact end-to-end example that augments the bundled data, fits a small
model, and builds both diagnostic plots.

```r
vignette("msmtools")
```

Bugs and issues can be reported at
[https://github.com/contefranz/msmtools/issues](https://github.com/contefranz/msmtools/issues).

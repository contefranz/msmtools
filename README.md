# Building augmented data for multi-state models: the `msmtools` package

[![lifecycle](https://lifecycle.r-lib.org/articles/figures/lifecycle-maturing.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![release](https://img.shields.io/badge/dev.%20version-2.0.1-blue)](https://github.com/contefranz/msmtools)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/msmtools)](https://cran.r-project.org/package=msmtools)

***

**msmtools** introduces a fast and general method for restructuring classical 
longitudinal datasets into *augmented* ones. The reason for this is to 
facilitate the modeling of longitudinal data under a multi-state framework 
using the **msm** package.

## Installation

``` r
# Install the released version from CRAN:
install.packages("msmtools")

# Install the development version from GitHub:
devtools::install_github("contefranz/msmtools")
```

## Overview

**msmtools** comes with 4 functions: 

* `augment()`: the main function of the package. This is the workhorse which 
takes care of the data reshaping. It is very efficient and fast so highly 
dimensional datasets can be processed with ease;

* `polish()`: it helps in find and remove those transition which occur at the 
same time but lead to different states within a given subject;

* `prevplot()`: this is a plotting function which mimics the usage of `msm()` 
function `plot.prevalence.msm()`, but with more things. Once you ran a 
multi-state model, use this function to plot a comparison between observed and 
expected prevalences;

* `survplot()`: the aims of this function are double. You can use `survplot()` 
as a plotting tool for comparing the empirical and the fitted survival curves. 
Or you can use it to build and get the datasets used for the plot. 
The function is based on **msm** `plot.survfit.msm()`, but does more things and 
it is considerably faster.

For more information about **msmtools**, please check out the vignette with 
`vignette( "msmtools" )`.

Bugs and issues can be reported at
[https://github.com/contefranz/msmtools/issues](https://github.com/contefranz/msmtools/issues).

## Breaking changes from version 2.0.0

**msmtools** has received a lot of improvements in the plotting functions. In particular, from
version 2.0.0 both `survplot()` and `prevplot()` support [**ggplot2**](https://ggplot2.tidyverse.org). 
This inevitably introduces
several breaking changes. Overall, both functions have been greatly simplified, but I encourage
to go over each function's documentation and the vignette to get a correct understanding on how they
work.

***

# Building augmented data for multi-state models: the `msmtools` package

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/msmtools)](https://cran.r-project.org/package=msmtools)
Linux/Unix: [![Build Status](https://travis-ci.org/contefranz/msmtools.svg?branch=master)](https://travis-ci.org/contefranz/msmtools)
Windows: [![Build status](https://ci.appveyor.com/api/projects/status/9d38hi27w1f7ccko/branch/master?svg=true)](https://ci.appveyor.com/project/contefranz/msmtools/branch/master)

***

**msmtools** introduces a fast and general method for restructuring classical 
longitudinal datasets into *augmented* ones. The reason for this is to 
facilitate the modeling of longitudinal data under a multi-state framework 
using the **msm** package.

## Installation

``` r
# Install the released version from CRAN:
install.packages( "msmtools" )

# Install the development version from GitHub:
devtools::install_github( "contefranz/msmtools" )
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
[www.github.com/contefranz/msmtools/issues](www.github.com/contefranz/msmtools/issues).

***

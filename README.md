# Building augmented data for multi-state models: the `msmtools` package

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/msmtools)](http://cran.r-project.org/package=msmtools)
[![Build Status](https://travis-ci.org/contefranz/msmtools.svg?branch=dev)](https://travis-ci.org/contefranz/msmtools) 
[![GitHub issues](https://img.shields.io/github/issues/contefranz/msmtools.svg)](https://github.com/contefranz/msmtools/issues)

`msmtools` introduces a fast and general method for restructuring classical longitudinal datasets
into *augmented* ones. The reason for this is to facilitate the modeling of longitudinal data 
under a multi-state framework using the `msm` package.

`msmtools` comes with 3 functions: 

* `augment()`: the main function of the package. This is the workhorse which takes care of the 
data reshaping. It is very efficient and fast so highly dimensional datasets can be processed
with ease;

* `prevplot()`: this is a plotting function which mimics the usage of `msm` function 
`plot.prevalence.msm()`, but with more things. Once you ran a multi-state model, use this function
to plot a comparison between observed and expected prevalences;

* `survplot()`: the aims of this function are double. You can use `survplot()` as a plotting tool
for comparing the empirical and the fitted survival curves. Or you can use it to build and get 
the datasets used for the plot. The function is based on `msm` `plot.survfit.msm()` but does
more things and it is a lot faster.

For more information about `msmtools`, please check out the vignette by running 
`vignette("msmtools")`.

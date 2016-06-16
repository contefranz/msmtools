# msmtools 1.2

### Breaking changes

* `msmtools` can now run with R 3.0.0 and above for retro compatibility reasons.

* `augment()` gains the new argument `check_NA` which allows the user to decide if the function
should run some checks to find missing data in the following arguments: `data_key`, `n_events`, 
`pattern`, `t_start` and `t_end`. Default is `FALSE`. Missing data checks are always carried out on
`more_status`.

* `augment()` gains the new argument `convert` which if set to `TRUE` efficiently converts the output 
to the old school `data.frame` class. 

* `survplot()` gains the new argument `return.all` which saves you some typing time when requesting both 
the data of the Kaplan-Meier and the fitter survival. Arguments `return.km` and `return.p` now are
set to `NULL` by default instead of `FALSE`. 

* `survplot()` gains the new argument `convert` which if set to `TRUE` efficiently converts
any object returned to the old school `data.frame` class.

### Changes in functions

* `augment()` gets a whole new implementation which comes into play when `pattern` has only
2 values ('alive' and 'dead'). Now the procedure runs with computational time only slightly longer
than the standard 3 values in `pattern`. This is due thanks to the fast joins method adopted.

* `augment()` now is much faster when defining the target size for the reshaping. This was a 
bottleneck which caused memory issues and wasted time. 

* `survplot()` and `prevplot()` have now a more detailed and better written help.

* General memory optimization in the function `augment()`.

### Other changes

* Some minor changes in the vignette.

### Bug fixes

* In `augment()`, the sequential status is now correctly computed. There was a wrong call which
blocked the object defined by `n_events`.

* In `augment()`, when `pattern` was detected with two unique values, inconsistent results were
produced during the status flag assignment. This was due to a wrong rounding of the amount of 
augmenting factor for each unit.

---

# msmtools 1.1

### Changes in functions

* `augment()` now is way faster then in v1.0 thanks to a new implementation when defining patterns

* `augment()` now uses the faster `uniqueN()` to extract the number of unique values in a vector

### Bug fixes

* `augment()` now correctly repositions new created variables

### Other changes

* `augment()` in-line help now provides correct information on what it returns

---

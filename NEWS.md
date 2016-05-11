# msmtools 1.2

### Breaking changes

* `augment()` gains the brand new argument `check_NA` which allows the user to decide if the function
should run some checks to find missing data in the following arguments: `data_key`, `n_events`, 
`pattern`, `t_start` and `t_end`. Default is `FALSE`.

### Changes in functions

* `augment()` now is much faster when defining the target size for the reshaping. This was a 
bottlneck which caused memory issues and wasted time. 

### Bug fixes

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

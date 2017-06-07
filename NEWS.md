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
`utils::globalVariables()`. This prevents the assignment to `NULL` in the preable
of functions which decreases the elegance of the code.

* The printing of information is now way more simplier and is not based 
on `sink()` anymore. This has been done to be less intrusive into the OS 
when redirecting on console messages. Also, no more OS type check is done so that
the control with argument `verbose` is the most general possible. Warnings are
still controlled as usual, so that they get printed, if any, just after the 
function call.

### Other changes

* The vignette has been updated to include new functionalities and it is now
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

* `pandoc` versions prior 1.17 does not fully support spaces in file names and 
caused a warning when compiling `msmtools` under Fedora using both `clang` 
ang `gcc`. Now all file names are without spaces. `msmtools` 1.3 has been built
using `pandoc` 1.19.2 and `pandoc-citeproc` 0.10.4.1

*** ***

# msmtools 1.2
***

### Major changes

* `msmtools` can now run with R 3.0.0 and above for retro compatibility reasons.

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

* `augment()` now correctly repositions new created variables.

### Other changes

* `augment()` in-line help now provides correct information on what it returns.

***

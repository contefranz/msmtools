# msmtools 2.0.1
***

This is a maintenance update. There are no major updates worth of notice besides few tweaks in 
the vignette which was not rendered appropriately.

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

* `pandoc` versions prior 1.17 does not fully support spaces in file names and 
caused a warning when compiling `msmtools` under Fedora using both `clang` 
and `gcc`. Now all file names are without spaces. `msmtools` 1.3 has been built
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

* `augment()` now correctly positions new created variables.

### Other changes

* `augment()` in-line help now provides correct information on what it returns.

***

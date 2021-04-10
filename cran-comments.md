# msmtools 2.0.1

### Release summary

This is a maintenance update. There are no major updates worth of notice besides few tweaks in 
the vignette which was not rendered appropriately.

### Package development

* macOS 10.15.7 with R 4.0.4

### R CMD build

* local MacOS 10.15.7
* win build created with `devtools::check_win_release()`
* multiplatform builds created with `devtools::check_rhub()`

### R CMD check results

* Everything looks amazing so far.

***

# msmtools 2.0.0

### Release summary

This marks a major redesign in how the package manages plots. It now uses **ggplot2**.
Also, most of the cumbersome arguments related to devices and plot layering have been improved and
substantially removed. The above changes are enough to declare that **msmtools** has now
reached full maturity and thus justify the jump to version 2.0.0.

### Package development

* macOS 10.15.7 with R 4.0.4

### R CMD build

* local MacOS
* win build through devtools::build_win()

### R CMD check results

* There were no ERRORs nor WARNINGs nor NOTEs. 

* `devtools::build_win()` found possible mispelling in the DESCRIPTION file. 
Though the file is correct.

* `devtools::build_win()` found non canonical URL in the vignette and in the 
README. The former is included in the bibliography and the latter is due to
a Github link.

***

# msmtools 1.3

### Release summary

This is version 1.3 of **msmtools**

### Package development

* macOS 10.12.5 with R 3.4.0

### R CMD build

* local macOS
* win build through devtools::build_win()

### R CMD check results

* There were no ERRORs nor WARNINGs nor NOTEs. 

* `devtools::build_win()` found possible mispelling in the DESCRIPTION file. 
Though the file is correct.

* `devtools::build_win()` found non canonical URL in the vignette and in the 
README. The former is included in the bibliography and the latter is due to
a Github link.


***
# msmtools 1.2

### Release summary

This is version 1.2 of **msmtools**

### Package development

* OS X 10.11.5 with R 3.3.0

### R CMD build

* local OS X
* win build through devtools::build_win()

### R CMD check results

There were no ERRORs or WARNINGs. 

There was 1 NOTE:

* Unknown, possibly mis-spelled, fields in DESCRIPTION: 'News'
This is due to the presence of a GitHub link which points at the file NEWS.md.

***
# msmtools 1.1

### Release summary

This is version 1.1 of **msmtools**

### Package development

* OS X 10.11.4 with R 3.2.4

### R CMD build

* local OS X
* win build through devtools::build_win()

### R CMD check results

There were no ERRORs or WARNINGs. 

There was 1 NOTE:

* checking CRAN incoming feasibility ... NOTE  
Maintainer: 'Francesco Grossetti <francesco.grossetti@polimi.it>'

New submission


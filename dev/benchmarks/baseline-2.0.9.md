# msmtools 2.0.9 Performance Baseline

This is a local developer baseline, not a public performance guarantee.
The script is excluded from package builds and must be run manually.

- Generated: 2026-05-25 09:51:57 CEST
- R version: R version 4.5.1 (2025-06-13)
- Platform: aarch64-apple-darwin24.4.0
- data.table: 1.18.4
- Peak memory: not recorded; install peakRAM for memory baselines

| Size | Subjects | Rows | Time type | Operation | Runtime (s) | Peak MiB | Checks | Side effects |
| --- | ---: | ---: | --- | --- | ---: | ---: | --- | --- |
| small |   100 |   400 | Date | augment | 0.017 | not recorded | convert=yes; n_events=yes; columns=yes | augment_adds_n_events=yes; augment_key=yes; polish_names=yes; polish_key=yes |
| small |   100 |   400 | Date | polish | 0.003 | not recorded | convert=yes; n_events=yes; columns=yes | augment_adds_n_events=yes; augment_key=yes; polish_names=yes; polish_key=yes |
| small |   100 |   400 | numeric | augment | 0.014 | not recorded | convert=yes; n_events=yes; columns=yes | augment_adds_n_events=yes; augment_key=yes; polish_names=yes; polish_key=yes |
| small |   100 |   400 | numeric | polish | 0.002 | not recorded | convert=yes; n_events=yes; columns=yes | augment_adds_n_events=yes; augment_key=yes; polish_names=yes; polish_key=yes |
| small |   100 |   400 | difftime | augment | 0.014 | not recorded | convert=yes; n_events=yes; columns=yes | augment_adds_n_events=yes; augment_key=yes; polish_names=yes; polish_key=yes |
| small |   100 |   400 | difftime | polish | 0.003 | not recorded | convert=yes; n_events=yes; columns=yes | augment_adds_n_events=yes; augment_key=yes; polish_names=yes; polish_key=yes |
| medium |  1000 |  4000 | Date | augment | 0.029 | not recorded | convert=yes; n_events=yes; columns=yes | augment_adds_n_events=yes; augment_key=yes; polish_names=yes; polish_key=yes |
| medium |  1000 |  4000 | Date | polish | 0.006 | not recorded | convert=yes; n_events=yes; columns=yes | augment_adds_n_events=yes; augment_key=yes; polish_names=yes; polish_key=yes |
| medium |  1000 |  4000 | numeric | augment | 0.032 | not recorded | convert=yes; n_events=yes; columns=yes | augment_adds_n_events=yes; augment_key=yes; polish_names=yes; polish_key=yes |
| medium |  1000 |  4000 | numeric | polish | 0.006 | not recorded | convert=yes; n_events=yes; columns=yes | augment_adds_n_events=yes; augment_key=yes; polish_names=yes; polish_key=yes |
| medium |  1000 |  4000 | difftime | augment | 0.028 | not recorded | convert=yes; n_events=yes; columns=yes | augment_adds_n_events=yes; augment_key=yes; polish_names=yes; polish_key=yes |
| medium |  1000 |  4000 | difftime | polish | 0.006 | not recorded | convert=yes; n_events=yes; columns=yes | augment_adds_n_events=yes; augment_key=yes; polish_names=yes; polish_key=yes |
| large | 10000 | 40000 | Date | augment | 0.136 | not recorded | convert=yes; n_events=yes; columns=yes | augment_adds_n_events=yes; augment_key=yes; polish_names=yes; polish_key=yes |
| large | 10000 | 40000 | Date | polish | 0.021 | not recorded | convert=yes; n_events=yes; columns=yes | augment_adds_n_events=yes; augment_key=yes; polish_names=yes; polish_key=yes |
| large | 10000 | 40000 | numeric | augment | 0.137 | not recorded | convert=yes; n_events=yes; columns=yes | augment_adds_n_events=yes; augment_key=yes; polish_names=yes; polish_key=yes |
| large | 10000 | 40000 | numeric | polish | 0.021 | not recorded | convert=yes; n_events=yes; columns=yes | augment_adds_n_events=yes; augment_key=yes; polish_names=yes; polish_key=yes |
| large | 10000 | 40000 | difftime | augment | 0.135 | not recorded | convert=yes; n_events=yes; columns=yes | augment_adds_n_events=yes; augment_key=yes; polish_names=yes; polish_key=yes |
| large | 10000 | 40000 | difftime | polish | 0.021 | not recorded | convert=yes; n_events=yes; columns=yes | augment_adds_n_events=yes; augment_key=yes; polish_names=yes; polish_key=yes |

## Notes

* `augment_adds_n_events` records the current by-reference addition of `n_events` when it is omitted.
* `augment_key` and `polish_key` record whether the input object's key changes by reference.
* `polish_names` records whether `polish()` restores temporary columns on the input object.

## Session

```
R version 4.5.1 (2025-06-13)
Platform: aarch64-apple-darwin24.4.0
Running under: macOS Tahoe 26.5

Matrix products: default
BLAS:   /opt/homebrew/Cellar/openblas/0.3.30/lib/libopenblasp-r0.3.30.dylib 
LAPACK: /opt/homebrew/Cellar/r/4.5.1/lib/R/lib/libRlapack.dylib;  LAPACK version 3.12.1

locale:
[1] C.UTF-8/C.UTF-8/C.UTF-8/C/C.UTF-8/C.UTF-8

time zone: Europe/Rome
tzcode source: internal

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
[1] data.table_1.18.4 msmtools_2.0.9    testthat_3.3.2   

loaded via a namespace (and not attached):
 [1] patchwork_1.3.2    vctrs_0.7.3        cli_3.6.6          rlang_1.2.0       
 [5] pkgload_1.5.2      generics_0.1.4     S7_0.2.2           glue_1.8.1        
 [9] msm_1.8.2          rprojroot_2.1.1    pkgbuild_1.4.8     brio_1.1.5        
[13] scales_1.4.0       grid_4.5.1         expm_1.0-0         tibble_3.3.0      
[17] mvtnorm_1.3-3      lifecycle_1.0.5    compiler_4.5.1     dplyr_1.1.4       
[21] RColorBrewer_1.1-3 pkgconfig_2.0.3    farver_2.1.2       lattice_0.22-7    
[25] R6_2.6.1           tidyselect_1.2.1   pillar_1.11.1      splines_4.5.1     
[29] magrittr_2.0.5     Matrix_1.7-4       withr_3.0.2        tools_4.5.1       
[33] gtable_0.3.6       survival_3.8-6     ggplot2_4.0.3      desc_1.4.3        
```

# Contributing to msmtools

## Local Workflow

Run the package tests before pushing:

```r
devtools::test()
```

Regenerate documentation after changing roxygen comments:

```r
roxygen2::roxygenise()
```

Run local coverage with the same test scope used by CI:

```r
cov <- covr::package_coverage(type = "tests", quiet = FALSE)
covr::percent_coverage(cov)
```

For a local package check:

```sh
RSTUDIO_PANDOC=/Applications/quarto/bin/tools/aarch64 R CMD build .
RSTUDIO_PANDOC=/Applications/quarto/bin/tools/aarch64 R CMD check --no-manual --as-cran msmtools_*.tar.gz
```

The explicit `RSTUDIO_PANDOC` path is useful on local macOS setups where Quarto
bundles pandoc but R cannot discover it automatically.

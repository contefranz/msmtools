# Performance Baselines

These scripts are for local development only. The `dev/` directory is excluded
from package builds, so these benchmarks do not run during installation,
examples, vignettes, tests, CI, or `R CMD check`.

Run the baseline manually from the repository root:

```sh
Rscript dev/benchmarks/run-baseline.R
```

The baseline records approximate runtime, optional peak memory, output
equivalence checks, and current by-reference side effects for `augment()` and
`polish()`. Results are machine-dependent and should be treated as refactor
guidance rather than public performance claims.

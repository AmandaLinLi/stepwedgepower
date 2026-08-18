# Development validation for version 0.4.1

Version 0.4.1 adds a batch-aware interface grounded in the standard terminology
for classic and batched stepped-wedge designs. Static checks in the build
environment verified balanced R delimiters, DESCRIPTION continuation structure,
Rd braces, NAMESPACE/function/documentation consistency, preservation of all
0.4.0 exports, and the exact SWD/BSWD resource totals (40 observed
clinic-periods in each design; 10 structurally unobserved clinic-periods in the
BSWD).

The new API supports calendar time, shared time on trial, and separate
batch-specific time. The separate model uses a categorical batch-by-local-time
factor. For the exact SWD/BSWD schedules in the vignette, independent model-
matrix checks found full rank under every supported no-interaction time model:

| Design | Calendar time | Time on trial | Separate time |
|---|---:|---:|---:|
| SWD | 6 / 6 | 6 / 6 | 6 / 6 |
| BSWD | 7 / 7 | 6 / 6 | 10 / 10 |

All 0.3.0 cumulative-intervention functions and all 0.4.0 component functions
remain available. No existing exported function was removed. The new batched
functions default to a no-interaction model because the common cumulative
Control/A/A+B schedule contains no B-only state; four-state designs can request
the interaction explicitly.

R and lme4 were not installed in the build environment. Native GLMM execution,
testthat, vignette rendering, roxygen regeneration, and R CMD check therefore
must be run locally before release:

```r
devtools::document()
devtools::test()
devtools::check()
urlchecker::url_check()
devtools::check_win_devel()
devtools::check_mac_release()
```

Build the final CRAN archive locally with `devtools::build()`.


## Benchmark module added in 0.4.1

The package now includes a non-exported benchmark suite under
`inst/benchmarks/` and `vignettes/benchmark-validation.Rmd`. Fast base-R tests
verify the Figure 3 matrix-inversion values, terminal-delay invariance, Figure
5 closed-form curves, independent WLS matrix-inversion values, and the
theoretical crossing point. Table 2 and Table 3 external-software calls are
optional and are not executed during standard package checks unless explicitly
requested.

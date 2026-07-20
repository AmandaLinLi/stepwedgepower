# Development validation for version 0.4.0

The component engine was subjected to the following development checks in the
build environment:

- all R source files were parsed with an R grammar;
- all Rd files were parsed with `tools::parse_Rd()`;
- `DESCRIPTION` was read with `read.dcf()`;
- deterministic tests covered state coding, withdrawal, wash-in, carryover,
  restart rules, interaction weights, conversion from the 0.3.0 cumulative
  representation, fixed-effect rank, and standard-contrast estimability;
- randomized property tests covered 100 arbitrary four-state schedules and
  verified simulation dimensions, state/component consistency, effect-weight
  bounds, interaction rules, sample sizes, probabilities, outcomes, and audit
  construction.

A native `R CMD check` with `lme4` was not available in the build environment.
Before release, run:

```r
devtools::document()
devtools::test()
devtools::check()
urlchecker::url_check()
devtools::check_win_devel()
devtools::check_mac_release()
```

Then build the CRAN archive with `devtools::build()`.

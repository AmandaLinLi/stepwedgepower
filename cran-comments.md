## Test environments

* local macOS installation: pending final maintainer run
* GitHub Actions: R release on macOS and Windows; R devel, release, and oldrel on Linux
* win-builder devel: pending final maintainer run
* macOS builder: pending final maintainer run

## R CMD check results

Final submission requires 0 errors, 0 warnings, and 0 notes on the source archive built locally with `devtools::build()`.

## Changes in version 0.4.1

This is a substantial update from CRAN version 0.1.3. It preserves the existing 0.1.x interfaces and adds general stepped-wedge design objects, component-based and batched designs, structurally unobserved cluster-periods, three time parameterizations, simulation diagnostics, and a non-exported benchmark-validation module.

## Notes

Optional comparisons with `swCRTdesign` and `swdpwr` are disabled during routine examples and vignette builds. They are available through the installed benchmark suite when explicitly requested.
The words flagged by the incoming spell check are author surnames (Bowden and Kasza) and established statistical terms (estimability and nonconvergence).

<!-- README.md is generated from README.Rmd. Please edit that file -->

# HydroSurvDOYTEMP

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

<!-- lastcommit: start -->

[![Last
Commit](https://img.shields.io/github/last-commit/caitobrien/HydroSurvSizePred)](https://github.com/caitobrien/HydroSurvSizePred/commits/main)
<!-- lastcommit: end -->

**HydroSurvDOYTEMP** an interactive visualization tool based on the
models adapted from Scheuerell et al. (2009) and Gosselin et al. (2018).

The goal of **HydroSurvDOYTEMP** is to showcase predicted survival for
Chinook salmon, *Oncorhynchus tshawytscha*, and Steelhead, *Oncorhynchus
mykiss*, influenced by seasonal changes experienced during downstream
migration through the Federal Columbia River Power System Hydrosystem
(FCRPS), Pacific Northwest, USA.

*Please note that this shiny app is currently under development*

## Installation

You can install the development version of HydroSurvDOYTEMP from
[GitHub](https://github.com/) with:

``` r
# Install the developmental version of the HydroSurvSizePred package from GitHub
devtools::install_github("caitobrien/HydroSurvDOYTEMP")

# Load the package
library(HydroSurvDOYTEMP)

# Run the app
run_app()
```

**HydroSurvDOYTEMP** is currently in development and changes are
continuously being made. If you have already imported to R studio,
please rerun `install_github("caitobrien/HydroSurvDOYTEMP")` to see
latest changes in the developmental version. If no changes have been
made since last import, a warning will appear:
`Skipping install of 'HydroSurvSizePred' from a github remote, the SHA1 (fdd71350) has not changed since last install. Use 'force = TRUE' to force installation`
and you have the latest developmental version imported.

If you are interested in the files that support the development version,
please see: <https://github.com/caitobrien/HydroSurvDOYTEMP> for files
necessary to run.

The app structure follows a Golem framework described in [Engineering
Production-Grade Shiny
Apps](https://engineering-shiny.org/setting-up-for-success.html) by
Colin Fay, Sébastien Rochette, Vincent Guyader and Cervan Girard.

## Contact

This app is being developed by Caitlin O’Brien, Research Scientist,
Columbia Basin Research, SAFS, University of Washington. Please reach
out with questions/concerns via <csobrien@uw.edu>.

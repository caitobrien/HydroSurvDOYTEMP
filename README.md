
<!-- README.md is generated from README.Rmd. Please edit that file -->

# [HydroSurvDOYTEMP](https://csobrien.shinyapps.io/hydrosurvdoytemp/)
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

**Option 1:** You can install the development version of
*HydroSurvDOYTEMP* from [GitHub](https://github.com/) with:

``` r
# Install the developmental version of the HydroSurvSizePred package from GitHub
devtools::install_github("caitobrien/HydroSurvDOYTEMP")

# Load the package
library(HydroSurvDOYTEMP)

# Run the app
run_app()

#if needed, detach the package workspace and repeat above lines of code
detach("package:HydroSurvDOYTEMP", unload=TRUE)
```

*HydroSurvDOYTEMP* is currently in development and changes are
continuously being made. If you have already imported to R studio,
please rerun `install_github("caitobrien/HydroSurvDOYTEMP")` to see
latest changes in the developmental version. If no changes have been
made since last import, a warning will appear:
`Skipping install of 'HydroSurvDOYTEMP' from a github remote, the SHAX (XXXXXX) has not changed since last install. Use 'force = TRUE' to force installation`
and you have the latest developmental version imported. Use `detach()`
if not working properly and reinstall.

This option will run the Shiny App within you RStudio environment but
will not download the background files necessary to run.

**Option 2:** If you are interested in the files that support the
development version, please see:
<https://github.com/caitobrien/HydroSurvDOYTEMP> to clone the
repository. Alternatively, within Rstudio, use the following steps:

1.  Start a new project with
    `File > New Project > Version Control > Git`

2.  In the repository URL field, paste
    `https://github.com/caitobrien/HydroSurvDOYTEMP.git`

3.  Once project is created, and the repository is cloned, you can run
    the app within R environment by going to the folder:
    `dev > run_dev.R` and loading lines of code, with the final
    `run_app()` to launch the app.

*Possible download error with GitHubs security settings:*

If you receive an error associated with github_PAT credentials, you may
need to setup a GitHub Personal Access Token (PAT) for authentication
when you try to install a package from GitHub using
`devtools::install_github()`. Here are the steps to resolve this issue:

1.  **Generate a new GitHub PAT**: Go to your GitHub account settings,
    then to Developer settings -\> Personal access tokens -\> Generate
    new token (classic or fine-grained). Set expiration date and save
    token. Token will disappear if not saved.

2.  **Set the new PAT in your R environment**: You can use the `usethis`
    package to set the PAT in your R environment. Here’s how:

``` r
# Install the usethis package if you haven't already
install.packages("usethis")

# Use usethis to set the GITHUB_PAT environment variable
usethis::edit_r_environ()

# This will open your .Renviron file in a text editor. Add the following line to the file, replacing "your_new_pat" with your actual PAT:
GITHUB_PAT=your_new_pat

# Save and close the file. Then, restart your R session to make sure the new environment variable takes effect.
```

3.  **Try installing the package again**: Now, you should be able to
    install the package from GitHub without encountering the
    authentication error. Follow steps in Option 1.

The app structure follows a Golem framework described in [Engineering
Production-Grade Shiny
Apps](https://engineering-shiny.org/setting-up-for-success.html) by
Colin Fay, Sébastien Rochette, Vincent Guyader and Cervan Girard.

## Contact

This app is being developed by Caitlin O’Brien, Research Scientist,
Columbia Basin Research, SAFS, University of Washington. Please reach
out with questions/concerns via <csobrien@uw.edu>.

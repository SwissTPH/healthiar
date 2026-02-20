# healthiar

[![CRAN
downloads](https://cranlogs.r-pkg.org/badges/grand-total/healthiar)](https://cran.r-project.org/package=healthiar)

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.18220360.svg)](https://doi.org/10.5281/zenodo.18220360)

## Introduction

*healthiar* is an R package to quantify and monetize health impacts
attributable to exposure (e.g. air pollution, noise…) in a study area.
Using *healthiar*, you can …

- Quantify health impacts choosing among multiple input data formats and
  calculation pathways:

  - Exposure data as single value or as distribution

  - Relative risk or absolute risk

  - Fixed-shape or user-defined exposure-response functions

  - Single or age-specific baseline health data (life table approach)

- Iterate across geographical units

- Compare scenarios

- Include and summarize uncertainties

- Monetize health impacts or perform cost-benefit analyses adjusting for
  inflation and discounting

- Consider social inequalities in the assessment and stratify the
  results

## Getting started

You have different materials to learn about the R package `healthiar`.

### Cheat sheet

Have a quick and visual look at the [cheat
sheet](https://swisstph.github.io/healthiar/articles/cheatsheet.html)
[![Package
Cheatsheet](reference/figures/cheatsheet_healthiar_1st_page.png)](https://swisstph.github.io/healthiar/articles/cheatsheet.html)

### Vignette

Read the vignette (~ package manual) *intro_to_healthiar*, which you you
can access

1.  on the [package
    website](https://swisstph.github.io/healthiar/articles/intro_to_healthiar.html)
    (recommended)

2.  in R Studio: Click on the *Packages* tab in RStudio, scroll down to
    the *healthiar* package and clicking on the hyperlinks *healthiar*
    \> *User guides, package vignettes and other documentation*

3.  in the web browser: Run `browseVignettes("healthiar")` in the R
    console and the page will open up in your browser

### Function documentation

See the function help pages for information about specific functions. In
RStudio, you can access the function documentation of e.g. the function
`attribute_health` by

1.  going to the [reference page of the package
    website](https://swisstph.github.io/healthiar/reference/index.html)

2.  running
    [`?attribute_health`](https://swisstph.github.io/healthiar/reference/attribute_health.md)
    in RStudio (with `healthiar` loaded)

3.  going to the `Packages tab` and then clicking on `healthiar`

### Video

Watch a 45 minutes video from an online international workshop (30
September 2025), which can be found
[here](https://team.swisstph.ch/s/aN_wN5MUTAS3bwEkWvtvaQ). The slides of
the presentation can be found
[here](https://github.com/SwissTPH/healthiar/tree/master/varia/Workshops_and_demos/workshop).

## Installation

We recommend to frequently install the newest *healthiar* version.
Please note that **`healthiar` requires R version 4.3.0 or higher**.
There are two options to install *healthiar*:

1.  **From CRAN**: Click on the *Packages* tab in RStudio and on the
    *Install* button. Leave the *Install from:* option set to
    *Reporsitory (CRAN)* and then search and select *healthiar* and
    finally click on *Install*, keeping *Install dependencies*
    activated.

2.  **From Github (most recent version)**: Run the following commands
    below in RStudio to install *healthiar*:

- `install.packages(c("knitr", "rmarkdown"))`
- `remotes::install_github(repo = "SwissTPH/healthiar", build_vignettes = TRUE)`
- Note: install or update all package dependencies (= other packages
  that are needed for *healthiar*) if you get asked to do so

**After installation**, do not forget to load the package by running the
call [`library(healthiar)`](https://swisstph.github.io/healthiar/).

## Citation

We love that you use *healthiar*. In that case, please do not forget to
cite *healthiar* in your work. Three options to get there:

1.  On the [healthiar package
    website](https://swisstph.github.io/healthiar/authors.html#citation)

2.  See
    [CITATION.R](https://github.com/SwissTPH/healthiar/blob/master/inst/CITATION)

3.  In your R console, enter *citation(“healthiar”)*.

In options b) and c), you always see the updated citation. In option a),
you see citation of the *healthiar* version that you have installed
locally, which might be outdated.

## Disclamer and licence

By using *healthiar*, you confirm that you agree with the following
disclaimer and terms of the licence:

1.  Disclaimer: The R package *healthiar* is work in progress and the
    developers are not liable for the results.

2.  License: Available
    [here](https://github.com/SwissTPH/healthiar/blob/master/LICENSE.md).

## Feedback

Feel free provide feedback via [GitHub
issues](https://github.com/SwissTPH/healthiar/issues)

## Presenting *healthiar*

If you would like us to present *healthiar* at a conference, lecture or
training, please, contact us: <alberto.castrofernandez@swisstph.ch> and
<axel.luyten@swisstph.ch>

## Acknowledgements

*healthiar* was been developed under the framework of EU project
BEST-COST. BEST-COST is funded by the European Union’s Horizon Europe
programme under Grant Agreement No.101095408.

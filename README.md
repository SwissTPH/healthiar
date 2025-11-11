# INSTALL THE *healthiar* R PACKAGE FROM GITHUB
## Installation from CRAN: 
Click on the *Packages* tab in RStudio and in *Install*. 
From CRAN, search *healthiar* and click on *Install* keeping *Install dependencies* activated.

Run the following commands below in RStudio to install *healthiar*
1) `install.packages(c("remotes", "knitr", "rmarkdown"))`
2) `remotes::install_github(repo = "SwissTPH/healthiar", build_vignettes = TRUE)`
3) If you get asked to install or update any dependencies (= other packages that are needed for *healthiar*) please install or update all of them.

We recommend to frequently install the newest *healthiar* version by running command 2) from above.
(If you had previously installed *healthiar* while the repository was still private, you must remove your personal access token (PAT) before installing the package again.)
## Load the package
Do not forget to load the package after the installation entering in your R console: `library(healthiar)`.

## Requirement 
R version 4.3.0 or higher

## Note 
If you had previously installed *healthiar* while the repository was still private, you must remove your personal access token (PAT) before installing the package again.

# GET STARTED WITH *healthiar*
The vignette *intro_to_healthiar* (i.e. documentation on how to use the healthiar package) will get you started with the *healthiar* R package.
1) Open it in RStudio: go to the *Packages* tab in RStudio, scroll down to the *healthiar* package and clicking on the hyperlinks *healthiar* > *User guides, package vignettes and other documentation* > *healthiar::intro_to_healthiar*
2) Open it in browser: run `browseVignettes("healthiar")` in the console and click on the *HTML* hyperlink on the page that opens up in your browser

A 45' minutes introduction to the package can be found here: https://team.swisstph.ch/s/aN_wN5MUTAS3bwEkWvtvaQ

The slides of the presentation can be found here: https://github.com/SwissTPH/healthiar/tree/master/varia/Workshops_and_demos/workshop

# FEEDBACK
Feedback is very welcome - please provide it via GitHub issues: https://github.com/SwissTPH/healthiar/issues

# TRAININGS
Would you like to be introduced to *healthiar*? In case you would like us to introduce the package at a conference or another event please get in touch: alberto.castrofernandez@swisstph.ch and axel.luyten@swisstph.ch

# DISCLAMER AND LICENCE
By using *healthiar*, you confirm that you agree with the following disclaimer and terms of licence:
- Disclaimer: The R package *healthiar* is work in progress and the developers are not liable for the results. 
- Terms of license: Available [here](https://github.com/SwissTPH/healthiar/blob/master/LICENSE.md).


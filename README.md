# INSTALL THE *healthiar* R PACKAGE FROM GITHUB
(Requirement: R version 4.3.0 or higher)

Run the following commands below in RStudio to install *healthiar*
1) `install.packages(c("remotes", "knitr", "rmarkdown"))`
2) `remotes::install_github(repo = "SwissTPH/healthiar", build_vignettes = TRUE)`
3) If you get asked to install or update any dependencies (= other packages that are needed for *healthiar*) please install or update all of them.
4) `library(healthiar)`

## Load the package
Do not forget to load the package after the installation entering in your R console: `library(healthiar)`.

## Requirement 
R version 4.3.0 or higher. 

If you had previously installed *healthiar* while the repository was still private, you must remove your personal access token (PAT) before installing the package again.

# GET STARTED WITH *healthiar*
A 45' minutes introduction to the package can be found here: https://team.swisstph.ch/s/aN_wN5MUTAS3bwEkWvtvaQ

The slides of the presentation can be found here: https://github.com/SwissTPH/healthiar/tree/master/varia/Workshops_and_demos/workshop

The vignette *intro_to_healthiar* (i.e. documentation on how to use the healthiar package) will get you started with the *healthiar* R package.
1) Open it in RStudio: go to the *Packages* tab in RStudio, scroll down to the *healthiar* package and clicking on the hyperlinks *healthiar* > *User guides, package vignettes and other documentation* > *healthiar::intro_to_healthiar*
2) Open it in browser: run `browseVignettes("healthiar")` in the console and click on the *HTML* hyperlink on the page that opens up in your browser

# DISCLAMER AND LICENCE
By using *healthiar*, you confirm that you agree with the following disclaimer and terms of the licence:

a) Disclaimer: The R package *healthiar* is work in progress and the developers are not liable for the results. 

b) License: Available [here](https://github.com/SwissTPH/healthiar/blob/master/LICENSE.md).

# FEEDBACK
Feel free provide feedback via [GitHub issues](https://github.com/SwissTPH/healthiar/issues)

# CITATION
We love that you use *healthiar*. In that case, please do not forget to cite *healthiar* in your work. Three options to get there:

a) In your R console, enter *citation("healthiar")*. 

b) On the [Github page](https://swisstph.github.io/healthiar/authors.html#citation)  

c) See [CITATION.R](https://github.com/SwissTPH/healthiar/blob/master/inst/CITATION)

In the option a, you see the version number of *healthiar* that your are using. In options b and c, you see the updated citation (might be more recent than your installed version). 

# TRAININGS
If you would like us to introduce the *healthiar* at a conference or another event please get in touch: alberto.castrofernandez@swisstph.ch and axel.luyten@swisstph.ch

# ACKNOWLEDGEMENTS
The development of *healthiar* has been funded by the EU project BEST-COST. 

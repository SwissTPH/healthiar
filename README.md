# healthiar <a href="https://swisstph.github.io/healthiar/"><img src="man/figures/logo.png" align="right" height="138" alt="healthiar website" /></a>

# WELCOME TO *healthiar* 

*healthiar* is an R package to quantify and monetize health impacts attributable to exposure (e.g. air pollution, noise...).

# INSTALLATION 
We recommend to frequently install the newest *healthiar* version. Please note that **`healthiar` requires R version 4.3.0 or higher**. There are two options to install *healthiar*:

a) **From CRAN**: Click on the *Packages* tab in RStudio and on the *Install* button. Leave the *Install from:* option set to *Reporsitory (CRAN)* and then search and select *healthiar* and finally click on *Install*, keeping *Install dependencies* activated.

b) **From Github (most recent version)**: Run the following commands below in RStudio to install *healthiar*:
- `install.packages(c("knitr", "rmarkdown"))`
- `remotes::install_github(repo = "SwissTPH/healthiar", build_vignettes = TRUE)`
- Note: install or update all package dependencies (= other packages that are needed for *healthiar*) if you get asked to do so

**After installation**, do not forget to load the package by running the call `library(healthiar)`. 

# GETTING STARTED 
To get started with the `healthiar` R package, we recommend to look at the vignette (~ package manual) *intro_to_healthiar*, which you cou can access

a) on the [package website](https://swisstph.github.io/healthiar/articles/index.html) (recommended)

b) in R Studio: Click on the *Packages* tab in RStudio, scroll down to the *healthiar* package and clicking on the hyperlinks *healthiar* > *User guides, package vignettes and other documentation* 

c) in the web browser: Run `browseVignettes("healthiar")` in the R console and the page will open up in your browser

Additionally, a 45 minutes introduction to the package can be found [here](https://team.swisstph.ch/s/aN_wN5MUTAS3bwEkWvtvaQ). The slides of the presentation can be found [here](https://github.com/SwissTPH/healthiar/tree/master/varia/Workshops_and_demos/workshop)

See the function help pages for information about specific functions. In RStudio, you can access the function documentation of e.g. the function `attribute_health` by

a) going to the [reference page of the package website]{https://swisstph.github.io/healthiar/reference/index.html}

b) running `?attribute_health` in RStudio (with `healthiar` loaded)

c) going to the `Packages tab` and then clicking on `healthiar`

# CITATION
We love that you use *healthiar*. In that case, please do not forget to cite *healthiar* in your work. Three options to get there: 

a) On the [healthiar package website](https://swisstph.github.io/healthiar/authors.html#citation)  

b) See [CITATION.R](https://github.com/SwissTPH/healthiar/blob/master/inst/CITATION)

c) In your R console, enter *citation("healthiar")*.

In options b) and c), you always see the updated citation. In option a), you see citation of the *healthiar* version that you have installed locally, which might be outdated.

# DISCLAMER AND LICENCE
By using *healthiar*, you confirm that you agree with the following disclaimer and terms of the licence:

a) Disclaimer: The R package *healthiar* is work in progress and the developers are not liable for the results. 

b) License: Available [here](https://github.com/SwissTPH/healthiar/blob/master/LICENSE.md).

# FEEDBACK
Feel free provide feedback via [GitHub issues](https://github.com/SwissTPH/healthiar/issues)

# PRESENTING *healthiar*
If you would like us to present *healthiar* at a conference, lecture or training, please, contact us: alberto.castrofernandez@swisstph.ch and axel.luyten@swisstph.ch

# ACKNOWLEDGEMENTS
*healthiar* was been developed under the framework of EU project BEST-COST. BEST-COST is funded by the European Union’s Horizon Europe programme under Grant Agreement No.101095408.

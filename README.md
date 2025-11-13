# WELCOME TO *healthiar* 
*healthiar* is an R package to quantify and monetize the burden of disease attributable to exposure

# INSTALLATION 
We recommend to frequently install the newest *healthiar* version.

From CRAN: 
Click on the *Packages* tab in RStudio and in *Install*. 
From CRAN, search *healthiar* and click on *Install* keeping *Install dependencies* activated.

From Github (most recent version):
Run the following commands below in RStudio to install *healthiar*:

1) `install.packages(c("knitr", "rmarkdown"))`

2) `remotes::install_github(repo = "SwissTPH/healthiar", build_vignettes = TRUE)`

3) If you get asked to install or update any dependencies (= other packages that are needed for *healthiar*) please install or update all of them.

Loading the package:
Do not forget to load the package after the installation entering in your R console: `library(healthiar)`.

Requirement:
R version 4.3.0 or higher

# GETTING STARTED 
A 45 minutes introduction to the package can be found [here](https://team.swisstph.ch/s/aN_wN5MUTAS3bwEkWvtvaQ)

The slides of the presentation can be found [here](https://github.com/SwissTPH/healthiar/tree/master/varia/Workshops_and_demos/workshop)

If you need more detailed information, have a look at the vignette (~ package manual) *intro_to_healthiar*. You can access it

a) on the [package website](https://swisstph.github.io/healthiar/articles/index.html) (recommended)

b) in R Studio: Click on the *Packages* tab in RStudio, scroll down to the *healthiar* package and clicking on the hyperlinks *healthiar* > *User guides, package vignettes and other documentation* 

c) in the web browser: Run `browseVignettes("healthiar")` in the R console and the page will open up in your browser

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
The development of *healthiar* has been funded by the EU project BEST-COST. 

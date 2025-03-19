# GET STARTED WITH THE *healthiar* PACKAGE

*(For initial installation of the healthiar package see steps further below)*

To get started with the healthiar package please consult the intro vignette "intro_to_healthiar" (i.e. documentation on how to use the healthiar package), by either 
1) running *browseVignettes("healthiar")* in the console and clicking on *HTML* on the page that pops up
2) going to the *Packages* tab in RStudio, scrolling to the *healthiar* package and clicking on *healthiar* > *User guides, package vignettes and other documentation* > *HTML*

# Initial installation of the *healthiar* package from GitHub

Requirements:
- GitHub account
- Access to the [BEST-COST GitHub page](https://github.com/best-cost/best-cost_WPs)

If you encounter any difficulties let Alberto or Axel know.

**Step 1** – Generate a GitHub personal access token (PAT)
1.	Sign in to [www.github.com](www.github.com)
2.	Click on your profile icon (top right of the screen) and then on *Settings*
3.	Scroll down and click *Developer settings* > *Personal acess tokens* > *Tokens (classic)*
4.	Click *Generate new token* > *Generate new token (classic)*
5.	Confirm access by signing in to you GitHub profile
6.	Name your token (e.g. "best-cost token"), set *No expiration date*, and tick the scopes boxes shown in the screenshot below (i.e. all boxes of repo and write:packages; no other boxes)
7.	Scroll down and click *Generate token*
8.	Copy your token and **save it somewhere (!)**

**Step 2** – Connect RStudio with your GitHub profile
1.	Open RStudio
2.	Install the R package *credentials*
3.	Run the code *credentials::set_github_pat()* in the console
4.	A little sign-in window appears: choose *Token*
5.	Enter your PAT (personal access token) into the field and click *Sign in*

Step 3 – Download & install the newest package version 
1.	Open RStudio
2.	Install the R package *remotes*
3.	Run the code *remotes::install_github(repo = "best-cost/best-cost_WPs", subdir = "/r_package/healthiar", ref = "HEAD", force = TRUE, build_vignettes = TRUE)* to install the *healthiar* package from GitHub
4.	If prompted to update some existing packages please do so
5.	Run *library(healthiar)* to load the package
6.	Done! : )

# Update the *healthiar* package

After initial installation you can install the newest/current version of the bestcost R package by running 
*remotes::install_github(repo = "best-cost/best-cost_WPs", subdir = "/r_package/healthiar", ref = "HEAD", force = TRUE, build_vignettes = TRUE)*

***** TERMS OF USE *****

By using the R package *healthiar* or accessing its corresponding [GitHub website](https://github.com/best-cost/) the user agrees not to disseminate the *healthiar* R package or any parts of it to third parties. If you have any questions regarding the terms of use write to <alberto.castrofernandez@swisstph.ch>.

***** NOTE ******

The development of "healthiar" is part of the European Union project BEST-COST (Burden of disease based methods for estimating the socio-economic cost of environmental stressors). This R package is not published at this moment. It will be published as open-source towards the end of BEST-COST, most likely in 2026.

***** DISCLAIMER *****

This is work in progress. Therefore, the functions of the R package, as including the required input data and the produced output data can change at any moment. The developers are not liable for any calculation errors or inaccuracies resulting from the use of the *healthiar* package.

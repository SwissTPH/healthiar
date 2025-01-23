.onAttach <- function(libname, pkgname) {
  # packageStartupMessage("This is version ", packageVersion(healthiar),
  #                       " of ", healthiar, "Disclaimer:")
  packageStartupMessage("Disclaimer: This R package (healthiar) is beeing developed under the framework of the project BEST-COST (Burden of disease based methods for estimating the socio-economic cost of environmental stressors). This is work in progress. Therefore, the functions of the R package, as including the required input data and the produced output data can change at any moment.
This R package will be published as open-source but has not been published yet. Therefore, it is extrictly forbiden to disseminate the source code as well as the functions of the R package without explicit permission of the main developers: Alberto Castro (Swiss TPH) and Axel Luyten (Swiss TPH).")
}

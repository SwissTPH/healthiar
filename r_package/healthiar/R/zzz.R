.onAttach <- function(libname, healthiar) {
  packageStartupMessage("This is version ", packageVersion(healthiar),
                        " of ", healthiar, "Disclaimer:")
}

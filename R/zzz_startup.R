.onAttach <- function(libname, pkgname) {
  # packageStartupMessage("This is version ", packageVersion(healthiar)," of ", healthiar,)
  packageStartupMessage("***** DISCLAIMER *****\nhealthiar is provided \"as is\", without warranty of any kind. The developers are not liable for any calculation errors, inaccuracies or results obtained with it. Functions, arguments and results may change between versions; please check the changelog when updating.\n***** HAPPY CODING *****")
}


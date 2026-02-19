# Just to avoid note of check() because no Rdpack:: call

#' @keywords internal
"_PACKAGE"

#' @noRd
.dummy_rdpack <- function() {
  # We use a real function name here so the check finds it
  # wrap in if(FALSE) so it never actually executes
  if (FALSE) Rdpack::reprompt()
}

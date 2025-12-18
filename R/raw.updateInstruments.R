#' Updates data from instruments into package
#'
#' @description
#' The instrument functions are added to rawBase when it is created
#' with create_rawBase() for example
#'
#'
#' @param rawBase object with information
#' @param quiet suppresses messages
#'
#' @importFrom dplyr "%>%"
#'
#' @export
raw.updateInstrument <- function(rawBase, quiet=FALSE) {
  if (is.null(rawBase$instruments)) return(rawBase)

  for(func in names(rawBase$instruments)) {
    if (!quiet) cat("Calling",func,"instrument for update.\n")
    rawBase$instruments[[func]](rawBase)
  }

  # returns updated rawBase
  rawBase
}


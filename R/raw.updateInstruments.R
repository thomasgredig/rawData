#' Updates data from instruments into package
#'
#' @description
#' The instrument functions are added to rawBase when it is created
#' with create_rawBase() for example
#'
#'
#' @param rawBase object with information
#' @importFrom dplyr "%>%"
#'
#' @export
raw.updateInstrument <- function(rawBase) {
  if (is.null(rawBase$instruments)) return()

  for(func in names(rawBase$instruments)) {
    r <- rawBase$instruments[[func]](rawBase)
  }
}


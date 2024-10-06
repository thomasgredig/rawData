#' Cleans the rawBase Variables
#'
#' @description
#' Any variable that contains "Paths" will have duplicate paths removed.
#'
#' @examples
#' rawBase = list(varWithDuplicates = c("letters","A","B","A","A","B","C"),
#'                 myPaths = c("letters","A","B","A","A","B","C"))
#' .cleanRawBase(rawBase)
#'
#' @export
.cleanRawBase <- function(rawBase) {
  paths_vars <- grep("Paths", names(rawBase), value = TRUE)
  rawBase[paths_vars] <- lapply(rawBase[paths_vars], unique)

  rawBase
}

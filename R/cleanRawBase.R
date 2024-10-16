#' Cleans the rawBase Variables
#'
#' @description
#' Any variable that contains "Paths" will have duplicate paths removed.
#'
#' @param rawBase S3 object
#' @returns updated rawBase object
#' @examples
#' rawBase = list(varWithDuplicates = c("letters","A","B","A","A","B","C"),
#'                 myPaths = c("letters","A","B","A","A","B","C"))
#' .cleanRawBase(rawBase)
#'
#' @export
.cleanRawBase <- function(rawBase) {
  paths_vars <- grep("paths", names(rawBase), value = TRUE)
  rawBase[paths_vars] <- lapply(rawBase[paths_vars], unique)

  rawBase
}

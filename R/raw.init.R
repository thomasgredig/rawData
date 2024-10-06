#' rawBase initialization
#'
#' @description
#' the rawBase list contains key information about how
#' data is stored in the dataProject; this includes
#' folder information, etc. - this function is called
#' to initialize this list.
#'
#' Will prompt for path to search for data files of that
#' project and also a path to store the SQL data.
#'
#' @param projectName short string with the project name "spinPc"
#' @param paths path or paths with data files
#' @param sqlPaths paths for location of SQL database
#'
#' @export
raw.init <- function(projectName, paths = NA, sqlPaths = NA, recursive=TRUE) {

  # initialize rawBase
  # ==================
  # get current package name
  description <- desc::desc(file = "DESCRIPTION")
  pkgName <- description$get("Package")
  rawBase = raw.rawBase(projectName, pkgName, paths, sqlPaths)

  # look for files and add them to the dataRAW
  dataRAW = raw.update(rawBase, recursive=recursive)

  list(
    rawBase = rawBase,
    dataRAW = dataRAW)
}

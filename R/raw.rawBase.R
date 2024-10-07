#' Create rawBase
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
#' @param pkgName name of the data package, such as "dataSpin"
#' @param paths path or paths with data files
#' @param sqlPaths paths for location of SQL database
#'
#' @export
raw.rawBase <- function(projectName, pkgName = "dataSpin", paths = NA, sqlPaths = NA) {
  projLen = nchar(projectName)
  if (projLen<=2 | projLen>=10) {
    warning("projectName of RAW data appears incorrect in size.")
  }

  # prompt for data and SQL path
  if (is.na(paths)) {
    paths = .promptPath("Enter path: ")
    if (!dir.exists(paths)) { warning("Path not found."); paths = "." }
  }
  if (is.na(sqlPaths)) {
    sqlPaths = .promptPath("Enter path for SQL repository: ")
    if (!dir.exists(sqlPaths)) { warning("Path not found."); sqlPaths = "." }
  }

  list(versionRAW = packageVersion("rawData"),
       pkgName = pkgName,
       token = floor(runif(1,1000,1000*1000)),  # random token for DB identification
       rawPaths = c(paths),
       sqlPaths = c(sqlPaths),
       projectName = projectName
  )
}

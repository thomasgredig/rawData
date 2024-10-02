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
#' @param pkgName name of the data package, such as "dataSanchez"
#'
#' @export
raw.init <- function(projectName, pkgName) {
  projLen = nchar(projectName)
  if (projLen<=2 | projLen>=10) {
    warning("projectName of RAW data appears incorrect in size.")
  }

  # prompt for data and SQL path
  path = .promptPath("Enter path: ")
  if (!dir.exists(path)) { warning("Path not found."); path = "." }
  pathSQL = .promptPath("Enter path for SQL repository: ")
  if (!dir.exists(pathSQL)) { warning("Path not found."); pathSQL = "." }

  list(versionRAW = packageVersion("rawData"),
       pkgName = pkgName,
       rawPaths = c(path),
       sqlPaths = c(pathSQL),
       projectName = projectName
  )
}

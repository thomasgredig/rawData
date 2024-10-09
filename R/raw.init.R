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
#' @param fileDataRAW filename with RAW-ID.csv data file (legacy code)
#' @param paths path or paths with data files
#' @param sqlPaths paths for location of SQL database
#' @param ... parameters for raw.addFiles, such as verbose
#'
#' @importFrom here here
#'
#' @export
raw.init <- function(projectName, fileDataRAW="", paths = NA, sqlPaths = NA, recursive=TRUE, ...) {
  # initialize rawBase
  # ==================
  # get current package name
  pkgName <- basename(here::here())
  rawBase = raw.rawBase(projectName, pkgName, paths, sqlPaths)

  dRaw = NULL
  if (file.exists(fileDataRAW)) {
    dRaw = raw.importRAWID(fileDataRAW)
  }

  # look for files and add them to the dataRAW
  dataRAW = raw.addFiles(rawBase, dataRAW = dRaw, recursive=recursive, ...)

  # create the SQL database
  raw.initDB(rawBase, ...)

  list(
    rawBase = rawBase,
    dataRAW = dataRAW)
}

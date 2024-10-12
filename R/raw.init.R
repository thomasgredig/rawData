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
raw.init <- function(projectName,
                     legacyRAWIDfile="",
                     paths = NULL,
                     sqlPaths = NULL,
                     recursive=TRUE, ...) {
  # create a rawBase object with the information
  rawBase = create_rawBase(projectName,
                           paths = paths,
                           sqlPaths = sqlPaths,
                           legacyRAWIDfile = legacyRAWIDfile,
                           recursive = recursive)

  # import legacy file IDs first
  rawBase = raw.importRAWID(rawBase)

  # look for files and add them to the dataRAW
  rawBase = raw.addFiles(rawBase)

  # create the SQL database
  raw.initDB(rawBase, ...)

  rawBase
}

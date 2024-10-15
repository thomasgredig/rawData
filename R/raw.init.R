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
#' @param recursive logical weather to search paths resursively
#' @param instrument_list list with instruments to be updated
#' @param ... parameters for raw.addFiles, such as verbose
#'
#' @returns rawBase object
#'
#' @importFrom here here
#' @importFrom usethis ui_silence
#'
#' @export
raw.init <- function(projectName,
                     legacyRAWIDfile="",
                     paths = NULL,
                     sqlPaths = NULL,
                     recursive=TRUE,
                     instrument_list = NULL,
                     ...) {
  # create a rawBase object with the information
  rawBase = create_rawBase(projectName,
                           paths = paths,
                           sqlPaths = sqlPaths,
                           legacyRAWIDfile = legacyRAWIDfile,
                           recursive = recursive,
                           instrument_list = instrument_list)

  # import legacy file IDs first
  rawBase = raw.importRAWID(rawBase)

  # look for files and add them to the dataRAW
  rawBase = raw.addFiles(rawBase)

  # create the SQL database
  raw.initDB(rawBase, ...)

  # save the rawBase
  ui_silence(
    usethis::use_data(rawBase, overwrite = TRUE)
  )

  # update the XRD, AFM, profiles.
  raw.updateInstrument(rawBase)

  rawBase
}

#' Update dataRAW with new files
#'
#' @description
#' Finds files that match the project name and are located in the path.
#' Then appends those files to the dataRAW.
#'
#' @param rawBase object with information about file locations
#' @param path path with updated information about files.
#' @param project any new project to be added to be searched
#' @param file_pattern pattern to search for additional files to be included
#' @param sqlPath any SQL path to be added
#' @param recursive logical, recursive search in path
#' @param instrument_list list, contains instruments to be added
#' @param extensions additional file extensions; i.e. asc or rasx
#' @param forceUpdateMissing if TRUE, all missing files are checked for folder update, useful for legacy RAWID files
#' @param quiet suppresses messages
#'
#' @importFrom usethis ui_silence
#'
#' @returns rawBase object
#'
#' @export
#'
raw.update <- function(
    rawBase,
    path = NULL,
    project = NULL,
    file_pattern = NULL,
    sqlPath = NULL,
    recursive = TRUE,
    instrument_list = list(XRD = instrumentXRD, AFM = instrumentAFM, ATE = instrumentATE),
    extensions = character(),
    forceUpdateMissing = FALSE,
    quiet=FALSE
) {
  # Validate input -----------------------------------------------------------
  check_rawBase(rawBase)

  # Update instruments -------------------------------------------------------
  if (length(instrument_list) > 0L) {
    rawBase <- raw.addInstrument(
      rawBase,
      instrument_list = instrument_list
    )
  }

  # Update extensions --------------------------------------------------------
  if (length(extensions) > 0L) {
    rawBase$extensions <- unique(c(rawBase$extensions,extensions))
  }

  # Update projects ----------------------------------------------
  if (!is.null(project)) {
    rawBase$project_name <- unique(c(rawBase$project_name, project))
  }

  # Add paths and files ------------------------------------------------------
  # Updates History ----------------------------------------------------------
  rawBase <- raw.addPath(  # this will add new projects / paths to search
    rawBase,
    project  = project,
    path     = path,
    sqlPath  = sqlPath,
    recursive = recursive
  )
  if (!is.null(file_pattern)) {
    rawBase$import_history = update_rawBaseHistory(rawBase$import_history,
                                                   "files",
                                                   "",
                                                   file_pattern,
                                                   recursive)
  }

  rawBase <- raw.addFiles(rawBase, file_pattern, verbose=!quiet)

  # check whether the reference folder has changed and then update missing files
  if (forceUpdateMissing) { rawBase <- raw.updateMissingFilePaths(rawBase) }

  # Update SQL databse
  rawBase = raw.updateDB(rawBase, quiet=quiet)

  # Update instrument-specific data -----------------------------------------
  # will update instrument specific files
  rawBase = raw.updateInstrument(rawBase, quiet=quiet)

  # Persist updated object ---------------------------------------------------
  usethis::ui_silence(
    usethis::use_data(rawBase, overwrite = TRUE)
  )

  # Return updated rawBase --------------------------------------------------
  rawBase
}



#' Add dataRAW with new files
#'
#' @description
#' Finds files that match the project name and are located in the path.
#' Then appends those files to the dataRAW; if the file already exists
#' it will be updated, but not added; the ID remains the same.
#'
#' @param rawBase see raw.init() to create this
#' @param file_pattern string to search for extra files
#' @param verbose logical to output more information
#'
#' @importFrom cli cli_alert_warning
#'
#' @returns rawBase object
#'
#' @export
raw.addFiles <- function(rawBase, file_pattern, verbose=FALSE) {
  check_rawBase(rawBase)

  # before adding new files, check which files are accessible
  rawBase <- raw.checkMissing(rawBase)

  # find files that could potentially be added to dataRAW
  fList = raw.find(rawBase, file_pattern, quiet=TRUE)

  # Quit if no files are found.
  if (length(fList)==0) {
    if (interactive()) {
      cli_alert_warning("No RAW data files are found in these folders.")
      cli_alert_warning("Check file naming conventions.")
      cli_alert_warning(paste("File names must include project name: _",rawBase$project_name,"_"))
    }
    return(rawBase)
  }

  startID = 7
  new_dataRAW = create_dataRAW(startID, rawBase$raw_paths, fList)

  if (is.null(rawBase$dataRAW) |  (length(rawBase$dataRAW)==0)) {
    rawBase$dataRAW = new_dataRAW
  } else {
    rawBase$dataRAW = rbind.dataRAW(rawBase$dataRAW, new_dataRAW)
  }

  rawBase
}


.getCRC <- function(filename) {
  strtoi( substr(md5sum(filename),1,7), base = 16 )
}


.truncatePath <- function(pRAW, pfad) {
  pRAW = gsub("\\\\","/",pRAW)
  gsub(pRAW,'', pfad)
}

.commonPath <- function(fList) {
  paths = unique(dirname(fList))
  Reduce(.common_prefix, paths)
}

.common_prefix <- function(str1, str2) {
  min_length <- min(nchar(str1), nchar(str2))
  for (i in 1:min_length) {
    if (substr(str1, i, i) != substr(str2, i, i)) {
      return(substr(str1, 1, i - 1))
    }
  }
  return(substr(str1, 1, min_length))
}

# 20230208_CuPcAnnealing_RM_AFM_RM20230125Si11_Post_06b.ibw
.getSampleName <- function(filename) {
  # Use a regular expression to match the pattern RM followed by digits and letters
  # pattern <- "[_-][A-Za-z]+\\d{8}[A-Za-z0-9]+[_-]"
  # match <- regmatches(filename, regexpr(pattern, filename))
  # return(match)
  gsub(".*_.*_.*_.*_([A-Za-z0-9]+)_.*","\\1", filename)
}




#' Returns File Type
#' @importFrom tools file_ext
#' @param filename filename with type extension
#' @noRd
.getFileType <- function(filename) {
  type = ""
  f = basename(filename)
  if (grepl('\\_XRD',f)) type = "XRD"
  if (grepl('\\_XRR',f)) type = "XRR"
  if (grepl('\\_AMR',f)) type = "AMR"
  if (grepl('\\_FMR',f)) type = "FMR"
  if (grepl('\\_AFM',f)) type = "AFM"
  if (grepl('\\_EDS',f)) type = "EDS"
  if (grepl('\\_SEM',f)) type = "SEM"
  if (grepl('\\_Rxx',f)) type = "AMR"
  if (grepl('\\_VSM',f)) type = "VSM"

  if (type=="") {
    f = tools::file_ext(filename)
    if (grepl('nid',f)) type = 'AFM'
    if (grepl('ras[x]',f)) type = 'XRD'
    if (grepl('ibw',f)) type = 'AFM'
    if (grepl('dat',f)) type = 'PPMS'
    if (grepl('tiff',f)) type = 'AFM'
    if (grepl('\\d{3}',f)) type = 'AFM'
    if (grepl('csv',f)) type = 'table'
    if (grepl('txt',f)) type = 'text'
  }

  type
}

.extendColumns <- function(df, dfNames) {
  mIn = names(df)
  d = df
  for(m in dfNames) {
    if (!(m %in% mIn)) {
      dAdd = rep("", nrow(d))
      d = cbind(d, dAdd)
    }
  }
  names(d) = dfNames
  d
}

#' provides error if rawBase is not in the correct format
#' @noRd
check_rawBase <-function(rawBase) {
  if(!inherits(rawBase,"rawBase")) {
    stop("Requires a rawBase object.")
  }
  TRUE
}


#' Update paths for missing raw files
#'
#' Searches for files in `rawBase$dataRAW` that are currently marked as missing
#' and attempts to locate them under the directories listed in
#' `rawBase$raw_paths`. Matching is based on filename and CRC, assuming the
#' filename itself has not changed. When a match is found, the corresponding
#' `path` entry is updated. After all candidate files are processed, missing-file
#' status is recomputed with [raw.checkMissing()].
#'
#' If no files are marked as missing, the function leaves `rawBase` unchanged and
#' emits an informational message.
#'
#' @param rawBase A raw-data database object containing at least:
#' \describe{
#'   \item{`dataRAW`}{A data frame-like object with columns `ID`, `filename`,
#'   `crc`, `path`, and `missing`.}
#'   \item{`raw_paths`}{A character vector of base directories to search for
#'   relocated files.}
#' }
#'
#' @details
#' For each row in `rawBase$dataRAW` with `missing == TRUE`, the function calls
#' `.findSubPath()` using the stored filename and CRC. If a matching file is
#' found, the file path is updated in `dataRAW`. Once all missing files have been
#' checked, the modified table is written back into `rawBase`, and
#' [raw.checkMissing()] is called to refresh the `missing` flags.
#'
#' The function assumes that missing files may have been moved to a different
#' directory, but that their filenames remain unchanged.
#'
#' @return
#' The updated `rawBase` object, with revised file paths where matches were found
#' and refreshed missing-file status.
#'
#' @seealso [raw.checkMissing()], `.findSubPath()`
#'
#' @examples
#' \dontrun{
#' rawBase <- raw.updateMissingFilePaths(rawBase)
#' }
raw.updateMissingFilePaths <- function(rawBase) {
  # find all missing files and update location (filename is assumed to be the same!)
  dataRAW <- as.data.frame(rawBase$dataRAW)
  missing_files <- which(dataRAW$missing==TRUE)
  if(length(missing_files)>0L) {
    for(m in missing_files) {
      ID = dataRAW$ID[m]
      filename = dataRAW$filename[m]
      CRC = dataRAW$crc[m]
      updated_path = .findSubPath(rawBase$raw_paths, filename, CRC)
      if(length(updated_path)>0L) {
        dataRAW$path[m] = updated_path
      }
    }
    rawBase$dataRAW <- dataRAW # update

    # update the missing files
    rawBase <- raw.checkMissing(rawBase)
  } else {
    cat("No missing files, so no update.")
  }

  rawBase
}


.findSubPath <- function(paths, filename, crc) {
  escape_regex <- function(x) { gsub("([][{}()+*^$.|?\\-])", "\\\\\\1", x) }
  new_path = ""
  for (path in paths) {
    if (dir.exists(path)) {
      file_list = dir(path, pattern=escape_regex(filename), recursive = TRUE)
      if(length(file_list)>0L) {
        for(file_found in file_list) {
          fname = file.path(path, file_found)
          c2 = .getCRC(fname)
          if (crc==c2) {
            new_path = dirname(file_found)
            next
          }
        }
      }
    }
  }
  new_path
}

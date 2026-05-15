#' Create an overview table for XRD RASX files
#'
#' Scans the files listed in `rawBase$dataRAW`, selects entries whose filenames
#' end in `.rasx`, reads their RASX measurement-condition/header information,
#' and returns a combined overview table. Each returned row is augmented with
#' the corresponding `ID` from `rawBase$dataRAW`.
#'
#' Files that are listed in `rawBase$dataRAW` but are not found on disk are
#' skipped.
#'
#' @param rawBase A raw database object containing a `dataRAW` table with at
#'   least the columns `filename` and `ID`.
#'
#' @return A `data.frame` containing the extracted XRD header/measurement
#'   information for all available `.rasx` files. If no matching files are found,
#'   or no matching files exist on disk, an empty `data.frame` is returned.
#' @importFrom rigakuXRD xrd_read_RASX_header
#' @export
raw.xrdInfo <- function(rawBase) {
  df <- as.data.frame(rawBase$dataRAW)
  idx <- grep("rasx$", df$filename)
  r <- data.frame()
  for(i in idx) {
    cat(".")
    f <- raw.getFilename(rawBase, df$ID[i])
    if (!file.exists(f)) next
    d <- xrd_read_RASX_header(f)
    d$ID = df$ID[i]
    r = rbind(r, d)
  }
  r
}



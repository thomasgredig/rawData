#' Load XRD instrument data
#'
#' @param rawBase rawBase object
#' @returns data frame with XRD data for all XRD files in rawBase
#'
#' @importFrom rigakuXRD xrd.import
#'
#' @export
instrumentXRD <- function(rawBase) {
  .isxrd <- function(filename) {
    grepl('rasx$', filename) | grepl('asc$', filename)
  }

  df <- as.data.frame(rawBase$dataRAW)

  for(ID in df$ID) {
    filename = raw.getFilename(rawBase, ID)
    if (!file.exists(filename)) next
    if (!.isxrd(filename)) next

    d = xrd.import(filename)
    d$ID = ID
    r = rbind(r,d)
  }

  r
}


#' Load XRD instrument data
#'
#' @param rawBase rawBase object
#' @returns data frame with XRD data for all XRD files in rawBase
#'
#' @importFrom rigakuXRD xrd.import
#' @importFrom usethis use_data ui_silence
#'
#' @export
instrumentXRD <- function(rawBase) {
  # which files are recognized as XRD files
  .isxrd <- function(filename) {
    grepl('rasx$', filename) | grepl('asc$', filename)
  }

  # data frame with all files
  df <- as.data.frame(rawBase$dataRAW)

  # load all XRD data
  r = data.frame()
  for(ID in df$ID) {
    filename = raw.getFilename(rawBase, ID)
    if (!file.exists(filename)) next
    if (!.isxrd(filename)) next

    d = xrd.import(filename)
    d$ID = ID
    r = rbind(r,d)
  }

  # save data to file
  dataXRD = r
  ui_silence(
    use_data(dataXRD, overwrite=TRUE)
  )

  invisible(TRUE)
}


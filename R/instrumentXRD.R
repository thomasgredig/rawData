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

  # import previous datasets
  if(exists("dataXRD")) {
    r = dataXRD
    old_IDs = unique(r$ID)
  } else {
    r = data.frame()
    old_IDs = c()
  }

  # data frame with all files
  df <- as.data.frame(rawBase$dataRAW)

  # load all XRD data
  for(ID in df$ID) {
    if (ID %in% old_IDs) next
    filename = raw.getFilename(rawBase, ID)
    if (!file.exists(filename)) next
    if (!.isxrd(filename)) next

    d = xrd.import(filename)
    d$ID = ID
    r = rbind(r,d)
  }

  dataXRD = r

  ui_silence(
    use_data(dataXRD, overwrite=TRUE)
  )

  invisible(TRUE)
}


#' Load XRD instrument data
#'
#' @param rawBase rawBase object
#' @returns  rawBase
#'
#' @importFrom rigakuXRD xrd_import
#' @importFrom usethis use_data ui_silence
#'
#' @export
instrumentXRD <- function(rawBase) {
  # which files are recognized as XRD files
  .isxrd <- function(filename) {
    grepl('rasx$', filename) | grepl('asc$', filename)
  }
  check_rawBase(rawBase)

  data_filename = file.path("data", "dataXRD.rda")
  if (file.exists(data_filename)) {
    load(data_filename)
  }
  # import previous datasets
  if(exists("dataXRD")) {
    r_xrd = dataXRD
    old_IDs = unique(r_xrd$ID)
  } else {
    r_xrd = data.frame()
    old_IDs = c()
  }

  # data frame with all files
  df <- as.data.frame(rawBase$dataRAW)
  if(nrow(df)==0L) {
    warning("rawBase has not files.")
    return(rawBase)
  }

  # load all XRD data
  for(ID in df$ID) {
    if (ID %in% old_IDs) next
    filename = raw.getFilename(rawBase, ID)
    if (!.isxrd(filename)) next
    if (!file.exists(filename)) next

    d = xrd_import(filename)
    d$ID = ID
    r_xrd = rbind(r_xrd,d)
  }

  dataXRD = r_xrd

  ui_silence(
    use_data(dataXRD, overwrite=TRUE)
  )

  rawBase
}


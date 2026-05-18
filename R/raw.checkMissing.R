#' Check missing files
#'
#' @param rawBase rawBase object
#' @returns dataRAW object with updated missing fields
#'
#' @export
raw.checkMissing <- function(rawBase) {
  check_rawBase(rawBase)

  dataRAW = as.data.frame(rawBase$dataRAW)
  IDs = dataRAW$ID

  for(ID in IDs) {
    m <- which(ID == IDs)
    filename = raw.getFilename(rawBase,ID)
    dataRAW$missing[m] = !file.exists(filename)
  }

  class(dataRAW) = "dataRAW"
  rawBase$dataRAW = dataRAW
  rawBase
}

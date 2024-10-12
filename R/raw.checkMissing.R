#' Check missing files
#'
#' @param dataRAW dataRAW S3 object with files
#' @returns dataRAW object with updated missing fields
#'
#' @export
raw.checkMissing <- function(rawBase) {
  if (!is(rawBase,"rawBase")) stop("rawBase oject required.")

  dataRAW = as.data.frame(rawBase$dataRAW)
  IDs = dataRAW$ID

  for(ID in IDs) {
    m <- which(ID == IDs)
    filename = raw.getFilename(rawBase,ID)
    missing = !file.exists(filename)
    dataRAW$missing[m] = missing
  }

  class(dataRAW) = "dataRAW"
  rawBase$dataRAW = dataRAW
  rawBase
}

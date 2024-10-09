#' Check missing files
#'
#' @param dataRAW dataRAW S3 object with files
#' @returns dataRAW object with updated missing fields
#'
#' @export
raw.checkMissing <- function(dataRAW) {
  IDs = dataRAW$ID
  for(ID in IDs) {
    m <- which(ID %in% dataRAW$ID)
    filename = raw.getFilename(dataRAW,ID)
    missing = !file.exists(filename)
    dataRAW$missing[m] = missing
  }

  dataRAW
}

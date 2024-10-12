#' Check missing files
#'
#' @param dataRAW dataRAW S3 object with files
#' @returns dataRAW object with updated missing fields
#'
#' @export
raw.checkMissing <- function(d) {
  IDs = d$df$ID
  for(ID in IDs) {
    m <- which(ID == IDs)
    filename = raw.getFilename(d,ID)
    missing = !file.exists(filename)
    d$df$missing[m] = missing
    # print(paste(ID,"=",missing))
  }

  d
}

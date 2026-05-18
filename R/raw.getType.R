#' Type of rawData item
#' @param rawData type
#' @param ID uique ID
#' @returns string with type, such as "XRD", "AFM", etc.
#' @export
raw.getType <- function(rawData, ID) {
  m = which(rawData$ID == ID)
  rawType = ""
  if(length(m)==1) {
    rawType = rawData$type[m]
  }
  rawType
}

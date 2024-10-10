#' Type of rawData item
#' @returns string with type, such as "XRD", "AFM", etc.
#' @export
raw.getType <- function(rawData, ID) {
  m = which(rawData$df$ID == ID)
  rawType = ""
  if(length(m)==1) {
    rawType = rawData$df$type[m]
  }
  rawType
}

#' Full Path and Filename for dataRAW object
#' @param dataRAW dataRAW object
#' @param ID dataRAW object ID
#' @returns full path of filename, if not founds returns empty NA
#' @export
raw.getFilename <- function(dataRAW, ID) {
  m = which(dataRAW$df$ID == ID)
  filename = NA
  if(length(m)==1) {
    filename = file.path(dataRAW$df$path[m], dataRAW$df$filename[m])
    # search for local master path using rawBase information
    # could be saved on different folders on different storage
    # systems
    paths = attr(dataRAW, "pRAW")
    for(path in paths) {
      f = file.path(path, filename)
      if (file.exists(f)) {
        filename = f
        break
      }
    }
  }
  filename
}

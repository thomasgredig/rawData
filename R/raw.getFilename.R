#' Full Path and Filename for dataRAW object
#' @param dataRAW dataRAW object
#' @param ID dataRAW object ID
#' @returns full path of filename, if not founds returns empty NA
#' @export
raw.getFilename <- function(rawBase, ID) {
  check_rawBase(rawBase)

  dataRAW = as.data.frame(rawBase$dataRAW)
  m = which(dataRAW$ID == ID)
  full_filename = ""
  if(length(m)==1) {
    filename = file.path(dataRAW$path[m], dataRAW$filename[m])
    # search for local master path using rawBase information
    # could be saved on different folders on different storage
    # systems
    paths = rawBase$raw_paths
    for(path in paths) {
      f = file.path(path, filename)
      if (file.exists(f)) {
        full_filename = f
        break
      }
    }
  }
  full_filename
}

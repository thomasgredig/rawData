#' Find project files
#'
#' @description
#' Searches a path for project files, you need to provide a projectName,
#' such as "spinPc" or another short string that must be included in the
#' filename. Filenames must have a specific format to be found. Search is
#' done recursively.
#'
#' If no path is provided, then it will search in the current path or if
#' in interactive mode, will prompt for a path.
#'
#' @param projectName short string with the project name, must be part of the filename
#' @param path path to be search
#' @param recursive logical, determines whether path is searched recursively.
#'
#' @export
raw.find <- function(projectName, path = NULL, recursive=TRUE) {
  # check PATH
  if (is.null(path)) {
    if (interactive()) {
      # prompt for path
      path <- readline(prompt = "Enter path with RAW data: ")
    } else {
      warning("No path provided to search for data files.")
      path = "."
    }
  }

  dir(path,
      pattern = paste0(".*\\D{8}[_-]",projectName,"[_-].*"),
      ignore.case = TRUE,
      include.dirs = TRUE,
      full.names = TRUE,
      recursive = recursive)
}



NULL
# helper functions

.getCRC <- function(filename) {
  strtoi( raw.getMD5(filename, 7), base = 16 )
}

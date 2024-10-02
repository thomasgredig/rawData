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
#' @param rawBase list generated with raw.init()
#' @param recursive logical, determines whether path is searched recursively.
#'
#' @export
raw.find <- function(rawBase, recursive=TRUE) {
  # check PATH
  path = rawBase$rawPaths
  if (is.null(path) | length(path)==0) {
    path = .promptPath()
  }

  dir(path,
      pattern = paste0(".*\\D{8}[_-]",rawBase$projectName,"[_-].*"),
      ignore.case = TRUE,
      include.dirs = TRUE,
      full.names = TRUE,
      recursive = recursive)
}



NULL
# helper functions

.promptPath <- function(str = "Enter path with RAW data: ") {
  if (interactive()) {
    # prompt for path
    path <- readline(prompt = str)
  } else {
    warning("Use interactive prompt to get path for prompt: ",str)
    path = "."
  }
}

.getCRC <- function(filename) {
  strtoi( raw.getMD5(filename, 7), base = 16 )
}

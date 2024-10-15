#' Find project files
#'
#' @description
#' Uses the rawBase list to include instructions on how to search files.
#' Searches files that contain an 8 digit date followed by underscore or dash
#' and includes the project name, something like 20240822_something_ProjName_something
#'
#'
#' If no path is provided, then it will search in the current path or if
#' in interactive mode, will prompt for a path.
#'
#' @param rawBase list generated with raw.init()
#' @param recursive logical, determines whether path is searched recursively.
#'
#' @returns vector with file list that includes paths
#'
#' @export
raw.find <- function(rawBase, recursive=TRUE) {
  if (!is(rawBase,"rawBase")) stop("rawBase object required.")

  paths = rawBase$raw_paths
  projectName = rawBase$project_name
  project_names = c(projectName, rawBase$extra)

  fList = c()
  for(path in paths) {
    for(projectName in project_names) {
      fList = c(fList, dir(path,
          pattern = paste0(".*20\\d{6}[_-]",projectName,"[_-].*"),
          ignore.case = TRUE,
          include.dirs = TRUE,
          full.names = TRUE,
          recursive = recursive))
    }
  }
  fList
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

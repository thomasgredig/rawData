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
  # check PATH
  paths = rawBase$rawPaths
  if (is.null(paths) | length(paths)==0) {
    paths = .promptPath()
  }

  # check project Name
  projectName = rawBase$projectName
  if (is.null(rawBase$projectName)) {
    projectName = .promptPath("Enter name of project: ")
  }

  nLen = nchar(projectName)
  if ((nLen<=2) | (nLen>12)) { warning("ProjectName may be too short or too long: '", projectName,"'") }

  fList = c()
  for(path in paths) {
    fList = c(fList, dir(path,
        pattern = paste0(".*20\\d{6}[_-]",projectName,"[_-].*"),
        ignore.case = TRUE,
        include.dirs = TRUE,
        full.names = TRUE,
        recursive = recursive))
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

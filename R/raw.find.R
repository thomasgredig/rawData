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

  df_history = rawBase$import_history
  fList = c()

  if(is.null(df_history)) return(fList)

  for(i in 1:nrow(df_history)) {
    path = df_history$path[i]
    projectName = df_history$project[i]
    recursive = df_history$recursive[i]

    if (!dir.exists(path)) next

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
    # clean path if needed
    path <- gsub("^'|'$", "", path)
  } else {
    warning("Use interactive prompt to get path for prompt: ",str)
    path = "."
  }
  path
}

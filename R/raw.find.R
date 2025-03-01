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
#' @importFrom cli cli_alert_warning cli_alert_info
#' @export
raw.find <- function(rawBase, recursive=TRUE) {

  # find files with specified extensions
  find_files_with_extensions <- function(pfad, extensions) {
    cli_alert_info(paste("Loading extensions:",paste(extensions, collapse = "|")))
    # Create a pattern to match the extensions
    pattern <- paste0("\\.(", paste(extensions, collapse = "|"), ")$")

    # List files in the directory matching the pattern
    dir(path = pfad,
        pattern = pattern,
        ignore.case = TRUE,
        include.dirs = TRUE,
        full.names = TRUE,
        recursive = TRUE)
  }

  if (!is(rawBase,"rawBase")) stop("rawBase object required.")

  df_history = rawBase$import_history
  fList = c()
  fList_ext = c()

  if(is.null(df_history)) {
    cli_alert_warning("No history in rawBase object.")
    return(fList)
  }

  for(i in 1:nrow(df_history)) {
    path = df_history$path[i]
    # cli_alert_info(paste("Searching path:",path))

    projectName = df_history$project[i]
    # cli_alert_info(paste("Project:",projectName))

    recursive = df_history$recursive[i]

    if (!dir.exists(path)) {
      cli_alert_info(paste0("Path not found: <<",path,">>."))
      next
    }

    fList = c(fList, dir(path,
                         pattern = paste0(".*20\\d{6}[_-]",projectName,"[_-].*"),
                         ignore.case = TRUE,
                         include.dirs = TRUE,
                         full.names = TRUE,
                         recursive = recursive))

    # if there are any extensions, then add files with those extensions
    if (length(rawBase$ext) >0 )
      fList_ext = c(fList_ext, find_files_with_extensions(path, rawBase$ext) )
  }

  c(fList, fList_ext)
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

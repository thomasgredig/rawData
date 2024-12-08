#' Add Paths to rawBase
#'
#' @returns rawBase object
#'
#' @export
raw.addPath <- function(rawBase, path, sqlPath, recursive = TRUE) {
  if(!is.null(path)) rawBase$raw_paths = c(rawBase$raw_paths, path)
  rawBase$import_history = update_rawBaseHistory(rawBase$import_history,
                                                 "add",
                                                 rawBase$project,
                                                 path,
                                                 recursive)
  # update SQL paths
  if(!is.null(sqlPath)) {
    if (dir.exists(sqlPath)) rawBase$sql_paths = c(rawBase$sql_paths, sqlPath)
  }
  # remove duplicate paths,etc.
  .cleanRawBase(rawBase)  # remove duplicate paths,etc.
}

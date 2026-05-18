#' Add raw-data and SQL paths to a rawBase object
#'
#' Adds a new raw-data path to `rawBase$raw_paths` and records the operation in
#' the import history. Optionally, a SQL path can also be added to
#' `rawBase$sql_paths` if it exists on disk. After updating paths and history,
#' the rawBase object is cleaned to remove duplicate entries and apply any
#' internal normalization performed by [.cleanRawBase()].
#'
#' @param rawBase A rawBase object.
#' @param project A character string identifying the project associated with the
#'   path addition. This value is recorded in the import history.
#' @param path A character string giving the raw-data directory to add. If
#'   `NULL`, no raw-data path is appended.
#' @param sqlPath A character string giving the SQL directory to add. If `NULL`,
#'   no SQL path is considered. The path is only added if `dir.exists(sqlPath)`
#'   returns `TRUE`.
#' @param recursive Logical; passed to [update_rawBaseHistory()] to indicate
#'   whether the path addition should be treated as recursive. Defaults to
#'   `TRUE`.
#'
#' @details
#' The function performs three updates:
#' \enumerate{
#'   \item Appends `path` to `rawBase$raw_paths` when `path` is not `NULL`.
#'   \item Records the action in `rawBase$import_history` using
#'   [update_rawBaseHistory()] with action `"add"`.
#'   \item Appends `sqlPath` to `rawBase$sql_paths` when `sqlPath` is not `NULL`
#'   and the directory exists.
#' }
#'
#' Finally, the updated object is passed to [.cleanRawBase()] to remove
#' duplicate paths and perform any additional cleanup.
#'
#' @return
#' An updated rawBase object.
#'
#' @seealso [update_rawBaseHistory()], [.cleanRawBase()]
#'
#' @export
raw.addPath <- function(rawBase, project, path = NULL, sqlPath = NULL, recursive = TRUE) {
  if(!is.null(path)) rawBase$raw_paths = c(rawBase$raw_paths, path)
  rawBase$import_history = update_rawBaseHistory(rawBase$import_history,
                                                 "add",
                                                 project,
                                                 path,
                                                 recursive)
  # update SQL path
  rawBase <- raw.addSQLpath(rawBase, sqlPath)

  # remove duplicate paths,etc.
  .cleanRawBase(rawBase)  # remove duplicate paths,etc.
}

#' Add an SQL path to a rawBase object
#'
#' Adds a directory to `rawBase$sql_paths` if the supplied path is not `NULL`
#' and exists on disk. Duplicate entries are removed after the new path is
#' added.
#'
#' @param rawBase A rawBase object.
#' @param sqlPath A character string giving the SQL directory to add. If
#'   `NULL`, the object is returned unchanged. The path is only added if
#'   `dir.exists(sqlPath)` returns `TRUE`.
#'
#' @return
#' An updated rawBase object with `sql_paths` modified and deduplicated.
#'
#' @export
raw.addSQLpath <- function(rawBase, sqlPath) {
  # update SQL paths
  if (!is.null(sqlPath)) {
    if (dir.exists(sqlPath)) {
      rawBase$sql_paths <- unique(c(rawBase$sql_paths, sqlPath))
      rawBase <- raw.initDB(rawBase)
    }
  }

  # Persist updated object ---------------------------------------------------
  if (interactive()) {
  usethis::ui_silence(
    usethis::use_data(rawBase, overwrite = TRUE)
  )
  }

  rawBase
}

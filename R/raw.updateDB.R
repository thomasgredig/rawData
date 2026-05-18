#' Update or initialize the RAW SQLite database
#'
#' Updates an existing SQLite database associated with `rawBase`, or creates and
#' initializes a new database if one does not already exist.
#'
#' @param rawBase A rawBase object containing one or more SQL database paths in
#'   `rawBase$sql_paths`.
#' @param quiet Logical. If `TRUE`, suppresses progress or informational output
#'   passed to downstream database update or initialization functions. Defaults
#'   to `FALSE`.
#'
#' @return The updated `rawBase` object. If no SQL path is set, returns `rawBase`
#'   unchanged after issuing a warning.
#'
#' @details
#' If `rawBase$sql_paths` is empty, the function warns and returns `rawBase`
#' unchanged. If a SQLite database already exists, [update_databaseName()] is
#' called to update it to the current database format if needed. Otherwise,
#' [raw.initDB()] is called to create and populate a new database.
#'
#' @seealso [sql_database_exists()], [update_databaseName()], [raw.initDB()]
#'
#' @examples
#' \dontrun{
#' rawBase <- raw.updateDB(rawBase)
#' rawBase <- raw.updateDB(rawBase, quiet = TRUE)
#' }
#'
#' @export
raw.updateDB <- function(rawBase, quiet = FALSE) {
  if (length(rawBase$sql_paths) == 0L) {
    warning("SQL Path not set in rawBase.")
    return(rawBase)
  }

  # check whether there is an SQL database already
  if (sql_database_exists(rawBase)) {
    if (!"afmHistory" %in% names(rawBase)) {
      # rawBase$afmHistory <- nanoAFMr::get
    }

    # update to current version, if necessary
    update_databaseName(rawBase, quiet = quiet)
  } else {
    # if SQL DB is not found, then create it and populate it, if possible
    raw.initDB(rawBase, quiet = quiet)
  }

  # return updated rawBase
  rawBase
}

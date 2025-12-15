#' Update the SQLite database or re-generate it
#'
#' @export
raw.updateDB <- function(rawBase, quiet=FALSE) {
  if (length(rawBase$sql_paths)==0L) {
    warning("SQL Path not set in rawBase.")
    return(rawBase)
  }

  # check whether there is an SQL database already
  if (sql_database_exists(rawBase)) {
    if (!"afmHistory" %in% names(rawBase)) {
      rawBase$afmHistory <- nanoAFMr::get
    }

    # update to current version, if necessary
    update_databaseName(rawBase,quiet=quiet)
  } else {
    # if SQL DB is not found, then create it and populate it, if possible
    raw.initDB(rawBase)
  }
  # return updated rawBase
  rawBase
}
rawBase$import_history = update_rawBaseHistory(rawBase$import_history,
                                               "add",
                                               rawBase$project,
                                               path,
                                               recursive)

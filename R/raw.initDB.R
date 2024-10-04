#' Initialize SQL database
#'
#' @param rawBase list created by raw.init()
#'
#' @importFrom DBI dbConnect dbDisconnect
#' @importFrom nanoAFMr AFM.writeDB
#'
#' @export
raw.initDB <- function(rawBase) {
  # generate filename
  sqlFileName = .getSQLdbName(rawBase$pkgName)

  # is there an SQL path, then choose the first one that exists:
  sqlPath = ""
  for(p in rawBase$sqlPaths) {
    if (dir.exists(p)) { sqlPath = p; break }
  }

  # put in the same path as previous one, if there is one.
  dbName = raw.getDatabase(rawBase, verbose=FALSE)
  if (nchar(dbName)>0) { sqlPath = basename(dbName) }

  # if no path was found, then prompt for one:
  if (sqlPath=="") {
    sqlPath = .promptPath("Enter SQL path:")
  }
  dbFilename = file.path(sqlPath, sqlFileName)
  if (file.exists(dbFilename)) stop("Database file already exists.")

  mydb <- DBI::dbConnect(RSQLite::SQLite(), dbFilename)
  nanoAFMr::AFM.writeDB(NULL, mydb, 1, verbose=FALSE)
  DBI::dbDisconnect(mydb)
}

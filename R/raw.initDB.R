#' Initialize SQL database
#'
#' @param rawBase object, use create_rawBase()
#' @param verbose logical to display additional information
#'
#' @importFrom DBI dbConnect dbDisconnect
#' @importFrom RSQLite SQLite
#'
#' @export
raw.initDB <- function(rawBase, verbose=TRUE) {
  if (!is(rawBase,"rawBase")) stop("rawBase oject required.")
  # generate filename
  # sqlFileName = .getDatabaseFileName(rawBase$package_name)
  # if (verbose) cat("SQL file:", sqlFileName, "\n")
  #
  # # is there an SQL path, then choose the first one that exists:
  # sqlPath = ""
  # for(p in rawBase$sqlPaths) {
  #   if (dir.exists(p)) { sqlPath = p; break }
  # }
  # if (verbose) cat("SQL path:", sqlPath, "\n")

  # put in the same path as previous one, if there is one.
  dbName = .getDatabaseName(rawBase)
  # if (nchar(dbName)>0) { sqlPath = dirname(dbName) }
  #
  # # if no path was found, then prompt for one:
  # if (interactive()) {
  #   if (sqlPath=="") {
  #     sqlPath = .promptPath("Enter SQL path:")
  #   }
  # }
  # dbFilename = file.path(sqlPath, sqlFileName)

  if (file.exists(dbName)) {
    # warning("Database file already exists:", dbName)
  } else {
    if (verbose) cat("Creating new database:", dbName, "\n")
    mydb <- dbConnect(RSQLite::SQLite(), dbName)
    .writeSQLdatabaseInit(mydb)
    .updateSQLhistory(mydb, rawBase$token, "init")
    dbDisconnect(mydb)
  }

}

#' Returns SQLite table names
.getSQLtableNames <- function() {
  list(
    tblNameAFM = paste0('afmData'),
    tblNameHistory = paste0('sqlHistory')
  )
}


#' writes a SQLite database initialization
#' @param mydb database connection from DBI::dbConnect
#' @importFrom DBI dbCreateTable
#' @export
.writeSQLdatabaseInit <- function(mydb, verbose=FALSE) {
  tbl <- .getSQLtableNames()
  dfAFM_empty = data.frame(ID = integer(),
                       channel = character(),
                       x.conv = integer(),
                       y.conv = integer(),
                       x.pixels = integer(),
                       y.pixels = integer(),
                       z.units = character(),
                       instrument = character(),
                       history = character(),
                       date = character(),
                       description = character(),
                       fullFilename = character())
  dbCreateTable(mydb, tbl$tblNameAFM, dfAFM_empty)
  if (verbose) print(paste("Created data table:",tbl$tblNameAFM))

  dfhist_empty = data.frame(ID = integer(),
                           token = numeric(),
                           date = character(),
                           version = character(),
                           description = character())
  dbCreateTable(mydb, tbl$tblNameHistory, dfhist_empty)
  if (verbose) print(paste("Created data table:",tbl$tblNameHistory))

  invisible(TRUE)
}

#' reads the sqlHistory table
#' @param mydb database connection from DBI::dbConnect
#' @importFrom DBI dbReadTable
#' @export
.readSQLhistory<- function(mydb) {
  tbl <- .getSQLtableNames()
  DBI::dbReadTable(mydb, tbl$tblNameHistory)
}

#' Prints the history of the SQLite database
#' @importFrom DBI dbConnect dbDisconnect
#' @importFrom RSQLite SQLite
#' @export
raw.showHistoryDB <- function(rawBase) {
  dbFilename = raw.getDatabase(rawBase)
  print(paste("DB name:",dbFilename))
  if (file.exists(dbFilename)) {
    mydb <- dbConnect(RSQLite::SQLite(), dbFilename)
    tbl <- .readSQLhistory(mydb)
    dbDisconnect(mydb)
    print(tbl)
  }
}

#' updates the sqlHistory table
#' @param mydb database connection from DBI::dbConnect
#' @importFrom DBI dbWriteTable
.updateSQLhistory <- function(mydb, token, description) {
  tbl <- .getSQLtableNames()
  tblHist <- .readSQLhistory(mydb)
  if (nrow(tblHist)>0) { ID = max(tblHist$ID)+1 } else { ID = 1 }
  new_row <- data.frame(ID = ID,
                        token = token,
                        date = format(Sys.Date(), "%Y-%m-%d"),
                        version = 0,
                        description = description)

  dbWriteTable(mydb, tbl$tblNameHistory,
               new_row, append = TRUE, row.names = FALSE)
}

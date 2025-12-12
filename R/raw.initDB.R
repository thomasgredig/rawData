#' Initialize SQL database
#'
#' Create and initialize a database, if none exists. The SQLite
#' database can store images and large data files, which would
#' otherwise slow down the R package. The database contains the
#' name of the package, includes the version number and ends
#' with .sqlite. Before creating a new database, it checks whether
#' a database already exists; it might have an older version. If
#' so, then it updates (renames) the file to correspond to the
#' latest version. The rawBase object stores information to what
#' is stored in this separate database, in case it gets lost or
#' needs to be recreated.
#'
#' @param rawBase object, use create_rawBase()
#' @param verbose logical to display additional information
#'
#' @importFrom DBI dbConnect dbDisconnect
#' @importFrom RSQLite SQLite
#' @importFrom cli cli_inform
#'
#' @export
raw.initDB <- function(rawBase, verbose=TRUE) {
  check_rawBase(rawBase) # make sure, it is a valid object

  if (sql_database_exists(rawBase)) {
    cli_inform("SQL database already exists; no new database generated, updated if needed.")
    # if needed, update the version in the name of the database.
    update_databaseName(rawBase)
    return(rawBase)
  }

  dbName = get_newSQLname(rawBase)

  if (verbose) cat("Creating new database:", dbName, "\n")
  if (file.exists(dbName)) {
    warning("Cannot create SQLite database, because it already exists.")
    return(rawBase)
  }
  mydb <- dbConnect(RSQLite::SQLite(), dbName)
  .writeSQLdatabaseInit(mydb)
  .updateSQLhistory(mydb, rawBase$token, "init")
  dbDisconnect(mydb)

  return(rawBase)
}

# check if database already exists, must have
# exact match with version
sql_database_exists <- function(rawBase) {
  dbName = .getDatabaseName(rawBase, include_oldVersions = TRUE)
  if(is.null(dbName)) return(FALSE)
  file.exists(dbName)
}

# update the database name if needed
update_databaseName <- function(rawBase) {
  # find the latest SQL version
  dbOldVersion = .getDatabaseName(rawBase, include_oldVersions = TRUE)
  # find the current name of the SQL database
  dbNewVersion = file.path(dirname(dbOldVersion),.getDatabaseFileName(rawBase$package_name))

  if (dbOldVersion != dbNewVersion) {
    # update old version to new version
    dbNewVersion = file.path(dirname(dbOldVersion), basename(dbNewVersion))
    file.rename(from=dbOldVersion,
                to = dbNewVersion)
  }
}

# returns the new SQL database name:
# - first forms the name (using the new version)
# - finds an existing directory
get_newSQLname <- function(rawBase) {
  sql_filename = .getDatabaseFileName(rawBase$package_name)
  sql_path = ""
  for(p in rawBase$sql_paths) {
    if (dir.exists(p)) {
      sql_path = p
      break
    }
  }
  sql_fullname = file.path(sql_path, sql_filename)
  # this should not happen, but just checking
  if (file.exists(sql_fullname)) warning("SQL file already exists; error in get_newSQLname() function.")
  sql_fullname
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
#' @param verbose logical to output extra information
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
#' @param token a number representing the time
#' @param description string with description of update
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


get_highest_version_file <- function(file_paths) {
  check_sql_file_paths(file_paths)
  # Extract version numbers from file paths
  extract_version <- function(file_path) {
    version <- sub(".*-([0-9\\.]+)\\.sqlite$", "\\1", file_path)
    as.numeric_version(version)
  }
  version2num <- function(x_vec) {
    v = 0
    mfactor = 10^9
    for(x in x_vec) {
      v = v+x*mfactor
      if(mfactor>100) mfactor=mfactor/100
    }
    v
  }

  # Apply the extract_version function to all file paths
  versions <- sapply(file_paths, extract_version)
  v_num = sapply(versions, version2num)

  # Find the index of the highest version
  highest_version_index <- which.max(v_num)

  # Return the file path with the highest version
  file_paths[highest_version_index]
}

# check that the files look like c("db-0.2.3.sqlite","db-0.2.1.sqlite")
check_sql_file_paths <- function(file_paths) {
  if(!inherits(file_paths,"character")) stop("Must provide a character string to check version.")
  if(!any(grepl("sqlite$",file_paths) == TRUE)) stop("All files must be SQLite database files.")
}

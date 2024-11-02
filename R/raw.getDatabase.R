#' Manage SQL Database with Data Package
#'
#' @description
#' Some data packages require large amounts of data (1 - 10GB), which cannot be
#' stored in the `extdata` folder directly, since the install would take too long.
#' If data exceeds about 10 MB, then RDA files become inefficient, and an SQL
#' database makes sense. For AFM data, you can use AFM.writeDB() for example.
#'
#' The database needs to be stored in the `extdata` and also needs to be version
#' controlled. This function helps manage this process. The data package generates
#' the small RDA data files and puts the large data files into the SQL databse
#' that is stored in the main directory of the database. The database needs to store
#' at least one more dummy file in the `inst/extdata` folder, so that this folder
#' is generated and loaded.
#'
#' When called with the `pkgname`, the function uses the version to generate the
#' database filename and return its path.
#'
#' @param rawBase rawBase object
#' @param verbose logical, additional information
#'
#' @importFrom utils packageVersion
#'
#' @return SQL database filename and path
#'
#' @export
raw.getDatabase <- function(rawBase, verbose = FALSE) {
  check_rawBase(rawBase)

  dbFolderListFile <- rawBase$sql_paths
  dbFile <- NULL

  if (!nzchar(system.file(package = rawBase$package_name))) {
    if (verbose) cat("Package", rawBase$package_name,"not installed.")
    return (NULL)
  }
  dbFileName = .getDatabaseFileName(rawBase$package_name)
  if (verbose) cat("SQL Database filename:", dbFileName,"\n")

  # not all search paths might exists, so check which exists
  dbSearchPaths = c()
  for(dbFolder in rawBase$sql_paths) {
    if (verbose) cat("Searching folder:", dbFolder,"\n")
    if (dir.exists(dbFolder)) dbSearchPaths = c(dbSearchPaths, dbFolder)
  }

  if (verbose) cat("Searching", length(dbSearchPaths), "folders.\n")
  if (length(dbSearchPaths)==0) {
    warning("No path for SQL repository found.")
    return("")
  }

  dbFile = ""
  for(pfad in dbSearchPaths) {
    dbFileCheck <- file.path(pfad,dbFileName)
    if (file.exists(dbFileCheck)) { dbFile <- dbFileCheck } else {
      dbFileNameAlternative = dir(pfad, pattern=paste0(rawBase$package_name,'.*sqlite$'))
      if (length(dbFileNameAlternative)>0) dbFile <- file.path(pfad, dbFileNameAlternative[1])
    }
  }

  if (verbose) {
    if (file.exists(dbFile)) cat("SQL DB size:", round(file.info(dbFile)$size/1024/1024,1),"MB\n")
  }

  dbFile
}

# Helper functions
NULL

# if include_oldVersions is TRUE, then other versions of the database are also returned
.getDatabaseName <- function(rawBase, include_oldVersions = FALSE) {
  sqlPath = ""
  sqlFile = .getDatabaseFileName(rawBase$package_name)
  sqlPattern = .getDatabaseFileName(rawBase$package_name, pattern=TRUE)
  sql_files = c()

  for(sqlPath in rawBase$sql_paths) {
    if(dir.exists(sqlPath)) {
      # check for exact match
      if (file.exists(file.path(sqlPath,sqlFile))) {
        sql_files = c(sql_files, file.path(sqlPath, sqlFile))
      } else {
        if (include_oldVersions) {
          # check for other versions in the same folder
          fList = dir(sqlPath, pattern = sqlPattern)
          if (length(fList) != 0L) {
            sql_files = c(sql_files, file.path(sqlPath, fList))
          }
        }
      }
    }
  }

  if(length(sql_files)>0L) { sql_files= get_highest_version_file(sql_files) }
  sql_files
}

#' get full path of SQL database
#'
#' if pattern is TRUE, then use * for version, otherwise match current
#'  version
#' @param pkgName name of package
#' @param pattern \code{logical} whether all versions are allowed
#' @importFrom utils packageVersion
#' @noRd
.getDatabaseFileName <- function(pkgName, pattern=FALSE) {
  pkgVersion = "0.0.0"
  if (pkgName != "tests") {
    pkgVersion <- if (pattern) "*" else as.character(packageVersion(pkgName))
  }
  paste0(pkgName,"-",pkgVersion,".sqlite")
}

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
#' @param pkgname name of the R data package
#' @param dbPath path
#' @param verbose logical, additional information
#'
#' @importFrom utils packageVersion
#'
#' @return SQL database filename and path
#'
#' @export
raw.getDatabase <- function(rawBase, verbose = FALSE) {
  dbFolderListFile <- rawBase$sqlPaths
  dbFile <- NULL

  if (!nzchar(system.file(package = rawBase$pkgName))) {
    if (verbose) cat("Package", rawBase$pkgName,"not installed.")
    return (NULL)
  }
  dbFileName = .getSQLdbName(rawBase$pkgName)
  if (verbose) cat("SQL Database filename:", dbFileName,"\n")

  # not all search paths might exists, so check which exists
  dbSearchPaths = c()
  for(dbFolder in rawBase$sqlPaths) {
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
      dbFileNameAlternative = dir(pfad, pattern=paste0(rawBase$pkgName,'.*sqlite$'))
      if (length(dbFileNameAlternative)>0) dbFile <- file.path(pfad, dbFileNameAlternative[1])
    }
  }

  if (verbose) {
    if (file.exists(dbFile)) cat("SQL DB size:", round(file.info(dbFile)$size/1024/1024,1),"MB\n")
  }

  dbFile
}


.getSQLdbName <- function(pkgName) {
  pkgVersion = as.character(packageVersion(pkgName))
  paste0(pkgName,"-",pkgVersion,".sqlite")
}

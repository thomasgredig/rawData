#' Constructor of rawBase S3 class
#'
#' @param projectName project name
#' @param pkgName name of the package
#' @param paths path or paths with data files
#' @param recursive logical weather to search paths recursively
#' @param sqlPaths paths for location of SQL database
#' @param instrument_list vector with instruments to be updated
#' @param legacyRAWIDfile full path and file name of RAW-ID.csv file
#' @param ext any extensions as a list
#'
#' @importFrom here here
#'
#' @export
create_rawBase <- function(projectName,
                           pkgName = NULL,
                           paths = NULL,
                           recursive = TRUE,
                           sqlPaths = NULL,
                           instrument_list = NULL,
                           legacyRAWIDfile = NULL,
                           extensions = c()) {
  .getToken <- function() { as.numeric(Sys.time()) }

  if (is.null(pkgName)) pkgName = basename(here::here())

  projLen = nchar(projectName)
  if (projLen<=2 | projLen>=10) {
    warning("projectName is too short or too long.")
  }

  # prompt for data and SQL path
  if (interactive()) {
    if (is.null(paths)) {
      paths = .promptPath("Enter path with RAW data: ")
      if (!dir.exists(paths)) { warning("RAW path not found."); paths = NULL }
    }
    if (is.null(sqlPaths)) {
      sqlPaths = .promptPath("Enter path for SQL repository: ")
      if (!dir.exists(sqlPaths)) { warning("SQL path not found."); sqlPaths = NULL }
    }
    if (is.null(legacyRAWIDfile)) {
      legacyRAWIDfile = .promptPath("Enter path for legacy RAW ID file: ")
      if (!dir.exists(legacyRAWIDfile)) { warning("Legacy path not found."); sqlPaths = NULL }
    }
  }

  # default paths
  if (is.null(paths)) paths = "."
  if (is.null(sqlPaths)) sqlPaths = "."
  if (is.null(legacyRAWIDfile)) legacyRAWIDfile = ""

  rawBase = list(
    dataRAW = data.frame(),
    project_name = projectName,
    package_name = pkgName,
    raw_paths = paths,
    import_history = create_rawBaseHistory(
      action = "add",
      project = projectName,
      path = paths,
      recursive = recursive),
    sql_paths = sqlPaths,
    legacyRAWIDfile = legacyRAWIDfile,
    extensions = extensions,
    token = .getToken()
  )

  # instruments, should be a list
  raw.addInstrument(rawBase, instrument_list)

  # Assign the class attribute
  class(rawBase) <- "rawBase"

  rawBase
}

#' print rawData base object
#' @importFrom cli cli_alert_danger cli_alert_success
#' @export
print.rawBase <- function(rawBase, ...) {
  dbName = .getDatabaseName(rawBase)
  dataRAW = as.data.frame(rawBase$dataRAW)
  cat("Project .........:",paste(rawBase$project_name,collapse=" :: "),"\n")
  cat("Package .........:",rawBase$package_name,"\n")
  cat("RAW paths .......:",paste(rawBase$raw_paths,collapse=" :: "),"\n")
  cat("Project names ...:",unique(rawBase$import_history$project),"\n")
  cat("SQL paths .......:",paste(rawBase$sql_paths,collapse=" :: "),"\n")
  cat("SQL database ....:",dbName,"\n")
  cat("Instruments .....:",paste(names(rawBase$instruments), collapse=", "),"\n")
  cat("RAW data files ..:",nrow(dataRAW)," (",length(which(rawBase$dataRAW$missing==TRUE)),"missing)\n")
  cat("Extensions ......:",paste0(rawBase$extensions, collapse = ", "),"\n")

  if (file.exists(raw.getDatabase(rawBase))) {
    cli::cli_alert_success("Success finding SQL database.")
  } else {
    cli::cli_alert_danger("Failed finding SQL database.")
    cli::cli_alert_info("Use raw.addSQLpath(rawBase, p) to add local path p for DB")
  }
}


create_rawBaseHistory <- function(action,project,path,recursive) {
  if(is.null(project)) project = ""
  if(is.null(path)) path = ""

  rawBaseHistory = data.frame(
    action = action,
    project = project,
    path = path,
    recursive = recursive
  )
  rawBaseHistory
}

update_rawBaseHistory <- function(rh,action,project,path,recursive) {
  if(is.null(project)) { project = rh$project[nrow(rh)] }  # inherit previous
  if(is.null(path)) { path= rh$path[nrow(rh)] } # inherit previous
  rh_new = create_rawBaseHistory(action,project,path,recursive)
  if(similar_rawBaseHistory(rh, rh_new)) return(rh)
  dplyr::bind_rows(rh, rh_new)
}

# checks whether rh_new is already similar to rh_vec somewhere
similar_rawBaseHistory <- function(rh_vec, rh_new) {
  matchFound = FALSE
  q = which(rh_new$path == rh_vec$path)
  if (length(q)>0) {
    rh_vec = rh_vec[q,]
    q1 = which(rh_new$project == rh_vec$project)
    if (length(q1)>0) matchFound = TRUE
  }
  matchFound
}

# check of whether the rawBase is valid
#' @importFrom cli cli_abort
#' @noRd
check_rawBase <- function(rawBase) {
  if (!is(rawBase,"rawBase")) cli_abort("rawBase object required.")
}

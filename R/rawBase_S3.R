#' Constructor of rawBase S3 class
#'
#' @importFrom here here
#'
#' @param ID unique ID for file
create_rawBase <- function(projectName,
                           pkgName = NULL,
                           paths = NULL,
                           recursive = TRUE,
                           sqlPaths = NULL,
                           legacyRAWIDfile = NULL) {
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
    raw_recursive = recursive,
    sql_paths = sqlPaths,
    legacyRAWIDfile = legacyRAWIDfile,
    token = .getToken()
  )

  # Assign the class attribute
  class(rawBase) <- "rawBase"

  rawBase
}

#' print rawData base object
#' @importFrom cli cli_alert_danger cli_alert_success
#' @export
print.rawBase <- function(rawBase, ...) {
  dataRAW = as.data.frame(rawBase$dataRAW)
  cat("Project .........:",rawBase$project_name,"\n")
  cat("Pacakge .........:",rawBase$package_name,"\n")
  cat("RAW paths .......:",rawBase$raw_paths[1],"\n")
  cat("SQL paths .......:",rawBase$sql_paths[1],"\n")
  cat("RAW data files ..:", nrow(dataRAW),"\n")

  if (file.exists(raw.getDatabase(rawBase))) {
    cli_alert_success("Success finding SQL database.")
  } else {
    cli::cli_alert_danger("Failed finding SQL database.")
  }
}

.getToken <- function() { as.numeric(Sys.time()) }

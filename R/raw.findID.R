#' Given a string that represents a partial filename or component, returns the ID
#'
#' @export
raw.findID <- function(rawBase, str) {
  check_rawBase(rawBase)

  if (missing(str) || is.null(str) || length(str) != 1L || is.na(str)) {
    stop("`str` must be a single non-NA character string.", call. = FALSE)
  }

  if (!is.character(str)) {
    stop("`str` must be a character string.", call. = FALSE)
  }

  # Data frame with all files
  df <- as.data.frame(rawBase$dataRAW)

  if (nrow(df) == 0L) {
    return(df[0, , drop = FALSE])
  }

  idx <- grep(str, df$filename)

  if (length(idx) == 0L) {
    return(df[0, , drop = FALSE])
  }

  df[idx, , drop = FALSE]
}

#' Find RAW data files by partial filename
#'
#' Given a character string representing a partial filename or filename
#' component, returns the rows in `rawBase$dataRAW` whose `filename` column
#' matches that string.
#'
#' @param rawBase A rawBase object. Must pass [check_rawBase()].
#' @param str A single non-`NA` character string used as the search pattern.
#'
#' @return A data frame containing matching rows from `rawBase$dataRAW`. If no
#'   files match, or if `rawBase$dataRAW` is empty, returns an empty data frame
#'   with the same columns as `rawBase$dataRAW`.
#'
#' @details
#' Matching is performed with [grep()] against the `filename` column of
#' `rawBase$dataRAW`, so `str` is interpreted as a regular expression.
#'
#' @examples
#' \dontrun{
#' raw.findID(rawBase, "sample_001")
#' raw.findID(rawBase, "\\.dat$")
#' }
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

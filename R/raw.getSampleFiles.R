#' Provide sample RAW data files
#'
#' Creates a temporary sample RAW text file for examples, tests, or demonstrations.
#'
#' @return A character string giving the full path and filename of the sample data file.
#'
#' @examples
#' sample_file <- raw.getSampleFiles()
#' file.exists(sample_file)
#'
#' @export
raw.getSampleFiles <- function() {
  tmpDir = tempdir()
  filename = file.path(tmpDir,"20241001_spinPc_TG_text_Sample.txt")
  writeLines("Text Test File 1", filename)

  filename
}

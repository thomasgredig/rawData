#' provide sample RAW data files
#' @returns path and filename with sample data
raw.getSampleFiles <- function() {
  tmpDir = tempdir()
  filename = file.path(tmpDir,"20241001_spinPc_TG_text_Sample.txt")
  writeLines("Text Test File 1", filename)

  filename
}

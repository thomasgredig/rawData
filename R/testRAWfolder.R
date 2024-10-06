#' Create a Test RAW folder with text data files
#' @param n number of files
#' @param projectName string for name of the project
#' @param returns folder with temporary files
#' @examples
#' dir(get_test_RAW_folder(2,"spinPc"))
#' @export
get_test_RAW_folder <- function(n, projectName) {
  tmpDir = tempdir()

  for(i in 1:n) {
    year = as.character(floor(runif(1,2010,2024.9)))
    month = paste0("0",floor(runif(1,1,9.9)))
    day = as.character(floor(runif(1,10,29.9)))
    randNum = as.character(floor(runif(1,0,100000)))
    if (runif(1)<0.5) { user = "SC" } else { user = "LN" }
    temp_file <- file.path(tmpDir, paste0(year,month,day,"_",
                                          projectName,"_",user,"_text_Sample",
                                          randNum,".txt"))
    writeLines(c(paste("This is test file number:",i),
                 paste("Random number:",runif(1))),
               temp_file)
  }

  tmpDir
}

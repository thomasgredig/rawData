#' Constructor of dataRAW S3 class
#' @export
create_dataRAW <- function(ID,
                           pRAW,
                           path_filename,
                           crc = NULL,
                           size = NULL,
                           type = NULL,
                           missing = NULL,
                           altered = NULL,
                           sample = NULL,
                           date = NULL,
                           meta=NULL) {
  nLen = length(ID)
  f = path_filename
  # do some checks
  if (is.null(meta)) {
    meta <- rep("", nLen)
  }

  # strip out common path pRAW
  fPath = gsub(pRAW,"",dirname(f))

  # Create a data.frame
  df = data.frame(ID,
                  path = fPath,
                  filename = basename(f),
                  crc = .getCRC(f),
                  size = file.info(f)$size,
                  type = .getFileType(f),
                  missing = !file.exists(f),
                  altered = rep(FALSE,nLen),
                  sample = rep("",nLen),
                  date = format(file.info(f)$atime),
                  meta = rep("",nLen)
  )

  # Assign the class attribute
  class(df) <- "dataRAW"

  return(df)
}


#' row bind two dataRAW sets
#' @param d1 first dataRAW object
#' @param d2 second dataRAW object to be appended
#' @export
rbind.dataRAW <- function(d1, d2) {
  if (min(d2$ID) <= max(d1$ID)) {
    # IDs in d2 are too low and overlap; move IDs up
    d2$ID = d2$ID + (min(d2$ID) - max(d1$ID) + 7)
    warning("IDs are shifted to row bind dataRAW.")
  }


  # Get the names of all variables in both lists
  all_names <- unique(c(names(d1), names(d2)))
  # Loop through each variable name
  for (name in all_names) {
    d1[[name]] <- c(d1[[name]], d2[[name]])
  }

  d1
}

#' Print method for the dataRAW class
#' @param x created with create_dataRAW() function
#' @param ... additional params
#' @importFrom utils head tail
#' @export
print.dataRAW <- function(x, ...) {
  print("dataRAW info on",length(x$ID),"files:\n")
  df = data.frame(x$ID, x$filename, x$size, x$type, x$sample)
  print(head(df,...))
  if (nrow(df)>10) {
    print("...\n")
    print(tail(df,...))
  }
}

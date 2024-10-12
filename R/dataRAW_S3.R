#' Constructor of dataRAW S3 class
#' @export
create_dataRAW <- function(ID,
                           filename,
                           crc = NULL,
                           size = NULL,
                           type = NULL,
                           missing = NULL,
                           altered = NULL,
                           sample = NULL,
                           date = NULL,
                           meta=NULL) {
  nLen = length(filename)  # number of files to add
  # assert that length of IDs and filenames are the same
  if(length(ID) != nLen) {
    # extend IDs or crop IDs
    if (length(ID) < nLen) {
      ID = c(ID, seq(max(ID)+1, max(ID)+nLen-length(ID) ))
    } else {
      ID = ID[1:nLen]
    }
  }

  if(is.null(crc)) {crc = .getCRC(filename) }
  if(is.null(size)) { size = file.info(filename)$size }
  if(is.null(type)) { type = sapply(filename, .getFileType) }
  if(is.null(missing)) { missing = !file.exists(filename) }
  if(is.null(altered)) { altered = rep(FALSE,nLen) }
  if(is.null(sample)) { sample = sapply(basename(filename), .getSampleName) }
  if(is.null(date)) { date = format(file.info(filename)$atime) }
  if(is.null(meta)) { meta = rep("",nLen) }

  # strip out common path pRAW
  pRAW = .commonPath(filename)
  fPath = gsub(pRAW,"",dirname(filename))

  # create basic dataframe
  df = data.frame(
    ID = ID,
    path = fPath,
    filename = basename(filename),
    crc = crc,
    size = size,
    type = type,
    missing = missing,
    altered = altered,
    sample = sample,
    date = date,
    meta = meta
  )

  dataRAW <- list(
    df = df,
    pRAW = pRAW
  )

  # Assign the class attribute
  class(dataRAW) <- "dataRAW"

  return(dataRAW)
}


#' row bind two dataRAW sets
#' @param d1 first dataRAW object
#' @param d2 second dataRAW object to be appended
#' @export
rbind.dataRAW <- function(d1, d2) {
  if (min(d2$df$ID) <= max(d1$df$ID)) {
    # IDs in d2 are too low and overlap; move IDs up
    d2$df$ID = d2$df$ID + (min(d2$df$ID) - max(d1$df$ID) + 7)
  }

  df1 = d1$df
  df2 = d2$df
  m <- which(df2$crc %in% df1$crc)
  df2 <- df2[-m,]
  if (nrow(df2)==0) return(d1)
  next_ID = max(df1$ID)
  df2$ID = 1:nrow(df2) + next_ID
  d1$df = rbind(df1,df2)
  d1$pRAW = c(d1$pRAW, d2$pRAW)

  d1
}

#' Print method for the dataRAW class
#' @param RAW created with create_dataRAW() function
#' @param ... additional params
#' @importFrom utils head tail
#' @export
print.dataRAW <- function(RAW, row.names = FALSE, ...) {
  print(paste("dataRAW info on",length(RAW$ID),"files:"))
  df = data.frame(RAW$ID, RAW$filename, RAW$size, RAW$type, RAW$sample)
  print(df, row.names = row.names, ...)
}

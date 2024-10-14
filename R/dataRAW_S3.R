#' Constructor of dataRAW S3 class
#'
#' @param ID unique ID for file
#' @param filename filename including path
#' @param crc 128-bit MD5 unique hash
#' @param size file size in bytes
#' @param type data type of file
#' @param missing logical, file cannot be found
#' @param altered logical, file likely been altered
#' @param sample string of sample name
#' @param date date for data recording
#' @param meta additional data in JSON format (use jsonlite)
#'
#' @importFrom desc desc_get_version
#'
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
  # number of files to add
  nLen = length(filename)

  # assert that length of IDs and file names are the same
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

  # create basic data frame
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

  dataRAW <- df
  # list(
  #   df = df,
  #   version = desc::desc_get_version(),
  #   pRAW = pRAW
  # )

  # Assign the class attribute
  class(dataRAW) <- "dataRAW"

  return(dataRAW)
}

#' @export
as.data.frame.dataRAW <- function(d,...) {
  data.frame(
    ID = d$ID,
    path = d$path,
    filename = d$filename,
    crc = d$crc,
    size = d$size,
    type = d$type,
    missing = d$missing,
    altered = d$altered,
    sample = d$sample,
    date = d$date,
    meta = d$meta
  )
}



#' row bind two dataRAW sets
#' @param d1 first dataRAW object
#' @param d2 second dataRAW object to be appended
#' @importFrom methods is
#' @export
rbind.dataRAW <- function(d1, d2) {
  # both objects should be dataRAW
  if (!is(d1,"dataRAW")) stop("dataRAW 1 object required.")
  if (!is(d2,"dataRAW")) stop("dataRAW 2 object required.")

  df1 = as.data.frame(d1)
  df2 = as.data.frame(d2)
  m <- which(df2$crc %in% df1$crc)
  df_dupl = df2[m,]
  if(length(m)>0) df2 <- df2[-m,]

  if (nrow(df2)==0) return(d1)

  next_ID = max(df1$ID)
  df2$ID = 1:nrow(df2) + next_ID
  df3 = rbind(df1,df2)

  class(df3) <- "dataRAW"
  df3
}

#' Print method for the dataRAW class
#' @param RAW created with create_dataRAW() function
#' @param ... additional params
#' @importFrom utils head tail
#' @export
print.dataRAW <- function(d, row.names = FALSE, ...) {
  print(paste("dataRAW info on",length(d$ID),"files:"))
  df = data.frame(d$ID, d$filename, d$size, d$type, d$sample)
  print(df, row.names = row.names, ...)
}

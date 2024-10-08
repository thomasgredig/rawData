#' Imports legacy RAW-ID.csv
#'
#' @export
raw.importRAWID <- function(filename_RAWID) {
  # read the header information
  df_header <- raw.readRAWIDheader(filename_RAWID)
  p = ""
  for(p in df_header$paths) {
    if(dir.exists(p)) pRAW = p
  }

  # read all the data
  df_raw = read.csv(filename_RAWID, comment.char = "#")

  for(i in 1:nrow(df_raw)) {
    df = df_raw[i,]
    # cat("ID",df$ID," .. ")
    filename = file.path(df$path, df$filename)
    create_dataRAW(df$ID,
                   pRAW,
                   filename,
                   crc = df$crc,
                   size = df$size,
                   type = df$type,
                   df$missing,
                   df$altered,
                   sample = df$sample,
                   df$date,
                   df$meta)  -> dRaw
    if(exists("dataRAW")) { dataRAW = rbind(dataRAW, dRaw) } else {dataRAW = dRaw }
  }
  dataRAW
}






#' Reads an RAW ID header
#'
#' Header information is presided by # and separated name:value with colon
#' for example # Version: 1.0
#'
#' @param fIDfile path and file name for RAW-ID.csv file
#'
#' @importFrom utils read.csv
#'
#' @export
raw.readRAWIDheader <- function(fIDfile) {
  # return empty path if RAW ID file is not found.
  if (!file.exists(fIDfile)) {
    header = list(path="")
    return(header)
  }

  df <- readLines(con <- file(fIDfile))
  close(con)
  h <- df[grep("^#",df)]
  if (is.integer(h)) return(list(version="0.1", path=""))

  h <- trimws(gsub('^#','',h))
  header = setNames(as.list(trimws(sapply(strsplit(h,": "),'[[',2 ))),
                    trimws(sapply(strsplit(h,": "),'[[',1 )))

  # un-serialize vectors
  for(keys in names(header)) {
    v = header[[keys]]
    if (grepl(";",v)) {
      header[[keys]] <- strsplit(v,";")[[1]]
    }
  }

  header
}


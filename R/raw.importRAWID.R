#' Imports legacy RAW-ID.csv
#'
#' @param filename_RAWID filename and path for RAW-ID.csv file in data-raw folder
#'
#' @export
raw.importRAWID <- function(filename_RAWID) {
  p = ""
  dataRAW = NULL

  # read the header information
  df_header <- raw.readRAWIDheader(filename_RAWID)

  for(p in df_header$paths) {
    if(dir.exists(p)) pRAW = p
  }

  # read all the data
  df = read.csv(filename_RAWID, comment.char = "#")
  create_dataRAW(df$ID,
                 file.path(pRAW, filename),
                 crc = df$crc,
                 size = df$size,
                 type = df$type,
                 df$missing,
                 df$altered,
                 sample = df$sample,
                 df$date,
                 df$meta)  -> dRaw

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


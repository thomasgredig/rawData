#' Imports legacy RAW-ID.csv
#'
#' @param filename_RAWID filename and path for RAW-ID.csv file in data-raw folder
#'
#' @export
raw.importRAWID <- function(rawBase) {
  if (!is(rawBase,"rawBase")) stop("rawBase oject required.")
  if (!file.exists(rawBase$legacyRAWIDfile)) return(rawBase)
  if (nrow(rawBase$dataRAW)>0) stop("dataRAW must be empty to add legacy IDs")

  # read the header information
  df_header <- raw.readRAWIDheader(rawBase$legacyRAWIDfile)
  rawBase$raw_paths =  df_header$paths
  rawBase$raw_recursive = rep(TRUE, length(df_header$paths))

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
                 df$meta)  -> rawBase$dataRAW

  rawBase
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


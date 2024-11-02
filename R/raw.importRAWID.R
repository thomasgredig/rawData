#' Imports legacy RAW-ID.csv
#'
#' @param rawBase object that contains information with the legacy coded
#'
#' @returns updated rawBase object
#'
#' @export
raw.importRAWID <- function(rawBase) {
  check_rawBase(rawBase)
  if (!file.exists(rawBase$legacyRAWIDfile)) return(rawBase)
  if (nrow(rawBase$dataRAW)>0) stop("dataRAW must be empty to add legacy IDs")

  # read the header information
  df_header <- raw.readRAWIDheader(rawBase$legacyRAWIDfile)
  rawBase$raw_paths =  df_header$paths
  rawBase$raw_recursive = rep(TRUE, length(df_header$paths))


  # read all the data
  df = read.csv(rawBase$legacyRAWIDfile, comment.char = "#")
  create_dataRAW(df$ID,
                 raw_paths = df_header$paths,
                 filename = df$filename,
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
#' @importFrom stats setNames
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


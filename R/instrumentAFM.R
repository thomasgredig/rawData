#' Load AFM instrument data
#'
#' @param rawBase rawBase object
#' @returns data frame with AFM data for all AFM files in rawBase
#'
#' @importFrom usethis use_data ui_silence
#' @importFrom nanoAFMr AFM.import AFM.partial AFMinfo AFMinfo.item
#' @importFrom dplyr "%>%" distinct bind_rows
#'
#' @export
instrumentAFM <- function(rawBase) {
  # which files are recognized as AFM files
  .isafm <- function(filename) {
    grepl('\\.tiff$', filename) | grepl('\\.nid$', filename) |
      grepl('\\.ibw$', filename) | grepl('\\.\\d{3}$', filename)
  }

  # data frame with all files
  df <- as.data.frame(rawBase$dataRAW)

  # import previous datasets
  if(exists("dataFilesAFM")) {
    r = dataFilesAFM
    old_IDs = unique(r$ID)
  } else {
    r = data.frame()
    old_IDs = c()
  }

  # load all AFM  data
  result = data.frame()
  for(ID in df$ID) {
    if (ID %in% old_IDs) next
    filename = raw.getFilename(rawBase, ID)
    # cat("Loading: ", filename, "...\n")
    if (!.isafm(filename)) next
    if (!file.exists(filename)) next


    df <- AFM.import(filename)
    if (is.null(df)) next

    dfInfo <- AFMinfo(filename)
    note   <- AFMinfo.item(dfInfo, 'Note')
    scanRate      <- dfInfo$scanRate.Hz
    cantilever    <- AFMinfo.item(dfInfo,"cantilever")
    setPoint      <- AFMinfo.item(dfInfo, "Setpoint")
    scanAngle     <- AFMinfo.item(dfInfo,"ScanAngle")
    driveFrequency <- AFMinfo.item(dfInfo,"DriveFrequency")

    res.px = as.numeric(gsub('(\\d+).*','\\1',summary(df)$resolution))
    size.nm = as.numeric(gsub('(\\d+).*','\\1',summary(df)$size))

    r = cbind(
      ID = ID,
      sample = "",
      filename = basename(filename),
      partial = AFM.partial(df),
      note = note,
      quality = "",
      scanRate = scanRate,
      cantilever = cantilever,
      setPoint = setPoint,
      scanAngle = scanAngle,
      driveFrequency = driveFrequency,
      summary(df)[1,],
      res.px = res.px[1],
      size.nm = size.nm[1]
    )
    r$channel = paste(summary(df)$channel, collapse=",")
    r$z.min = paste(summary(df)$z.min, collapse=",")
    r$z.max = paste(summary(df)$z.max, collapse=",")
    r$z.units = paste(summary(df)$z.units, collapse=",")
    r$imgNo = 0
    r$direction = ""
    r$history <- NULL
    # Number of the image for the run
    r$imgNo = as.numeric(gsub('.*(\\d{3})\\.?.*','\\1',filename))
    # Scanning Direction of AFM run
    r$direction = 0
    r$direction[which(grepl('Forward',  filename)==TRUE)] = 1
    r$direction[which(grepl('Backward',  filename)==TRUE)] = -1

    result = rbind(result, r)
  }

  # save data to file
  data_filename = file.path("data", "dataFilesAFM.rda")
  if (file.exists(data_filename)) {
    former <- load(data_filename)
    dataFilesAFM <- bind_rows(former, result) %>%
      distinct(ID, .keep_all = TRUE, .fromLast = TRUE)
  } else {
    dataFilesAFM = result
  }
  ui_silence(
    use_data(dataFilesAFM, overwrite=TRUE)
  )

  rawBase
}

#' Update dataRAW with new files
#'
#' @description
#' Finds files that match the project name and are located in the path.
#' Then appends those files to the dataRAW.
#'
#' @returns dataRAW table
#'
#' @export
raw.update <- function(rawBase,
                       startID = 7,
                       recursive = TRUE,
                       verbose = FALSE) {
  # remove duplicate paths,etc.
  rawBase = .cleanRawBase(rawBase)

  # find files that could potentially be added to dataRAW
  fList = raw.find(rawBase, recursive=recursive)

  # Quit if no files are found.
  if (length(fList)==0) {
    cat("No RAW data files are found in these folders;
        check file naming conventions.")
    return(NULL)
  }

  # Start Processing by finding the new ID
  pRAW = .commonPath(fList)
  if (verbose) cat("Found", length(fList), "files.\n")

  # add each filename
  for(filename in fList) {
    new_dataRAW = create_dataRAW(startID, pRAW, filename)
    if (exists("dataRAW")) { dataRAW = rbind(dataRAW, new_dataRAW) } else { dataRAW = new_dataRAW }
    startID = startID + 1

  }

  return(dataRAW)
}


.getCRC <- function(filename) {
  strtoi( raw.getMD5(filename, 7), base = 16 )
}


.addFile <- function(f, ID, pRAW) {
  r = data.frame(
    ID = ID,
    path = .truncatePath(pRAW, dirname(f)),
    filename = basename(f),
    crc = .getCRC(f),
    size = file.info(f)$size,
    type = .getFileType(f),
    missing = !file.exists(f),
    altered = FALSE,
    sample = "",
    date = format(file.info(f)$atime),
    meta = ""
  )
  if (is.na(r$crc)) stop("Cannot generate MD5 check sum for file:",f)

  r
}

.truncatePath <- function(pRAW, pfad) {
  pRAW = gsub("\\\\","/",pRAW)
  gsub(pRAW,'', pfad)
}

.commonPath <- function(fList) {
  paths = unique(dirname(fList))
  Reduce(.common_prefix, paths)
}

.common_prefix <- function(str1, str2) {
  min_length <- min(nchar(str1), nchar(str2))
  for (i in 1:min_length) {
    if (substr(str1, i, i) != substr(str2, i, i)) {
      return(substr(str1, 1, i - 1))
    }
  }
  return(substr(str1, 1, min_length))
}

# returns file type
.getFileType <- function(filename) {
  type = ""
  f = basename(filename)
  if (grepl('\\_XRD',f)) type = "XRD"
  if (grepl('\\_XRR',f)) type = "XRR"
  if (grepl('\\_AMR',f)) type = "AMR"
  if (grepl('\\_FMR',f)) type = "FMR"
  if (grepl('\\_AFM',f)) type = "AFM"
  if (grepl('\\_EDS',f)) type = "EDS"
  if (grepl('\\_SEM',f)) type = "SEM"
  if (grepl('\\_Rxx',f)) type = "AMR"
  if (grepl('\\_DAT',f)) type = "VSM"

  if (type=="") {
    f = tools::file_ext(filename)
    if (grepl('nid',f)) type = 'AFM'
    if (grepl('ras[x]*',f)) type = 'XRD'
    if (grepl('ibw',f)) type = 'AFM'
    if (grepl('tiff',f)) type = 'AFM'
    if (grepl('\\d{3}$',f)) type = 'AFM'
    if (grepl('csv',f)) type = 'table'
  }

  type
}

.extendColumns <- function(df, dfNames) {
  mIn = names(df)
  d = df
  for(m in dfNames) {
    if (!(m %in% mIn)) {
      dAdd = rep("", nrow(d))
      d = cbind(d, dAdd)
    }
  }
  names(d) = dfNames
  d
}

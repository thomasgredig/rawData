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
                    dataRAW,
                    path,
                    ...) {
  rawBase$rawPaths = c(rawBase$rawPaths, path)
  # remove duplicate paths,etc.
  rawBase = .cleanRawBase(rawBase)
  dataRAW = raw.addFiles(rawBase, dataRAW, ...)
  dataRAW = raw.checkMissing(dataRAW)

  list(rawBase = rawBase,
       dataRAW = dataRAW)
}


#' Add dataRAW with new files
#'
#' @description
#' Finds files that match the project name and are located in the path.
#' Then appends those files to the dataRAW; if the file already exists
#' it will be updated, but not added; the ID remains the same.
#'
#' @param rawBase see raw.init() to create this
#' @param dataRAW a dataRAW S3 object
#' @param recursive logical to search paths recursively or not
#' @param verbose logical to output more information
#'
#' @returns dataRAW table
#'
#' @export
raw.addFiles <- function(rawBase,
                       dataRAW = NULL,
                       recursive = TRUE,
                       verbose = FALSE) {
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
  if (is.null(dataRAW)) {
    startID = 7
  } else {
    startID = max(dataRAW$df$ID) + 1
    dataRAW$df$missing = rep(TRUE, length(dataRAW$df$ID))
  }

  new_dataRAW = create_dataRAW(startID, fList)
  if (is.null(dataRAW)) { dataRAW = new_dataRAW } else { dataRAW = rbind(dataRAW, new_dataRAW) }

  # # add each file name
  # for(filename in fList) {
  #   new_dataRAW = create_dataRAW(startID, pRAW, filename)
  #   if (!is.null(dataRAW)) {
  #     # check if CRC is different
  #     noFile = which(dataRAW$crc==new_dataRAW$crc)
  #     if (length(noFile)>0) {
  #       # file is already in CRC
  #       # check if path and file name need to be updated
  #       dataRAW$filename[noFile] = new_dataRAW$filename
  #       dataRAW$path[noFile] = new_dataRAW$path
  #       dataRAW$sample[noFile] = new_dataRAW$sample
  #       dataRAW$type[noFile] = new_dataRAW$type
  #       dataRAW$missing[noFile] = FALSE
  #     } else {
  #       # brand new file
  #       dataRAW = rbind(dataRAW, new_dataRAW)
  #       startID = startID + 1
  #     }
  #   } else {
  #     # first file in dataRAW
  #     dataRAW = new_dataRAW
  #     startID = startID + 1
  #   }
  # }

  if (!is.null(attr(dataRAW, "pRAW"))) {
    attr(dataRAW, "pRAW") <- c(attr(dataRAW, "pRAW"),pRAW)
  } else {
    attr(dataRAW, "pRAW") <- pRAW
  }

  return(dataRAW)
}


.getCRC <- function(filename) {
  strtoi( substr(md5sum(filename),1,7), base = 16 )
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

# 20230208_CuPcAnnealing_RM_AFM_RM20230125Si11_Post_06b.ibw
.getSampleName <- function(filename) {
  # Use a regular expression to match the pattern RM followed by digits and letters
  # pattern <- "[_-][A-Za-z]+\\d{8}[A-Za-z0-9]+[_-]"
  # match <- regmatches(filename, regexpr(pattern, filename))
  # return(match)
  gsub(".*_.*_.*_.*_([A-Za-z0-9]+)_.*","\\1", filename)
}




#' Returns File Type
#' @importFrom tools file_ext
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
    if (grepl('ras[x]',f)) type = 'XRD'
    if (grepl('ibw',f)) type = 'AFM'
    if (grepl('tiff',f)) type = 'AFM'
    if (grepl('\\d{3}',f)) type = 'AFM'
    if (grepl('csv',f)) type = 'table'
    if (grepl('txt',f)) type = 'text'
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

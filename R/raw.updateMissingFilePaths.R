#' updates path for missing files
raw.updateMissingFilePaths <- function(rawBase) {
  # find all missing files and update location (filename is assumed to be the same!)
  dataRAW <- as.data.frame(rawBase$dataRAW)
  missing_files <- which(dataRAW$missing==TRUE)
  if(length(missing_files)>0L) {
    for(m in missing_files) {
      ID = dataRAW$ID[m]
      filename = dataRAW$filename[m]
      CRC = dataRAW$crc[m]
      updated_path = .findSubPath(rawBase$raw_paths, filename, CRC)
      if(length(updated_path)>0L) {
        dataRAW$path[m] = updated_path
      }
    }
    rawBase$dataRAW <- dataRAW # update

    # update the missing files
    rawBase <- raw.checkMissing(rawBase)
  } else {
    cli_inform("No missing files, so no update.")
  }

  rawBase
}


.findSubPath <- function(paths, filename, crc) {
  escape_regex <- function(x) { gsub("([][{}()+*^$.|?\\-])", "\\\\\\1", x) }
  new_path = ""
  for (path in paths) {
    if (dir.exists(path)) {
      file_list = dir(path, pattern=escape_regex(filename), recursive = TRUE)
      if(length(file_list)>0L) {
        for(file_found in file_list) {
          fname = file.path(path, file_found)
          c2 = .getCRC(fname)
          if (crc==c2) {
            new_path = dirname(file_found)
            next
          }
        }
      }
    }
  }
  new_path
}

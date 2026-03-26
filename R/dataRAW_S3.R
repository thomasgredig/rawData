#' Constructor of dataRAW S3 class
#'
#' @param ID unique ID for file
#' @param raw_paths vector with paths to search files
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
                           raw_paths,
                           filename,
                           crc = NULL,
                           size = NULL,
                           type = NULL,
                           missing = NULL,
                           altered = NULL,
                           sample = NULL,
                           date = NULL,
                           meta=NULL) {

  find_common_path <- function(paths, filenameList) {
    .fcp <- function(paths, filename) {
      for (path in paths) {
        if (startsWith(filename, path)) {
          return(gsub(path,"",filename))
        }
      }
      return(filename)
    }
    fList <- sapply(filenameList, function(x) { .fcp(paths,x) })
    unlist(fList)
  }

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

  # strip out common paths
  filename = find_common_path(raw_paths, filename)

  # create basic data frame
  df = data.frame(
    ID = ID,
    path = dirname(filename),
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
  # if (!is(d1,"dataRAW")) stop("dataRAW 1 object required.")
  # if (!is(d2,"dataRAW")) stop("dataRAW 2 object required.")

  # Convert dataRAW to dataframe
  df2 = as.data.frame(d2)
  if (nrow(df2)==0) return(d1)
  df1 = as.data.frame(d1)
  if (nrow(df1)==0) return(d2)
  df1 <- .consolidate_duplicated_IDs(df1)

  # df1 should not have any duplicates(!)
  if(length(which(duplicated(df1$crc)))>0L) warning("dataRAW frame 1 should not have duplicates.")

  # update IDs
  next_ID = max(df1$ID)
  df2$ID = 1:nrow(df2) + next_ID

  # find duplicates
  df3 = rbind(df1,df2)
  m <- which(duplicated(df3$crc)==TRUE)

  c_del = c()
  if (length(m)>0) {
    # iterate through all duplicates
    for(num in m) {
      m2 <- which(df3$crc==df3$crc[num])
      df3$ID[m2] = min(df3$ID[m2])  # IMPORTANT: change IDs
      # keep only the first none missing file with that CRC
      m2_keep = which(df3$missing[m2]==FALSE)
      if(length(m2_keep)>0L) {
        m_del = m2[-m2_keep[1]]
        c_del = c(c_del, m_del)
      }
    }
  }

  # remove duplicates
  c_del = unique(c_del)
  if(length(c_del)>0L) {
    # cat("Deleting ",length(c_del),"files.\n")
    df3 <- df3[-c_del,]
  }

  # Convert dataframe to dataRAW
  class(df3) <- "dataRAW"
  df3
}

.consolidate_duplicated_IDs <- function(df1) {
  # Find rows sharing the same ID
  split_rows <- split(seq_len(nrow(df1)), df1$ID)

  keep_idx <- unlist(lapply(split_rows, function(idx) {
    # Only one row for this ID -> keep it
    if (length(idx) == 1) {
      return(idx)
    }

    sub <- df1[idx, , drop = FALSE]

    # Require duplicated IDs to refer to the same filename
    if (length(unique(sub$filename)) != 1) {
      stop(
        sprintf(
          "ID %s appears multiple times with different filenames: %s",
          unique(sub$ID),
          paste(unique(sub$filename), collapse = ", ")
        )
      )
    }

    # Prefer an altered == TRUE row if present
    altered_idx <- idx[which(sub$altered %in% TRUE)]

    if (length(altered_idx) >= 1) {
      return(altered_idx[1])
    }

    # Otherwise keep the first one
    idx[1]
  }))

  # Return rows in original order
  df1[sort(keep_idx), , drop = FALSE]
}

#' Print method for the dataRAW class
#' @param RAW created with create_dataRAW() function
#' @param row.names logical to output additional row names
#' @param ... additional params
#' @importFrom utils head tail
#' @export
print.dataRAW <- function(d, row.names = FALSE, ...) {
  print(paste("dataRAW info on",length(d$ID),"files:"))
  df = data.frame(d$ID, d$filename, d$size, d$type, d$sample)
  print(df, row.names = row.names, ...)
}

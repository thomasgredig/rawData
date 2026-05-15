#' Load ATE instrument data
#'
#' @param rawBase rawBase object
#' @returns data frame with ATE data for deposition files
#'
#' @importFrom usethis use_data ui_silence
#' @importFrom angstromATE  ATE.readRecipe
#' @importFrom dplyr "%>%" distinct bind_rows
#' @importFrom cli cli_inform
#'
#' @export
instrumentATE <- function(rawBase) {
  # which files are recognized as AFM files
  .isATE <- function(filename) {
    grepl('\\.rcp$', filename)
  }

  # data frame with all files
  df <- as.data.frame(rawBase$dataRAW)

  data_filename = file.path("data", "dataATE.rda")
  if (file.exists(data_filename)) {
    load(data_filename)
  }


  # import previous datasets
  if(exists("dataATE", inherits = FALSE)) {
    result = dataATE
    old_IDs = unique(result$ID)
    cli_inform(paste0("dataATE found: ",length(old_IDs)," IDs."))
  } else {
    result = data.frame()
    old_IDs = c()
  }

  # load all ATE  data
  for(ID in df$ID) {
    if (ID %in% old_IDs) next
    filename = raw.getFilename(rawBase, ID)
    # cat("Loading: ", filename, "...\n")
    if (!.isATE(filename)) next
    if (!file.exists(filename)) next

    d_ate <- ATE.readRecipe(filename)
    if (is.null(d_ate)) next

    r_ate = data.frame(
      ID = ID,
      # sample = df$sample[df$ID==ID],
      filename = basename(filename),
      base_pressure = min(d_ate$pre_vacuum_pressure),
      source = paste(d_ate$source_name, collapse=";"),
      dep_time = paste(d_ate$deposition_time, collapse = ";")
    )


    result = dplyr::bind_rows(result, r_ate)
  }

  # save data to file
  dataATE = result
  ui_silence(
    use_data(dataATE, overwrite=TRUE)
  )

  rawBase
}

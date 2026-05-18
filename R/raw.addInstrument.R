#' Add Instruments
#'
#' @param rawBase rawBase object
#' @param instrument_list list with instrument name (XRD, AFM) and function to call
#'
#' @seealso [raw.init()]
#'
#' @export
raw.addInstrument <- function(rawBase, instrument_list) {
  if (!is.null(instrument_list)) {
    if (is(instrument_list,"list")) {
      # check whether instruments have been added already
      if(!is(rawBase$instruments,"list")) {
        rawBase$instruments = instrument_list
      } else {
        # add any new instruments to the list
        merged_list <- c(rawBase$instruments, instrument_list)
        unique_names <- unique(names(merged_list))
        rawBase$instruments <- merged_list[unique_names]
      }
    } else {
      warning("Instrument list must be a list.")
    }
  }

  rawBase
}

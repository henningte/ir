#' Imports infrared spectra from various files.
#'
#' \code{ir_import_csv} imports raw infrared spectra from a .csv file
#' that contains one spectrum, with x axis values (e.g. wavenumbers) in
#' the first column and intensity values in the second column, or several
#' of such files. Note that
#' the function does not perform any checks for the validity of the
#' content read from the .csv file.
#'
#' @param filenames A character vector representing the complete paths to
#' the .csv files to import.
#' @param sample_id A character vector with the same length as \code{filenames}
#' representing the name of the sample. If not provided (i.e. \code{sample_id = NULL}),
#' the sample name will be extracted from \code{filenames}.
#' @param ... Further arguments passed to \code{\link[utils:read.table]{read.csv}}.
#' @return An object of class \code{\link[ir:ir_new_ir]{ir}} containing the infrared spectra
#' extracted from the .csv file(s).
#' @export
ir_import_csv <- function(filenames,
                           sample_id = NULL,
                           ...) {

  # extract the sample name
  if(is.null(sample_id)){
    sample_id <- strsplit(filenames, split = "/")
    sample_id <- vapply(sample_id, function(x) x[[length(x)]], FUN.VALUE = character(1))
    sample_id <- substring(sample_id, 1, regexpr("\\.[^\\.]*$", sample_id) - 1)
  }


  x <- lapply(filenames, utils::read.csv, ...)
  ir_new_ir(spectra = x,
            sample_id = sample_id,
            metadata = tibble::tibble())

}

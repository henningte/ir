#' Imports infrared spectra from various files.
#'
#' \code{ir_import_csv} imports raw infrared spectra from one or more .csv file
#' that contains at least one spectrum, with x axis values (e.g. wavenumbers) in
#' the first column and intensity values of spectra in remaining columns. Note that
#' the function does not perform any checks for the validity of the content read
#' from the .csv file.
#'
#' @param filenames A character vector representing the complete paths to
#' the .csv files to import.
#' @param sample_id A character vector with the same length as \code{filenames}
#' representing the name of each sample. Can alternatively be set to \code{NULL} or
#' \code{"from_colnames"}. If set to \code{NULL}, the sample name(s) will be extracted
#' from \code{filenames}. If set to \code{"from_colnames"}, the sample name(s) will be
#' extracted from the column names of the imported data.
#' @param ... Further arguments passed to \code{\link[utils:read.table]{read.csv}}.
#' @return An object of class \code{\link[ir:ir_new_ir]{ir}} containing the infrared spectra
#' extracted from the .csv file(s).
#' @examples
#' # get sample data
#' d <- ir::ir_sample_data
#' # transform to list
#' dl <-
#'   list(
#'     ir::ir_flatten(d[1:10, ]),
#'     ir::ir_flatten(d[11:nrow(d), ])
#'     )
#' # save to temporary files
#' tf1 <- tempfile(fileext = ".csv")
#' tf2 <- tempfile(fileext = ".csv")
#' write.csv(dl[[1]], tf1, row.names = FALSE)
#' write.csv(dl[[2]], tf2, row.names = FALSE)
#' # import data from csv files
#' filenames <- list.files(gsub("\\\\file.+$", "", tf1), pattern = ".csv", full.names = TRUE)
#' d <- ir::ir_import_csv(filenames, sample_id = "from_colnames")
#' @export
ir_import_csv <- function(filenames,
                          sample_id = NULL,
                          ...) {

  # import the data
  x <- lapply(filenames, utils::read.csv, ...)

  # define the sample names
  x_nsamples <- vapply(x, ncol, FUN.VALUE = integer(1)) - 1
  if(!is.null(sample_id) && sample_id != "from_colnames") {
    x_nsamples_sum <- sum(x_nsamples)
    nsample_id <- length(sample_id)
    if(x_nsamples_sum != nsample_id) {
      rlang::abort(paste0("The files contain ", x_nsamples_sum, " spectra, but `sample_id` has ", nsample_id, " elements. `sample_id` must have the same number of elements."))
    }
  } else {
    if(is.null(sample_id)){
      sample_id <- strsplit(filenames, split = "/")
      sample_id <- vapply(sample_id, function(x) x[[length(x)]], FUN.VALUE = character(1))
      sample_id <- substring(sample_id, 1, regexpr("\\.[^\\.]*$", sample_id) - 1)
      sample_id <- rep(sample_id, x_nsamples)
    } else if(sample_id == "from_colnames") {
      sample_id <- unlist(lapply(x, function(y) colnames(y)[-1]))
    }
  }

  # get list column spectra
  x <- lapply(x, function(y) {
    lapply(y[, -1, drop = FALSE], function(z) {
      data.frame(x = y[, 1, drop = TRUE], y = z)
    })
  })
  x <- unlist(x, recursive = FALSE)

  ir_new_ir(spectra = x,
            sample_id = sample_id,
            metadata = tibble::tibble())

}

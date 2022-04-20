#' Imports infrared spectra from various files
#'
#' `ir_import_csv` imports raw infrared spectra from one or more `.csv` file
#' that contains at least one spectrum, with x axis values (e.g. wavenumbers) in
#' the first column and intensity values of spectra in remaining columns. Note
#' that the function does not perform any checks for the validity of the content
#' read from the .csv file.
#'
#' @param filenames A character vector representing the complete paths to
#' the `.csv` files to import.
#'
#' @param sample_id Either:
#' \itemize{
#'   \item `NULL`: Nothing additional happens.
#'   \item A character vector with the same length as `filenames`: This
#'     vector will be added as column `sample_id` to the `ir` object.
#'   \item `"from_filenames"`: The file name(s) will be used as values for
#'     a new column `sample_id` to add (the default).
#'   \item `"from_colnames"`: The header in the csv file will be used as
#'     values for a new column `sample_id` to add.
#' }
#'
#' @param ... Further arguments passed to
#' [`read.csv()`][utils::read.table].
#'
#' @return An object of class [`ir`][ir_new_ir()] containing the
#' infrared spectra extracted from the `.csv` file(s).
#'
#' @examples
#' # get sample data
#' d <- ir::ir_sample_data
#'
#' # transform to list
#' dl <-
#'   list(
#'     ir::ir_flatten(d[1:10, ]),
#'     ir::ir_flatten(d[11:nrow(d), ])
#'     )
#'
#' # save to temporary files
#' tf1 <- tempfile(fileext = ".csv")
#' tf2 <- tempfile(fileext = ".csv")
#' write.csv(dl[[1]], tf1, row.names = FALSE)
#' write.csv(dl[[2]], tf2, row.names = FALSE)
#'
#' # import data from csv files
#' filenames <- list.files(gsub("\\\\file.+$", "", tf1), pattern = ".csv", full.names = TRUE)
#' d <- ir::ir_import_csv(filenames, sample_id = "from_colnames")
#'
#' @export
ir_import_csv <- function(filenames,
                          sample_id = "from_filenames",
                          ...) {

  # import the data
  x <- lapply(filenames, utils::read.csv, ...)

  # define the sample names
  x_nsamples <- vapply(x, ncol, FUN.VALUE = integer(1)) - 1
  if(!is.null(sample_id) && ! sample_id %in% c("from_filenames", "from_colnames")) { # sample_id represents the sample_id values

    x_nsamples_sum <- sum(x_nsamples)
    nsample_id <- length(sample_id)
    if(x_nsamples_sum != nsample_id) {
      rlang::abort(paste0("The files contain ", x_nsamples_sum, " spectra, but `sample_id` has ", nsample_id, " elements. `sample_id` must have the same number of elements."))
    }

  } else if (sample_id %in% c("from_filenames", "from_colnames")) {
    switch(
      sample_id,
      "from_filenames" = {
        sample_id <- strsplit(filenames, split = "/")
        sample_id <- vapply(sample_id, function(x) x[[length(x)]], FUN.VALUE = character(1))
        sample_id <- substring(sample_id, 1, regexpr("\\.[^\\.]*$", sample_id) - 1)
        sample_id <- rep(sample_id, x_nsamples)
      },
      "from_colnames" = {
        sample_id <- unlist(lapply(x, function(y) colnames(y)[-1]))
      }
    )
  }
  metadata <- tibble::tibble(sample_id = sample_id)


  # get list column spectra
  x <- lapply(x, function(y) {
    lapply(y[, -1, drop = FALSE], function(z) {
      data.frame(x = y[, 1, drop = TRUE], y = z)
    })
  })
  x <- unlist(x, recursive = FALSE)

  ir_new_ir(spectra = x, metadata = metadata)

}

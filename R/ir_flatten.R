#' Converts objects of class `ir` to wide matrices
#'
#' `ir_flatten` takes and object of class `ir`, extracts the
#' `spectra` column and combines the spectra into one matrix. Metadata are
#' not retained during flattening.
#'
#' @param x An object of class [`ir`][ir_new_ir()].
#' @param measurement_id A character vector an element for each row in `x`
#' that contains the names to use as column names for the spectra in the matrix.
#' @return An object of class [`ir_flat()`][ir::ir_new_ir_flat].
#' @examples
#' x_flat <-
#'    ir::ir_sample_data %>%
#'    ir::ir_flatten()
#' @export
ir_flatten <- function(x,
                       measurement_id = as.character(seq_len(nrow(x)))) {

  if(!is.character(measurement_id)) {
    rlang::abort(paste0("`measurement_id` must be a character vector, not", class(measurement_id)[[1]], "."))
  }
  if(length(measurement_id) != nrow(x)) {
    rlang::abort("The number of rows in `x` must equal the length of `measurement_id`.")
  }

  ir_check_ir(x)
  x$spectra <- purrr::map2(x$spectra, measurement_id, function(x, y) {
    colnames(x)[[2]] <- y
    x
  })
  x <- purrr::reduce(x$spectra, dplyr::full_join, by = "x")
  ir_new_ir_flat(x[order(x$x), ])

}


#' Cleans objects of class `ir_flat`
#'
#' `ir_flatten_clean` takes an object of class `ir_flat` and either
#' returns all non-empty spectra or all empty spectra as object of class
#' `ir_flat`.
#'
#' @param x An object of class [`ir_flat()`][ir::ir_new_ir_flat].
#' @param return_empty A logical value indicating if the empty spectra should be
#' returned (`return_empty = TRUE`) or the non-empty spectra
#' (`return_empty = FALSE`).
#' @return A matrix where the first column (`"x"`) contains the unique x
#' axis values (e.g. wavenumbers) in `ir` and all remaining columns
#' represent intensity values from the spectra in `x`.
#' @export
ir_flat_clean <- function(x,
                          return_empty = FALSE) {

  ir_flat_check(x)

  x_is_empty <- purrr::map_lgl(x, function(x) all(is.na(x)))

  if(return_empty) {
    x_is_empty[[1]] <- TRUE
    x[, x_is_empty, drop = FALSE]
  } else {
    x[, !x_is_empty, drop = FALSE]
  }

}




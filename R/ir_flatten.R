#' Converts objects of class \code{ir} to wide matrices
#'
#' \code{ir_flatten} takes and object of class \code{ir}, extracts the
#' \code{spectra} column and combines the spectra into one matrix. Metadata are
#' not retained during flattening.
#'
#' @param x An object of class \code{\link[ir:ir_new_ir]{ir}}.
#' @param measurement_id A character vector an element for each row in \code{x}
#' that contains the names to use as column names for the spectra in the matrix.
#' @return An object of class \code{\link[ir:ir_new_ir_flat]{ir_flat}}.
#' @examples
#' x_flat <-
#'    ir::ir_sample_data %>%
#'    ir::ir_flatten()
#' @export
ir_flatten <- function(x,
                       measurement_id = as.character(x$measurement_id)) {

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


#' Cleans objects of class \code{ir_flat}
#'
#' \code{ir_flatten_clean} takes an object of class \code{ir_flat} and either
#' returns all non-empty spectra or all empty spectra as object of class
#' \code{ir_flat}.
#'
#' @param x An object of class \code{\link[ir:ir_new_ir_flat]{ir_flat}}.
#' @param return_empty A logical value indicating if the empty spectra should be
#' returned (\code{return_empty = TRUE}) or the non-empty spectra
#' (\code{return_empty = FALSE}).
#' @return A matrix where the first column (\code{"x"}) contains the unique x
#' axis values (e.g. wavenumbers) in \code{ir} and all remaining columns
#' represent intensity values from the spectra in \code{x}.
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




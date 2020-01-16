#' Cleans objects of class \code{ir_flat}.
#'
#' \code{ir_flatten_clean} takes an object of class \code{ir_flat} and
#' either returns all non-empty spectra
#' or all empty spectra as object of class \code{ir_flat}.
#'
#' @param x An object of class \code{\link[ir:ir_new_ir_flat]{ir}}.
#' @param return_empty A logical value indicating if the empty
#' spectra should be returned (\code{return_empty = TRUE}) or
#' the non-empty spectra (\code{return_empty = FALSE}).
#' @return A matrix where the first column (\code{"x"}) contains the
#' unique x axis values (e.g. wavenumbers) in \code{ir} and all remaining
#' columns represent intensity values from the spectra in \code{x}.
#' @export
ir_flat_clean <- function(x,
                          return_empty = FALSE) {

  ir_flat_check(x)

  x_is_empty <- purrr::map_lgl(x, function(x) all(is.na(x)))
  if(return_empty) {
    x_is_empty[[1]] <- TRUE
    x[, x_is_empty, drop = FALSE]
  } else {
    x[,!x_is_empty, drop = FALSE]
  }

}



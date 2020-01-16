#' Creates an object of class \code{ir_flat}.
#'
#' \code{ir_new_ir_flat} is the constructor function for objects of class
#' \code{ir_flat}.
#' An object of class \code{ir_flat} is a \code{data.frame}
#' where the first column (\code{"x"}) contains unique x values of
#' spectra (e.g. wavenumbers) and all remaining
#' columns represent intensity values from spectra corresponding to the
#' x values.
#'
#' @param x A \code{data.frame} with only numeric columns and only the first
#' column name being "x".
#' @return An object of class \code{ir_flat}.
#' @export
ir_new_ir_flat <- function(x) {

  if(!is.data.frame(x)) {
    rlang::abort(paste0("`x` must be a data.frame, not ", class(x)[[x]], "."))
  }
  x_all_numeric <- purrr::map_lgl(x, is.numeric)
  if(!all(x_all_numeric)) {
    if(sum(!x_all_numeric) == 1) {
      rlang::abort(paste0("All columns of `x` must be numeric. Column", which(!x_all_numeric), " is not numeric."))
    } else {
      rlang::abort(paste0("All columns of `x` must be numeric. Columns", paste(which(!x_all_numeric), collapse = ", "), " are not numeric."))
    }
  }
  if(colnames(x)[[1]] != "x") {
    rlang::abort(paste0("The first column of `x` must be named 'x' and not ", colnames(x)[[1]], "."))
  }
  x_x_all_not_na <- !is.na(x$x)
  if(!all(x_x_all_not_na)) {
    if(sum(!x_x_all_not_na) == 1) {
      rlang::abort(paste0("Values of `x$x` must not be NA. Value", which(!x_x_all_not_na), " is NA."))
    } else {
      rlang::abort(paste0("Values of `x$x` must not be NA. Values", paste(which(!x_x_all_not_na), collapse = ", "), " are NA."))
    }
  }
  x_x_all_unique <- !duplicated(x$x)
  if(!all(x_x_all_unique)) {
    rlang::abort("`x$x` must not contain duplicated values.")
  }
  structure(x, class = c("ir_flat", class(x)))

}

#' Checks if an object is of class \code{ir_flat}.
#'
#' \code{ir_flat_check} checks if an object is of class \code{\link[ir:ir_flat]{ir_flat}}.
#'
#' @param x An object.
#' @return An object of class \code{\link[ir:ir_new_ir_flat]{ir_flat}}.
#' @export
ir_flat_check <- function(x) {
  x_sym <- as.character(rlang::get_expr(rlang::enquo(x)))
  if(!inherits(x, "ir_flat"))
    rlang::abort(paste0("`", x_sym, "` must be of class `ir_flat`, not ", class(x)[[1]], "."))
  x
}

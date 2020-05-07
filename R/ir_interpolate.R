#' Interpolates intensity values of infrared spectra for new wavenumber values.
#'
#' \code{ir_interpolate} interpolates intensity values for infrared spectra
#' for new wavenumber values.
#'
#' @param x An object of class \code{\link[ir:ir_new_ir]{ir}}.
#' @param start A numerical value indicating the start wavenumber value
#' relative to which new wavenumber values will be interpolated. The value
#' is not allowed to be < \code{floor(firstvalue) - 2}, whereby \code{firstvalue}
#' is the first wavenumber value within \code{x}. If \code{start = NULL},
#' \code{floor(firstvalue)} will be used as first wavenumber value.
#' @param dw A numerical value representing the desired wavenumber value
#' difference between adjacent values.
#' @return An object of class \code{ir} containing the interpolated
#' spectra.
#' @export
ir_interpolate <- function(x,
                           start = NULL,
                           dw = 1) {

  # checks
  ir_check_ir(x)
  x_flat <- ir_flatten(x)
  if(!is.null(start)) {
    if(!is.numeric(start)) {
      rlang::abort("`start` must be numeric, not ", class(start)[[1]], ".")
    }
    if(length(start) != 1) {
      rlang::abort("`start` must be of length 1, not ", length(start), ".")
    }
    if(min(x_flat[, 1, drop = TRUE]) <= start) {
      rlang::abort("`start` must not be smaller than the smallest x axis value of any spectrum in `x` (", min(x_flat[, 1, drop = TRUE]), ").")
    }
  } else {
    start <- floor(min(x_flat$x, na.rm = TRUE))
  }
  if(!is.numeric(dw)) {
    rlang::abort("`dw` must be numeric, not ", class(dw)[[1]], ".")
  }
  if(length(dw) != 1) {
    rlang::abort("`dw` must be of length 1, not ", length(dw), ".")
  }

  x_flat <- dplyr::arrange(x_flat, x)

  # fill NA values to support easy indexing
  x_flat <- as.data.frame(data.table::nafill(x_flat, type = "locf"))
  colnames(x_flat) <- c("x", x$measurement_id)

  # define the new wavenumber values
  wavenumber_new <- seq(from = start,
                        to = max(x_flat$x, na.rm = TRUE),
                        by = dw)

  # do the interpolation
  x_flat_new <- cbind(x = wavenumber_new, purrr::map_df(x_flat[, -1, drop = FALSE], function(y){
    stats::approx(x = x_flat$x,
                  y = y,
                  xout = wavenumber_new,
                  method = "linear",
                  rule = 1,
                  ties = "ordered")$y
  }))

  # interpolate new wavenumber values
  x$spectra <- ir_stack(x_flat_new)$spectra
  x

}

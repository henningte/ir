#' Clips infrared spectra to new wavenumber ranges.
#'
#' \code{ir_clip} clips infrared spectra to a new, specified,
#' wavenumber range.
#'
#' @param x An object of class \code{\link[ir:ir_new_ir]{ir}}.
#' @param range A numeric vector of length two
#' with the first element specifying the start
#' of the new wavenumber range and the second
#' element specifying the end of the new wavenumber
#' range.
#' @return An object of class \code{ir}.
#' @export
ir_clip <- function(x,
                    range) {

  # checks
  if(!inherits(x, "ir")) {
    rlang::abort(paste0("`x` must be of class ir, not ", class(x)[[1]],"."))
  }
  if(!is.numeric(range)) {
    rlang::abort("`range` must be a numeric vector.")
  }
  if(length(range) != 2) {
    stop("`range` must be of length 2.")
  }

  range <- sort(range)

  # detect the corresponding row indices
  x_flat <- ir_flatten(x)
  range <- ir_get_wavenumberindex(x_flat, wavenumber = range, warn = TRUE)
  index <- range[1]:range[2]

  # clip x
  x$spectra <- ir_stack(x_flat[index, ])$spectra
  x

}

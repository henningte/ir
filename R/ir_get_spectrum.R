#' Extracts selected spectra from an object of class \code{ir}
#'
#' \code{ir_get_spectrum} extracts selected spectra from an object of class
#' \code{ir}.
#'
#' @param x An object of class \code{\link[ir:ir_new_ir]{ir}}.
#' @param what A numeric vector with each element representing a row in \code{x}
#' for which to extract the spectrum.
#' @return An integer vector with the same length as \code{wavenumber} with the
#' row indices of \code{x} corresponding to the wavenumber values in
#' \code{wavenumber}.
#' @examples
#' x <-
#'    ir::ir_sample_data %>%
#'    ir::ir_get_spectrum(what = c(5, 9))
#' @export
ir_get_spectrum <- function(x,
                            what) {

  # checks
  ir_check_ir(x)
  if(!(is.numeric(what))){
    rlang::abort("`what` must be numeric.")
  }
  what_integer <- what %% 1 == 0
  if(!all(what_integer)) {
    if(sum(!what_integer) == 1) {
      rlang::abort(paste0("`what` must only contain whole numbers. Element ", which(!what_integer), " is a double."))
    } else {
      rlang::abort(paste0("`what` must only contain whole numbers. Elements ", which(!what_integer), " are doubles."))
    }
  }

  x$spectra[what]

}

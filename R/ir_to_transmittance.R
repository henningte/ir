#' Converts absorbance spectra to transmittance spectra or vice versa
#'
#' \code{ir_to_transmittance} converts absorbance spectra to transmittance
#' spectra. \code{ir_to_absorbance} converts transmittance spectra to absorbance
#' spectra. Note that neither function checks whether the input spectra are
#' absorbance or transmittance spectra.
#'
#' @param x An object of class \code{\link{ir}}.
#' @return \code{x} with y values fore each spectrum as transmittance values
#' (in case of \code{ir_to_transmittance}) or absorbance values (in case of
#' \code{ir_to_absorbance}).
#' @source \insertCite{Stuart.2004}{ir}.
#' @references
#' \insertAllCited{}
#' @examples
#' # convert from absorbance to transmittance
#' x1 <-
#'     ir::ir_sample_data %>%
#'     ir::ir_to_transmittance()
#'
#' @export
ir_to_transmittance <- function(x) {

  ir_check_ir(x)

  x$spectra <- lapply(x$spectra, function(z) {
    z$y <- 10^(-z$y)
    z
  })
  x

}

#' @rdname  ir_to_transmittance
#' @examples
#' # convert from transmittance to absorbance
#' x2 <-
#'     x1 %>%
#'     ir::ir_to_absorbance()
#'
#' vapply(
#'   seq_along(x2$spectra),
#'   FUN = function(i) all.equal(x2$spectra[[i]], ir::ir_sample_data$spectra[[i]]),
#'   FUN.VALUE = logical(1L)
#' ) %>%
#'   all()
#'
#' @export
ir_to_absorbance <- function(x) {

  ir_check_ir(x)

  x$spectra <- lapply(x$spectra, function(z) {
    z$y <- -log10(z$y)
    z
  })
  x

}

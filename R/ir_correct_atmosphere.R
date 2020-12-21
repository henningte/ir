#' Corrects artifacts in a spectrum based on reference spectra of the artifact compound.
#'
#' \code{ir_correct_atmosphere} takes two objects of class \code{ir} with the same
#' number of spectra in each and corrects the spectra of the first object with the
#' spectra of the second object according to the procedure presented by
#' \insertCite{PerezGuaita.2013}{ir}.
#'
#' @note The function was not tested yet.
#'
#' @param x An object of class \code{\link[ir:ir_new_ir]{ir}} containing the
#' spectra to correct.
#' @param ref An object of class \code{\link[ir:ir_new_ir]{ir}} containing the
#' reference spectra to use for correction. \code{ref} must have the same number
#' of rows as \code{x}, the contained spectra must cover the wavenumber range of
#' all spectra in \code{x}, and if \code{interpolate = FALSE}, all spectra must have
#' identical wavenumber values.
#' @param wn1 A numeric value representing the first wavenumber value to use as
#' reference point \insertCite{PerezGuaita.2013}{ir}. Examples used by
#' \insertCite{PerezGuaita.2013;textual}{ir} are:
#' \describe{
#'   \item{H\eqn{_2}O}{3902 cm\eqn{^{-1}}.}
#'   \item{CO\eqn{_2}}{2361 cm\eqn{^{-1}}.}
#' }
#' @param wn2 A numeric value representing the second wavenumber value to use as
#' reference point \insertCite{PerezGuaita.2013}{ir}. Examples used by
#' \insertCite{PerezGuaita.2013;textual}{ir} are:
#' \describe{
#'   \item{H\eqn{_2}O}{3912 cm\eqn{^{-1}}.}
#'   \item{CO\eqn{_2}}{2349 cm\eqn{^{-1}}.}
#' }
#' @param interpolate A logical value indicating if \code{x} and \code{ref} should be
#' interpolated prior correction (\code{TRUE}) or not (\code{FALSE}).
#' @param start See \code{\link[ir:ir_interpolate]{ir_interpolate}}.
#' @param dw See \code{\link[ir:ir_interpolate]{ir_interpolate}}.
#' @param warn A logical value indicating if warnings about mismatching
#' wavenumber values should be displayed (\code{TRUE}) or not (\code{FALSE}).
#' If set to \code{TRUE} and \code{wn1} or \code{wn2} do not exactly match the
#' wavenumber values in \code{x} and \code{ref}, a warning will be printed to
#' inform about the wavenumber difference between the selected and targeted
#' wavenumber value.
#' @return \code{x} corrected with the reference spectra in \code{ref}.
#' @source
#' \insertAllCited{}
#' @export
ir_correct_atmosphere <- function(x,
                                  ref,
                                  wn1,
                                  wn2,
                                  interpolate = FALSE,
                                  start = NULL,
                                  dw = 1,
                                  warn = TRUE) {

  # checks
  if(!inherits(x, "ir")) {
    rlang::abort(paste0("`x` must be of class ir, not ", class(x)[[1]],"."))
  }
  if(!inherits(ref, "ir")) {
    rlang::abort(paste0("`ref` must be of class ir, not ", class(ref)[[1]],"."))
  }
  if(nrow(x) != nrow(ref)) {
    rlang::abort('`ref` must have the same number of rows as `x`.')
  }
  if(!is.logical(interpolate) | length(interpolate) != 1) {
    rlang::abort('`interpolate` must be a logical value.')
  }
  if(interpolate) {
    x <- ir_interpolate(x = x, start = start, dw = dw)
    ref <- ir_interpolate(x = ref, start = start, dw = dw)
  }
  x$spectra <- purrr::map(x$spectra, dplyr::arrange, .data$x) # assure that x and ref have same wavenumber order
  x_flat <- ir_flatten(x)
  ref <- ir_clip(x = ref, range = data.frame(start = x_flat$x[[1]], end = x_flat$x[[nrow(x_flat)]], stringsAsFactors = FALSE))
  ref_flat <- ir_flatten(ref)
  if(x_flat$x[[1]] < ref_flat$x[[1]] | x_flat$x[[nrow(x_flat)]] > ref_flat$x[[nrow(ref_flat)]]) {
    rlang::abort('`ref` must cover the complete wavenumber range of `x`, but covers only a smaller range.')
  }
  if(!identical(x_flat$x, ref_flat$x)) {
    rlang::abort('`x` and `ref` must have identical wavenumber values. If `interpolate` was set to `FALSE`, you could try to interpolate both spectra to achieve this.')
  }
  if(!is.numeric(wn1) | length(wn1) != 1) {
    rlang::abort('`wn1` must be a numeric value.')
  }
  if(!is.numeric(wn2) | length(wn2) != 1 | wn2 == wn1) {
    rlang::abort('`wn2` must be a numeric value and not identical to `wn1`.')
  }
  # ensure same dimensions of all spectra
  x$spectra <- ir_stack(x_flat)$spectra
  ref$spectra <- ir_stack(ref_flat)$spectra

  # get intensities
  ra_x <- tidyr::pivot_wider(dplyr::bind_rows(ir_get_intensity(x, wavenumber = c(wn1, wn2), warn = warn)$intensity), names_from = "x", values_from = "y", values_fn = list)
  colnames(ra_x) <- c("wn1", "wn2")
  ra_x <- tidyr::unnest(ra_x, cols = c("wn1", "wn2"))
  ra_ref <- tidyr::pivot_wider(dplyr::bind_rows(ir_get_intensity(ref, wavenumber = c(wn1, wn2), warn = warn)$intensity), names_from = "x", values_from = "y", values_fn = list)
  colnames(ra_ref) <- c("wn1", "wn2")
  ra_ref <- tidyr::unnest(ra_ref, cols = c("wn1", "wn2"))

  # compute relative absorbance and contribution
  ra_x$ra <- ra_x$wn1 - ra_x$wn2
  ra_ref$ra <- ra_ref$wn1 - ra_ref$wn2
  ra_x$f <- ra_x$ra / ra_ref$ra

  # baseline subtraction
  x$spectra <- purrr::map(seq_along(x$spectra), function(i) {
    y <- x$spectra[[i]]
    y$y <- y$y - ra_x$wn2[[i]]
    y
  })
  ref$spectra <- purrr::map(seq_along(ref$spectra), function(i) {
    y <- ref$spectra[[i]]
    y$y <- y$y - ra_ref$wn2[[i]]
    y
  })

  # correction
  x$spectra <- purrr::map(seq_along(x$spectra), function(i) {
    y <- x$spectra[[i]]
    y$y <- y$y - ra_x$f[[i]] * ref$spectra[[i]]$y
    y
  })

  # revert baseline correction
  x$spectra <- purrr::map(seq_along(x$spectra), function(i) {
    y <- x$spectra[[i]]
    y$y <- y$y + ra_x$wn2[[i]]
    y
  })

  x

}

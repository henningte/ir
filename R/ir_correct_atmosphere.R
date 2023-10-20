#' Corrects artifacts in a spectrum based on reference spectra of the artifact compound
#'
#' `ir_correct_atmosphere` takes two objects of class `ir` with the same number
#' of spectra in each and corrects the spectra of the first object with the
#' spectra of the second object according to the procedure presented by
#' \insertCite{PerezGuaita.2013}{ir}.
#'
#' @note The function was not tested yet.
#'
#' @param x An object of class [`ir`][ir_new_ir()] containing the spectra to
#' correct (with intensities representing absorbances).
#'
#' @param ref An object of class [`ir`][ir_new_ir()] containing the reference
#' spectra to use for correction  (with intensities representing absorbances).
#' `ref` must have the same number of rows as `x`, the contained spectra must
#' cover the wavenumber range of all spectra in `x`, and if
#' `do_interpolate = FALSE`, all spectra must have identical wavenumber values.
#'
#' @param wn1 A numeric value representing the first wavenumber value to use as
#' reference point \insertCite{PerezGuaita.2013}{ir}. Examples used by
#' \insertCite{PerezGuaita.2013;textual}{ir} are:
#' \describe{
#'   \item{H\eqn{_2}O}{3902 cm\eqn{^{-1}}.}
#'   \item{CO\eqn{_2}}{2361 cm\eqn{^{-1}}.}
#' }
#'
#' @param wn2 A numeric value representing the second wavenumber value to use as
#' reference point \insertCite{PerezGuaita.2013}{ir}. Examples used by
#' \insertCite{PerezGuaita.2013;textual}{ir} are:
#' \describe{
#'   \item{H\eqn{_2}O}{3912 cm\eqn{^{-1}}.}
#'   \item{CO\eqn{_2}}{2349 cm\eqn{^{-1}}.}
#' }
#'
#' @param return_contribution A logical value indicating whether in addition to
#' the corrected spectra, the computed relative contribution of `ref` to each
#' spectrum in `x` should be added to the returned object as new column
#' `contribution` (`TRUE`) or not (`FALSE`).
#'
#' @param do_interpolate A logical value indicating if `x` and `ref` should be
#' interpolated prior correction (`TRUE`) or not (`FALSE`).
#'
#' @param start See [ir_interpolate()].
#'
#' @param dw See [ir_interpolate()].
#'
#' @param warn A logical value indicating whether warnings about mismatching
#' wavenumber values should be displayed (`TRUE`) or not (`FALSE`). If set to
#' `TRUE` and `wn1` or `wn2` do not exactly match the wavenumber values in `x`
#' and `ref`, a warning will be printed to inform about the wavenumber
#' difference between the selected and targeted wavenumber value.
#'
#' @return `x` corrected with the reference spectra in `ref`.
#'
#' @source
#' \insertAllCited{}
#'
#' @examples
#' x1 <-
#'   ir_correct_atmosphere(
#'     ir_sample_data[1:5, ], ir_sample_data[1:5, ], wn1 = 2361, wn2 = 2349
#'   )
#'
#' x2 <-
#'   ir_correct_atmosphere(
#'     ir_sample_data[1:5, ], ir_sample_data[1:5, ], wn1 = 2361, wn2 = 2349,
#'     return_contribution = TRUE
#'   )
#'
#' x2$contribution
#'
#' @export
ir_correct_atmosphere <- function(x,
                                  ref,
                                  wn1,
                                  wn2,
                                  return_contribution = FALSE,
                                  do_interpolate = FALSE,
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
  if(!is.logical(return_contribution) | length(return_contribution) != 1) {
    rlang::abort('`return_contribution` must be a logical value.')
  }
  if(!is.logical(do_interpolate) | length(do_interpolate) != 1) {
    rlang::abort('`do_interpolate` must be a logical value.')
  }
  if(do_interpolate) {
    x <- ir_interpolate(x = x, start = start, dw = dw)
    ref <- ir_interpolate(x = ref, start = start, dw = dw)
  }
  spectrum_is_empty <- ir_identify_empty_spectra(x)
  if(all(spectrum_is_empty)) {
    return(x)
  }
  x_flat <- ir_flatten(x)
  x_flat <- x_flat[order(x_flat$x), ] # assure that x and ref have same wavenumber order
  ref <- ir_clip(x = ref, range = data.frame(start = x_flat$x[[1]], end = x_flat$x[[nrow(x_flat)]], stringsAsFactors = FALSE))
  ref_flat <- ir_flatten(ref)
  if(x_flat$x[[1]] < ref_flat$x[[1]] | x_flat$x[[nrow(x_flat)]] > ref_flat$x[[nrow(ref_flat)]]) {
    rlang::abort('`ref` must cover the complete wavenumber range of `x`, but covers only a smaller range.')
  }
  if(!identical(x_flat$x, ref_flat$x)) {
    rlang::abort('`x` and `ref` must have identical wavenumber values. If `do_interpolate` was set to `FALSE`, you could try to interpolate both spectra to achieve this.')
  }
  if(!is.numeric(wn1) || length(wn1) != 1) {
    rlang::abort('`wn1` must be a numeric value.')
  }
  if(!is.numeric(wn2) || length(wn2) != 1 | wn2 == wn1) {
    rlang::abort('`wn2` must be a numeric value and not identical to `wn1`.')
  }
  # ensure same dimensions of all spectra
  x$spectra <- ir_stack(x_flat)$spectra
  ref$spectra <- ir_stack(ref_flat)$spectra

  # get intensities
  ra_x <- tidyr::pivot_wider(tidyr::unnest(tibble::tibble(x = ir_get_intensity(x, wavenumber = c(wn1, wn2), warn = warn)$intensity), cols = "x"), names_from = "x", values_from = "y", values_fn = list)
  colnames(ra_x) <- c("wn1", "wn2")
  ra_x <- tidyr::unnest(ra_x, cols = c("wn1", "wn2"))
  ra_ref <- tidyr::pivot_wider(tidyr::unnest(tibble::tibble(x = ir_get_intensity(ref, wavenumber = c(wn1, wn2), warn = warn)$intensity), cols = "x"), names_from = "x", values_from = "y", values_fn = list)
  colnames(ra_ref) <- c("wn1", "wn2")
  ra_ref <- tidyr::unnest(ra_ref, cols = c("wn1", "wn2"))

  # compute relative absorbance and contribution
  ra_x$ra <- ra_x$wn1 - ra_x$wn2
  ra_ref$ra <- ra_ref$wn1 - ra_ref$wn2
  ra_x$f <- ra_x$ra / ra_ref$ra
  if(return_contribution) {
    x$contribution <- ra_x$f
  }

  # baseline subtraction
  x <- x - ra_x$wn2
  ref <- ref - ra_ref$wn2

  # correction
  x_corr <- x - (ref * ra_x$f)

  # revert baseline correction
  intensity_corr_wn2 <- tidyr::unnest(ir::ir_get_intensity(x_corr, wavenumber = wn2, warn = warn), cols = "intensity") %>% dplyr::pull(.data$y)
  x_corr + intensity_corr_wn2

}

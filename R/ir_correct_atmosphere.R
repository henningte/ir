#' Corrects artifacts in a spectrum based on reference spectra of the artifact compound
#'
#' `ir_correct_atmosphere` takes two objects of class `ir` with the same number
#' of spectra in each and corrects the spectra of the first object with the
#' spectra of the second object according to the procedure presented by
#' \insertCite{Perez-Guaita.2013}{ir}.
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
#' reference point \insertCite{Perez-Guaita.2013}{ir}. Examples used by
#' \insertCite{Perez-Guaita.2013;textual}{ir} are:
#' \describe{
#'   \item{H\eqn{_2}O}{3902 cm\eqn{^{-1}}.}
#'   \item{CO\eqn{_2}}{2361 cm\eqn{^{-1}}.}
#' }
#'
#' @param wn2 A numeric value representing the second wavenumber value to use as
#' reference point \insertCite{Perez-Guaita.2013}{ir}. Examples used by
#' \insertCite{Perez-Guaita.2013;textual}{ir} are:
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
#' @param return_ir_flat Logical value. If `TRUE`, the spectra are returned as
#' [`ir_flat`][ir_new_ir_flat()] object.
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
                                  warn = TRUE,
                                  return_ir_flat = FALSE) {

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
  if(!is.logical(return_ir_flat) | length(return_ir_flat) != 1) {
    rlang::abort('`return_ir_flat` must be a logical value.')
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

  # new
  x_flat <- ir_flatten(x)
  x_flat <- x_flat[order(x_flat$x), ] # assure that x and ref have same wavenumber order
  ref_flat <-
    ir_clip(
      x = ref,
      range = data.frame(start = x_flat$x[[1]], end = x_flat$x[[nrow(x_flat)]], stringsAsFactors = FALSE),
      return_ir_flat = TRUE
    )
  ref_flat <- ref_flat[order(ref_flat$x), ]
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

  # get intensities
  index <- ir_get_wavenumberindex(x = x_flat, wavenumber = c(wn1, wn2), warn = warn)
  ra_x <-
    tibble::tibble(
      wn1 = unlist(x_flat[index[[1]], -1]),
      wn2 = unlist(x_flat[index[[2]], -1])
    )
  ra_ref <-
    tibble::tibble(
      wn1 = unlist(ref_flat[index[[1]], -1]),
      wn2 = unlist(ref_flat[index[[2]], -1])
    )

  # compute relative absorbance and contribution
  ra_x$ra <- ra_x$wn1 - ra_x$wn2
  ra_ref$ra <- ra_ref$wn1 - ra_ref$wn2
  ra_x$f <- ra_x$ra / ra_ref$ra
  if(return_contribution) {
    x$contribution <- ra_x$f
  }

  # baseline subtraction
  x_flat[, -1] <- x_flat[, -1, drop = FALSE] - matrix(ra_x$wn2, nrow = nrow(x_flat), ncol = ncol(x_flat) - 1L, byrow = TRUE)
  ref_flat[, -1] <- ref_flat[, -1, drop = FALSE] - matrix(ra_ref$wn2, nrow = nrow(x_flat), ncol = ncol(x_flat) - 1L, byrow = TRUE)

  # correction
  x_corr <- x_flat
  x_corr[, -1] <- x_flat[, -1, drop = FALSE] - (ref_flat[, -1, drop = FALSE] * matrix(ra_x$f, nrow = nrow(x_flat), ncol = ncol(x_flat) - 1L, byrow = TRUE))

  # revert baseline correction
  intensity_corr_wn2 <- unlist(x_flat[index[[2]], -1])
  x_corr[, -1] <- x_corr[, -1, drop = FALSE] + matrix(intensity_corr_wn2, nrow = nrow(x_flat), ncol = ncol(x_flat) - 1L, byrow = TRUE)

  res <-
    if(return_ir_flat) {
      x_corr
    } else {
      x$spectra <-
        ir_stack(x_corr)$spectra
      x
    }

  res

}

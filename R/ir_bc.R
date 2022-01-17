#' Performs baseline correction on infrared spectra
#'
#' \code{ir_bc} performs baseline correction for infrared
#' spectra. Baseline correction
#' is either performed by using a polynomial with user defined
#' degree fitted to each spectrum (see
#' \code{\link[ChemoSpec:baselineSpectra]{baselineSpectra}}), or by using a
#' rubberband function that is fitted to each spectrum
#' (see \code{\link[hyperSpec:spc.rubberband]{spc.rubberband}}), or using a
#' Savitzky-Golay smoothed version of the input spectra (see
#' \code{\link{ir_bc_sg}}).
#'
#' @param x An object of class \code{\link[ir:ir_new_ir]{ir}}.
#' @param method A character value indicating which method should be used
#' for baseline correction. If \code{method = "polynomial"}, a polynomial
#' is used for baseline correction. If \code{method = "rubberband"}, a
#' rubberband function is used for baseline correction. If \code{method = "sg"},
#' a Savitzky-Golay smoothed version of the input spectra is used for baseline
#' correction.
#' @param ... Further arguments passed to \code{\link{ir_bc_polynomial}} or
#'  \code{\link{ir_bc_sg}}.
#' @param return_bl A logical value indicating if for each spectrum the baseline
#' should be returned in addition to the corrected intensity values
#' (\code{return_bl = TRUE}) or not (\code{return_bl = FALSE}).
#' @return An object of class \code{ir} with the baseline
#' corrected spectra and if \code{returnbl = TRUE} a new list column
#' "baselines" with the baselines.
#' @examples
#' # rubberband baseline correction
#' x1 <-
#'    ir::ir_sample_data %>%
#'    ir::ir_bc(method = "rubberband")
#'
#' # polynomial baseline correction
#' x2 <-
#'    ir::ir_sample_data %>%
#'    ir::ir_bc(method = "polynomial", degree = 2)
#'
#' # Savitzky-Golay baseline correction
#' x3 <-
#'    ir::ir_sample_data %>%
#'    ir::ir_bc(method = "sg", p = 3, n = 199, ts = 1, m = 0)
#' @export
ir_bc <- function(x,
                  method = "rubberband",
                  ...,
                  return_bl = FALSE) {

  # checks
  ir_check_ir(x)
  if(!(is.logical(return_bl) |
       length(return_bl) == 1)){
    rlang::abort("`return_bl` must be a logical value")
  }

  # perform baseline correction
  switch(
    method,
    polynomial = {
      ir_bc_polynomial(
        x,
        ...,
        return_bl = return_bl
      )
    },
    rubberband = {
      ir_bc_rubberband(
        x,
        return_bl = return_bl
      )
    },
    sg = {
      ir_bc_sg(
        x,
        ...,
        return_bl = return_bl
      )
    },
    {rlang::abort("Unknown method.")}
  )

}


#' Performs baseline correction on infrared spectra using a Savitzky-Golay baseline
#'
#' \code{ir_bc_sg} computes a smoothed version of spectra using
#' \code{\link{ir_smooth}} with \code{method = "sg"} and uses this as baseline
#' which is subtracted from the spectra to perform a baseline correction
#' \insertCite{Lasch.2012}{ir}.
#'
#' @inheritParams ir_bc_rubberband
#' @param ... Arguments passed to \code{\link{ir_smooth}} (except for
#' \code{method} which is always set to \code{"sg"}.
#' @references
#' \insertAllCited{}
#' @examples
#' x <-
#'    ir::ir_sample_data %>%
#'    ir::ir_bc_sg(p = 3, n = 199, ts = 1, m = 0, return_bl = FALSE)
#' @export
ir_bc_sg <- function(x, ..., return_bl = FALSE) {

  x_bl <- ir::ir_smooth(x, method = "sg", ...)
  if(return_bl) {
    x_bl
  } else {
    ir::ir_subtract(x, x_bl)
  }

}

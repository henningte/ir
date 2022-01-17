#' Performs baseline correction on infrared spectra
#'
#' \code{ir_bc} performs baseline correction for infrared
#' spectra. Baseline correction
#' is either performed by using a polynomial with user defined
#' degree fitted to each spectrum (see
#' \code{\link[ChemoSpec:baselineSpectra]{baselineSpectra}}) or by using a rubberband
#' function that is fitted to each spectrum
#' (see \code{\link[hyperSpec:spc.rubberband]{spc.rubberband}}).
#'
#' @param x An object of class \code{\link[ir:ir_new_ir]{ir}}.
#' @param method A character value indicating which method should be used
#' for baseline correction. If \code{method = "polynomial"}, a polynomial
#' is used for baseline correction. If \code{method = "rubberband"}, a
#' rubberband function is used for baseline correction.
#' @param degree An integer value representing the degree of the polynomial
#' used for baseline correction if \code{method = "polynomial"}.
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
#' @export
ir_bc <- function(x,
                  method = "rubberband",
                  degree = 2,
                  return_bl = FALSE) {

  # checks
  ir_check_ir(x)
  if(!(is.logical(return_bl) |
       length(return_bl) == 1)){
    rlang::abort("`return_bl` must be a logical value")
  }
  if(!(is.integer(degree) |
       length(degree) == 1)){
    rlang::abort("`degree` must be an integer value")
  }

  # perform baseline correction
  switch(
    method,
    polynomial = {
      ir_bc_polynomial(
        x,
        degree = degree,
        return_bl = return_bl
      )
    },
    rubberband = {
      ir_bc_rubberband(
        x,
        return_bl = return_bl
      )
    },
    {rlang::abort("Unknown method.")}
  )

}

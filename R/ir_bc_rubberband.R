#' Performs baseline correction on infrared spectra using a rubberband algorithm.
#'
#' \code{ir_bc_rubberband} performs baseline correction for infrared
#' spectra using a rubberband algorithm. \code{ir_bc_rubberband} is an
#' extended wrapper function for \code{\link[hyperSpec:spc.rubberband]{spc.rubberband}}.
#'
#' @param x An object of class \code{\link[ir:ir_new_ir]{ir}}
#' @param return_bl A logical value indicating if for each spectrum the baseline
#' should be returned in addition to the corrected intensity values
#' (\code{return_bl = TRUE}) or not (\code{return_bl = FALSE}).
#' @return An object of class \code{ir} with the baseline
#' corrected spectra and, if \code{returnbl = TRUE},  the baselines.
#' @seealso
#' \code{\link{ir_bc}},
#' \code{\link{ir_bc_polynomial}}.
#' @export
ir_bc_rubberband <- function(x,
                             return_bl = FALSE) {

  # flatten x
  x_flat <- ir_flatten(x = x, measurement_id = as.character(x$measurement_id))
  x_flat_empty <- ir_flat_clean(x = x_flat, return_empty = TRUE)
  x_flat <- ir_flat_clean(x = x_flat, return_empty = FALSE)

  # create a hyperSpec object
  x_hs <- methods::new("hyperSpec",
                       spc = t(x_flat[,-1]),
                       wavelength = x_flat$x)

  # calculate the baseline
  x_bl <- hyperSpec::spc.rubberband(x_hs,
                                    spline = FALSE,
                                    df = 30)@data$spc

  # remove NAs at the beginning and end
  x_bl[is.na(x_bl)] <- 0 # ___ remove if bug in hyperSpec is fixed

  # prepare the baseline as table
  x_bl1 <- x_flat
  x_bl1[, -1] <- t(x_bl)

  # substract the baseline
  x_bc <- x_flat
  x_bc[,-1] <- x_bc[,-1] - x_bl1[,-1]

  x_bl1 <- dplyr::left_join(x_bl1, x_flat_empty, by = "x")
  x_bl1 <- x_bl1[, match(c("x", x$measurement_id), c(colnames(x_bl1)))]
  x_bc <- dplyr::left_join(x_bc, x_flat_empty, by = "x")
  x_bc <- x_bc[, match(c("x", x$measurement_id), c(colnames(x_bc)))]

  # replace the values in x by the baseline corrected values
  x$spectra <- ir_stack(x_bc)$spectra

  # add baselines to x
  if(return_bl) {
    x$baselines <- ir_stack(x_bl1)$spectra
  }

  x

}

#' Standard Normal Variate correction for `ir` objects
#'
#' @param x An object of class [`ir`][ir_new_ir()] containing the spectra to
#' correct.
#'
#' @param return_ir_flat Logical value. If `TRUE`, the spectra are returned as
#' [`ir_flat`][ir_new_ir_flat()] object.
#'
#' @return `x` after Standard Normal Variate correction (or the same as
#' `ir_flat` object if `return_ir_flat = TRUE`).
#'
#' @source Code adapted from the ['prospectr'](https://cran.r-project.org/web/packages/prospectr) package.
#'
#' @examples
#' x1 <-
#'   ir_sample_data %>%
#'   ir_snv()
#'
#' @export
ir_snv <- function(x, return_ir_flat = FALSE) {

  if(!is.logical(return_ir_flat) | length(return_ir_flat) != 1) {
    rlang::abort('`return_ir_flat` must be a logical value.')
  }
  ir_check_ir(x)

  x_flat <- ir_flatten(x)
  x_flat[, -1] <- sweep(x_flat[, -1], 2, colMeans(x_flat[, -1], na.rm = TRUE), "-")
  x_flat[, -1] <- sweep(x_flat[, -1], 2, apply(x_flat[, -1], 2, sd, na.rm = TRUE), "/")

  res <-
    if(return_ir_flat) {
      x_flat
    } else {
      x$spectra <-
        ir_stack(x_flat)$spectra
      x
    }

  res

}

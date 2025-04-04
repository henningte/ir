#' Interpolates intensity values of infrared spectra in an `ir` object for new wavenumber values
#'
#' `ir_interpolate` interpolates intensity values for infrared spectra for
#' new wavenumber values.
#'
#' @param x An object of class [`ir`][ir_new_ir()].
#'
#' @param start A numerical value indicating the start wavenumber value relative
#' to which new wavenumber values will be interpolated. The value is not allowed
#' to be < `floor(firstvalue) - 2`, whereby `firstvalue` is the first
#' wavenumber value within `x`. If `start = NULL`,
#' `floor(firstvalue)` will be used as first wavenumber value.
#'
#' @param dw A numerical value representing the desired wavenumber value
#' difference between adjacent values.
#'
#' @param return_ir_flat Logical value. If `TRUE`, the spectra are returned as
#' [`ir_flat`][ir_new_ir_flat()] object.
#'
#' @return An object of class `ir` (or `ir_flat`, if `return_ir_flat = TRUE`),
#' containing the interpolated spectra. Any `NA` values resulting from
#' interpolation will be automatically dropped.
#'
#' @examples
#' x <-
#'    ir::ir_sample_data |>
#'    ir::ir_interpolate(start = NULL, dw = 1)
#'
#' @export
ir_interpolate <- function(x, start = NULL, dw = 1, return_ir_flat = FALSE) {

  # checks
  if(!is.logical(return_ir_flat) | length(return_ir_flat) != 1) {
    rlang::abort('`return_ir_flat` must be a logical value.')
  }
  .start <- eval(match.call()$start, parent.frame()) # avoid confusion with function `start()`
  ir_check_ir(x)
  empty_spectra <- ir_identify_empty_spectra(x)
  if(all(empty_spectra)) {
    return(x)
  }
  # new
  x_flat <- ir::ir_flatten(x)
  x_flat <- x_flat[order(x_flat$x), ]
  x_range_max <-
    tibble::tibble(
      start = min(x_flat$x),
      end = max(x_flat$x)
    )

  stopifnot(is.null(.start) || (is.numeric(.start) && length(.start == 1)))
  if(is.null(.start)) {
    .start <- floor(x_range_max$start)
  }
  if(x_range_max$start < .start) {
    rlang::abort("`.start` must not be smaller than the smallest x axis value of any spectrum in `x` (", x_range_max$start, ").")
  }

  # define the new x axis values
  wavenumber_new <- seq(from = .start, to = x_range_max$end, by = dw)
  n_wavenumber_new <- length(wavenumber_new)

  x_flat_new <-
    matrix(NA, nrow = n_wavenumber_new, ncol = ncol(x_flat)) %>%
    as.data.frame() %>%
    stats::setNames(nm = colnames(x_flat)) %>%
    dplyr::mutate(
      x = wavenumber_new
    )

  for(i in seq_len(ncol(x_flat))[-1]) {
    if(sum(! is.na(x_flat[, i, drop = TRUE])) <= 1) {
      next;
    }
    x_flat_new[, i] <-
      stats::approx(
        x = x_flat$x,
        y = x_flat[, i, drop = TRUE],
        xout = x_flat_new$x,
        method = "linear",
        rule = 1,
        ties = "ordered"
      )$y
  }

  res <-
    if(return_ir_flat) {
      x_flat_new
    } else {
      x$spectra <-
        ir::ir_stack(x_flat_new)$spectra
      x
    }

  res

}

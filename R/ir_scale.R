#' Scales spectra in an `ir` object
#'
#' @param x An object of class `ir`, where all non-empty spectra have identical
#' wavenumber values.
#'
#' @inheritParams base::scale
#'
#' @return `x` where spectra have been scaled, i.e. from each intensity value,
#' the average across all spectra is subtracted (when `center` is a logical
#' value), or the respective value in `center` is subtracted (when `center` is
#' numerical), and each intensity value is divided by the standard deviation of
#' the intensity values at this wavenumber across all spectra (when `scale` is a
#' logical value), or the respective value in `scale` (when `scale` is
#' numerical). `NA`s are omitted during this process.
#'
#' @examples
#' ir_sample_data %>%
#'  ir_scale() %>%
#'  plot()
#'
#' @export
ir_scale <- function(x, center = TRUE, scale = TRUE) {

  ir_check_ir(x)
  spectrum_is_empty <- ir_check_for_empty_spectra(x)
  if(all(spectrum_is_empty)) {
    return(spectrum_is_empty)
  }
  stopifnot(all(purrr::map_lgl(x$spectra, function(.x) {
      identical(.x$x, x$spectra[!spectrum_is_empty][[1]]$x) || nrow(.x) == 0
  })))

  x_flat <-
    ir::ir_flatten(x)
  res <- scale(t(x_flat[, -1]), center = center, scale = scale)
  x_flat[, -1] <- t(res)

  x$spectra <-
    ir::ir_stack(x_flat)$spectra

  x %>%
    structure("scaled:center" = attr(res, "scaled:center"), "scaled:scale" = attr(res, "scaled:scale"))

}

#' Interpolates intensity values of infrared spectra for new wavenumber values
#'
#' \code{ir_interpolate} interpolates intensity values for infrared spectra for
#' new wavenumber values.
#'
#' @param x An object of class \code{\link[ir:ir_new_ir]{ir}}.
#' @param start A numerical value indicating the start wavenumber value relative
#' to which new wavenumber values will be interpolated. The value is not allowed
#' to be < \code{floor(firstvalue) - 2}, whereby \code{firstvalue} is the first
#' wavenumber value within \code{x}. If \code{start = NULL},
#' \code{floor(firstvalue)} will be used as first wavenumber value.
#' @param dw A numerical value representing the desired wavenumber value
#' difference between adjacent values.
#' @return An object of class \code{ir} containing the interpolated spectra. Any
#' \code{NA} values resulting from interpolation will be automatically dropped.
#' @examples
#' x <-
#'    ir::ir_sample_data %>%
#'    ir::ir_interpolate(start = NULL, dw = 1)
#' @export
ir_interpolate <- function(x, start = NULL, dw = 1) {

  # checks
  .start <- match.call()$start # avoid confusion with function `start()`
  ir_check_ir(x)
  x_range_max <-
    x %>%
    ir_drop_unneccesary_cols() %>%
    range(.dimension = "x", .col_names = c("x_min", "x_max"), na.rm = TRUE) %>%
    dplyr::summarise(
      start = min(.data$x_min),
      end = max(.data$x_max)
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
  x <-
    x %>%
    dplyr::mutate(
      spectra = purrr::map(.data$spectra, dplyr::arrange, .data$x)
    )

  # do the interpolation
  x %>%
    dplyr::mutate(
      spectra = purrr::map(.data$spectra, function(z) {

        x_new <- wavenumber_new

        if(all(is.na(z$y))) {
          y_new <- rep(NA_real_, n_wavenumber_new)
        } else {
          y_new <-
            stats::approx(
              x = z$x,
              y = z$y,
              xout = x_new,
              method = "linear",
              rule = 1,
              ties = "ordered"
            )$y
        }

        tibble::tibble(
          x = x_new,
          y = y_new
        ) %>%
          dplyr::filter(!is.na(.data$y))

      })
    )

}

#' Extracts intensities from spectra for specific x axis values
#'
#' `ir_get_intensity` extracts intensity values of spectra for specific
#' user-defined x axis values (e.g. wavenumber values).
#'
#' @param x An object of class [`ir`][ir_new_ir()].
#' @param wavenumber A numeric vector with x axis values (e.g. wavenumber
#' values) for which to extract intensities.
#' @param warn logical value indicating if warnings should be displayed
#' (`TRUE`) or not (`FALSE`).
#' @return `x` with an additional column `intensity`.
#' `x$intensity` is a list column with each element representing a
#' `data.frame` with a row for each element in `wavenumber` and two
#' columns:
#' \describe{
#'   \item{x}{The x axis values extracted with
#'   [ir_get_wavenumberindex()] applied on `wavenumber` and the
#'   corresponding spectrum in `x`.}
#'   \item{y}{The extracted intensity values}.
#' }
#' @examples
#' x <-
#'    ir::ir_sample_data %>%
#'    ir::ir_get_intensity(wavenumber = 1090)
#' @export
ir_get_intensity <- function(x,
                             wavenumber,
                             warn = TRUE) {

  # checks
  ir_check_ir(x)

  x$intensity <-
    purrr::map(x$spectra, function(d) {
      tibble::tibble(
        x = wavenumber,
        y = d$y[ir_get_wavenumberindex(d, wavenumber = wavenumber, warn = warn)]
      )
    })
  x

}

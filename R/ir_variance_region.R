#' Computes the variance of a spectrum in a given region
#'
#' \code{ir_variance_region} takes a spectrum \code{x} and, depending on the
#' arguments computes the following summary:
#' \describe{
#'   \item{if \code{subtract_smoothed = FALSE}}{it computes the variance of the
#'   intensity values for each spectrum in \code{x}. If in addition \code{range}
#'   is not \code{NULL}, it computes the variance only for the region(s)
#'   represented by \code{range}.}
#'   \item{if \code{subtract_smoothed = TRUE}}{it smoothes \code{x}, subtracts
#'   the smoothed \code{x} from the unsmoothed \code{x} and computes the
#'   variance of the difference intensity values. If in addition \code{range} is
#'   not \code{NULL}, it computes the variance only for the region(s)
#'   represented by \code{range}.}
#' }
#'
#' @details The computed variance is always the variance of normalized spectra
#' (\code{ir::ir_normalize(method = "area")}).
#'
#' @param x An object of class \code{\link{ir}}. These are the spectra for which
#' to compute the variance.
#' @param subtract_smoothed A logical value. If \code{subtract_smoothed = TRUE},
#' \code{x} is copied, the copy smoothed using \code{ir_smooth} with
#' \code{method = "sg"} and subtracted from \code{x} before the variance of the
#' intensity values from \code{x} is computed. This allows e.g. to estimate the
#' noise level in a specific region of spectra. If
#' \code{subtract_smoothed = FALSE} (the default), nothing is subtracted from
#' \code{x} before computing the variance of the intensity values.
#' @param ... Arguments passed to \code{\link{ir_smooth}} (except for
#' \code{method} which is always set to \code{sg} if \code{subtract_smoothed} is
#' \code{TRUE}). If \code{subtract_smoothed = FALSE}, these arguments will be
#' ignored.
#' @param range See \code{\link{ir_clip}}. This is the range for which the
#' variance of the intensity values will be computed.
#' @return \code{x} with two additional columns:
#' \describe{
#'   \item{variance}{A numeric vector with the computed variances of the
#'   intensity values for the respective spectra.}
#'   \item{n_variance}{An integer vector with the number of intensity values
#'   used during computing the variance.}
#' }
#' @examples
#' # Whole spectra variance
#' x1 <-
#'    ir::ir_sample_data %>%
#'    ir::ir_variance_region(subtract_smoothed = FALSE, range = NULL)
#'
#' # Spectra variance, but only from a specific region
#' range <- data.frame(start = 2700, end = 2800)
#'
#' x2 <-
#'    ir::ir_sample_data %>%
#'    ir::ir_variance_region(subtract_smoothed = FALSE, range = range)
#'
#' # Spectra variance after subtracting a smoothed version of the spectra and
#' # only from a specific region
#' x3 <-
#'    ir::ir_sample_data %>%
#'    ir::ir_variance_region(subtract_smoothed = FALSE, range = range,
#'                           p = 3, n = 31, ts = 1, m = 0)
#' @export
ir_variance_region <- function(x, subtract_smoothed = FALSE, ..., range = NULL) {

  ir_check_ir(x)
  stopifnot(length(subtract_smoothed) == 1 && is.logical(subtract_smoothed))
  stopifnot(is.null(range) || is.data.frame(range))

  if(subtract_smoothed) {
    y <-
      x %>%
      ir::ir_smooth(method = "sg", ...)
    res <-
      ir::ir_subtract(x, y) %>%
      ir::ir_normalise()
  } else {
    res <- x
  }

  if(!is.null(range)) {
    res <-
      res %>%
      ir::ir_clip(range = range)
  }

  # compute variance
  res <-
    purrr::map_dfr(res$spectra, function(y) {
      tibble::tibble(
        n_variance = nrow(y),
        variance = stats::var(y$y)
      )
    })

  dplyr::bind_cols(x, res)

}

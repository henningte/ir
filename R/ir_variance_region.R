#' Computes the variance of a spectrum in a given region
#'
#' `ir_variance_region` takes a spectrum `x` and, depending on the
#' arguments computes the following summary:
#' \describe{
#'   \item{if `subtract_smoothed = FALSE`}{it computes the variance of the
#'   intensity values for each spectrum in `x`. If in addition `range`
#'   is not `NULL`, it computes the variance only for the region(s)
#'   represented by `range`.}
#'   \item{if `subtract_smoothed = TRUE`}{it smoothes `x`, subtracts
#'   the smoothed `x` from the unsmoothed `x` and computes the
#'   variance of the difference intensity values. If in addition `range` is
#'   not `NULL`, it computes the variance only for the region(s)
#'   represented by `range`.}
#' }
#'
#' @param x An object of class [`ir`][ir_new_ir()]. These are the spectra for which
#' to compute the variance.
#' @param subtract_smoothed A logical value. If `subtract_smoothed = TRUE`,
#' `x` is copied, the copy smoothed using `ir_smooth` with
#' `method = "sg"` and subtracted from `x` before the variance of the
#' intensity values from `x` is computed. This allows e.g. to estimate the
#' noise level in a specific region of spectra. If
#' `subtract_smoothed = FALSE` (the default), nothing is subtracted from
#' `x` before computing the variance of the intensity values.
#' @param do_normalize A logical value. If set to `TRUE`, the spectra in
#' `x` are normalized after subtraction of a smoothed version, else no
#' normalization is performed.
#' @param normalize_method See [ir_normalize()].
#' @param ... Arguments passed to [ir_smooth()] (except for
#' `method` which is always set to `"sg"` if `subtract_smoothed` is
#' `TRUE`). If `subtract_smoothed = FALSE`, these arguments will be
#' ignored.
#' @param range See [ir_clip()]. This is the range for which the
#' variance of the intensity values will be computed.
#' @return `x` with two additional columns:
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
#'    ir::ir_variance_region(
#'       subtract_smoothed = FALSE,
#'       do_normalize = TRUE,
#'       normalize_method = "area",
#'       range = NULL
#'    )
#'
#' # Spectra variance, but only from a specific region
#' range <- data.frame(start = 2700, end = 2800)
#'
#' x2 <-
#'    ir::ir_sample_data %>%
#'    ir::ir_normalize(method = "area") %>%
#'    ir::ir_variance_region(
#'       subtract_smoothed = FALSE,
#'       do_normalize = TRUE,
#'       normalize_method = "area",
#'       range = range
#'    )
#'
#' # Spectra variance after subtracting a smoothed version of the spectra and
#' # only from a specific region
#' x3 <-
#'    ir::ir_sample_data %>%
#'    ir::ir_variance_region(
#'       subtract_smoothed = TRUE,
#'       do_normalize = FALSE,
#'       range = range,
#'       p = 3, n = 31, ts = 1, m = 0
#'    )
#' @export
ir_variance_region <- function(x, subtract_smoothed = FALSE, do_normalize = FALSE, normalize_method, ..., range = NULL) {

  ir_check_ir(x)
  stopifnot(length(subtract_smoothed) == 1 && is.logical(subtract_smoothed))
  stopifnot(is.null(range) || is.data.frame(range))
  stopifnot(length(do_normalize) == 1 && is.logical(do_normalize))
  if(do_normalize) {
    stopifnot(is.character(normalize_method) && length(normalize_method) == 1)
  }

  if(subtract_smoothed) {
    y <-
      x %>%
      ir::ir_smooth(method = "sg", ...)
    res <-
      ir::ir_subtract(x, y)
  } else {
    res <- x
  }

  if(do_normalize) {
    res <-
      res %>%
      ir::ir_normalize(method = normalize_method)
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

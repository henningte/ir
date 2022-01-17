#' Clips infrared spectra to new wavenumber ranges
#'
#' \code{ir_clip} clips infrared spectra to a new, specified, wavenumber range
#' or multiple new specified wavenumber ranges.
#'
#' @param x An object of class \code{\link[ir:ir_new_ir]{ir}}.
#' @param range A \code{data.frame} with two columns and a row for each
#' wavenumber range to keep. The columns are:
#' \describe{
#'   \item{start}{A numeric vector with start values for wavenumber ranges.}
#'   \item{end}{A numeric vector with end values for wavenumber ranges.}
#' }
#' If \code{range} has more than one row, multiple ranges are clipped from
#' \code{x} and merged together. Overlapping ranges are not allowed.
#' @return An object of class \code{ir}.
#' @examples
#' ## clipping with one range
#'
#' # define clipping range
#' range <-
#'   data.frame(start = 900, end = 1000)
#'
#' # clip
#' x <-
#'    ir::ir_sample_data %>%
#'    ir::ir_clip(range = range)
#'
#' ## clipping with mutliple ranges
#'
# # define clipping range
#' range <-
#'   data.frame(start = c(900, 1900), end = c(1000, 2200))
#'
#' # clip
#' x <-
#'    ir::ir_sample_data %>%
#'    ir::ir_clip(range = range)
#' @export
ir_clip <- function(x,
                    range) {

  # checks
  ir_check_ir(x)
  if(!inherits(range, "data.frame")) {
    rlang::abort("`range` must be a data.frame.")
  }
  if(ncol(range) != 2) {
    rlang::abort("`range` must have two columns.")
  }
  range_check <- apply(range, 1, function(x) {
    x[[1]] >= x[[2]]
  })
  if(any(range_check)) {
    rlang::abort(paste0("For each row in `range`, `range$start` must be smaller than `range$end`. This is not the case for row(s) ", which(range_check), "."))
  }

  range <- range[order(range[, 1, drop = TRUE], decreasing = FALSE), ]

  # detect the corresponding row indices
  x_flat <- ir_flatten(x)
  range_nrow <- nrow(range)
  range <- ir_get_wavenumberindex(x_flat, wavenumber = as.matrix(range), warn = TRUE)
  range <- matrix(range, byrow = FALSE, nrow = range_nrow)
  index <- unlist(apply(range, 1, function(x) x[[1]]:x[[2]]))

  # clip x
  x$spectra <- ir_stack(x_flat[index, ])$spectra
  x

}

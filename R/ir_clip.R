#' Clips infrared spectra to new wavenumber ranges
#'
#' `ir_clip` clips infrared spectra to a new, specified, wavenumber range
#' or multiple new specified wavenumber ranges.
#'
#' @param x An object of class [`ir`][ir_new_ir()].
#'
#' @param range A `data.frame` with two columns and a row for each
#' wavenumber range to keep. The columns are:
#' \describe{
#'   \item{start}{A numeric vector with start values for wavenumber ranges.}
#'   \item{end}{A numeric vector with end values for wavenumber ranges.}
#' }
#' If `range` has more than one row, multiple ranges are clipped from
#' `x` and merged together. Overlapping ranges are not allowed.
#'
#' @param return_ir_flat Logical value. If `TRUE`, the spectra are returned as
#' [`ir_flat`][ir_new_ir_flat()] object.
#'
#' @return An object of class `ir` (or `ir_flat`, if `return_ir_flat = TRUE`)
#' where spectra have been clipped.
#'
#' @examples
#' ## clipping with one range
#'
#' # define clipping range
#' range <-
#'   data.frame(start = 900, end = 1000)
#'
#' # clip
#' x <-
#'    ir::ir_sample_data |>
#'    ir::ir_clip(range = range)
#'
#' ## clipping with mutliple ranges
#'
#' # define clipping range
#' range <-
#'   data.frame(start = c(900, 1900), end = c(1000, 2200))
#'
#' # clip
#' x <-
#'    ir::ir_sample_data |>
#'    ir::ir_clip(range = range)
#' @export
ir_clip <- function(x, range, return_ir_flat = FALSE) {

  # checks
  ir_check_ir(x)
  empty_spectra <- ir_identify_empty_spectra(x)
  if(all(empty_spectra)) {
    return(x)
  }
  if(!is.logical(return_ir_flat) | length(return_ir_flat) != 1) {
    rlang::abort('`return_ir_flat` must be a logical value.')
  }
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

  # get clipping indices
  x_flat <- ir_flatten(x)
  indices <-
    purrr::map(seq_len(nrow(range)), function(i) {
      x_flat$x >= range$start[[i]] & x_flat$x <= range$end[[i]]
    }) %>%
    purrr::reduce(`+`)

  # clip
  res <- x_flat[indices > 0, ]

  res <-
    if(return_ir_flat) {
      res
    } else {
      x$spectra <- ir::ir_stack(res)$spectra
      x
    }

  res

}



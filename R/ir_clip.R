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
#' @return An object of class `ir` where spectra have been clipped.
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
ir_clip <- function(x, range) {

  # checks
  ir_check_ir(x)
  empty_spectra <- ir_identify_empty_spectra(x)
  if(all(empty_spectra)) {
    return(x)
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

  # detect the corresponding row indices
  range_nrow <- nrow(range)

  indices <-
    purrr::map2(x$spectra, empty_spectra, function(z, .y) {
      if(.y) {
        NA
      } else {
        z_range <- ir_get_wavenumberindex(z, wavenumber = as.matrix(range), warn = TRUE)
        z_range <- matrix(z_range, byrow = FALSE, nrow = range_nrow)
        unlist(apply(z_range, 1, function(x) x[[1]]:x[[2]]))
      }
    })

  # clip
  x %>%
    dplyr::mutate(
      spectra =
        purrr::map2(.data$spectra, !!indices, function(z, i) {
          z %>% dplyr::slice(i)
        })
    )

}



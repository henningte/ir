#' Interpolates selected regions in infrared spectra in an `ir` object
#'
#' `ir_interpolate_region` linearly interpolates a user-defined region in
#' infrared spectra.
#'
#' @param x An object of class [`ir`][ir_new_ir()].
#'
#' @param range A `data.frame` with a row for each region to interpolate
#' linearly and two columns:
#' \describe{
#'   \item{start}{A numeric vector with start values for regions to interpolate
#'   linearly (x axis values).}
#'   \item{end}{A numeric vector with end values for regions to interpolate
#'   linearly (x axis values).}
#' }
#' For each row in `range`, the values in `range$start` have to be
#' smaller than the values in `range$end`.
#'
#' @return `x` with the defined wavenumber region(s) interpolated linearly.
#'
#' @examples
#' # interpolation range
#' range <- data.frame(start = 1000, end = 1500)
#'
#' # do the interpolation
#' x <-
#'    ir::ir_sample_data %>%
#'    ir::ir_interpolate_region(range = range)
#'
#' @export
ir_interpolate_region <- function(x, range) {

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
  range_nrow <- nrow(range)

  x_ranges <-
    purrr::map(x$spectra, function(z) {
      if(nrow(z) == 0) {
        return(NA)
      }
      z_range <- ir_get_wavenumberindex(z, wavenumber = as.matrix(range), warn = TRUE)
      z_range <- matrix(z_range, byrow = FALSE, nrow = range_nrow)
      purrr::map(seq_len(nrow(z_range)), function(x) z_range[x, ][[1]]:z_range[x, ][[2]])
    })

  x %>%
    dplyr::mutate(
      spectra =
        purrr::map2(.data$spectra, x_ranges, function(z, i) {
          y_new <-
            purrr::map(i, function(j) {
              if(all(is.na(z$y))) {
                y_new <- rep(NA_real_, length(j))
              } else {
                d <- z[j, ]
                m <- stats::lm(y ~ x, data = d[c(1, nrow(d)), , drop = FALSE])
                y_new <- stats::predict(m, newdata = d)
              }
            }) %>%
            unlist()
          z$y[unlist(i)] <- y_new
          z
        })
    )

}

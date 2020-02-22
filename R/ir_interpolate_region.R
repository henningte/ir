#' Interpolates selected regions in infrared spectra.
#'
#' \code{ir_interpolate_region} linearly interpolates a user-defined
#' region in infrared spectra.
#'
#' @param x An object of class \code{\link[ir:ir_new_ir]{ir}}.
#' @param range A \code{data.frame} with a row for each region to interpolate
#' linearly and two columns:
#' \describe{
#'   \item{start}{A numeric vector with start values for regions to interpolate
#'   linearly (x axis values).}
#'   \item{end}{A numeric vector with end values for regions to interpolate
#'   linearly (x axis values).}
#' }
#' For each row in \code{range}, the values in \code{range$start} have to be
#' smaller than the values in \code{range$end}.
#' @return \code{x} with the defined wavenumber region(s) interpolated linearly.
#' @export
ir_interpolate_region <- function(x,
                                  range) {

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

  x_flat <- ir_flatten(x)
  index <- ir_get_wavenumberindex(x_flat,
                                  wavenumber = as.matrix(range),
                                  warn = TRUE)
  index <- matrix(index, byrow = FALSE, nrow = nrow(range))
  x_ranges <- purrr::map(seq_len(nrow(index)), function(x) index[x, ][[1]]:index[x, ][[2]])

  for(index in x_ranges) {

    x_flat[index, -1] <- purrr::map_df(x_flat[, -1, drop = FALSE], function(y){
      d <- data.frame(y = y[index], z = x_flat[index, 1, drop = TRUE])
      if(all(is.na(d$y))) {
        rep(NA_real_, length(index))
      } else {
        m <- stats::lm(y ~ z, data = d[c(1, nrow(d)), , drop = FALSE])
        stats::predict(m, newdata = d)
      }
    })

  }

  x$spectra <- ir_stack(x_flat)$spectra
  x

}

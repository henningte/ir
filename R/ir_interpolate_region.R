#' Interpolates selected regions in infrared spectra.
#'
#' \code{ir_interpolate_region} linearly interpolates a user-defined
#' region in infrared spectra.
#'
#' @param x An object of class \code{\link[ir:ir_new_ir]{ir}}.
#' @param region A numerical vector of length two with the first value
#' representing the start (smaller) wavenumber value of the region and the
#' second value representing the end (larger) wavenumber value of the region
#' to interpolate.
#' @return \code{x} with the defined wavenumber region interpolated linearly.
#' @export
ir_interpolate_region <- function(x,
                                  region) {

  ir_check_ir(x)
  if(!is.numeric(region)) {
    rlang::abort("`region` must be numeric, not ", class(region)[[1]], ".")
  }
  if(length(region) != 2) {
    rlang::abort("`region` must be of length 2, not ", length(region), ".")
  }

  x_flat <- ir_flatten(x)
  index <- ir_get_wavenumberindex(x_flat,
                                  region,
                                  warn = TRUE)

  x_range <- index[1]:index[2]

  intensity_missing <- purrr::map_lgl(x_flat[, -1, drop = FALSE], function(y){
    d <- data.frame(y = y[index], z = x_flat[index, 1, drop = TRUE])
    all(is.na(d$y))})
  if(any(intensity_missing)) {
    rlang::warn(if(sum(intensity_missing) == 1) {
      paste0("Spectrum ", which(intensity_missing), " has only NA values at the start and end of `region` and therefore was not inerpolated.")
    }else {
      paste0("Spectra ", which(intensity_missing), " have only NA values at the start and end of `region` and therefore were not inerpolated.")
    })
  }

  x_flat[x_range, -1] <- purrr::map_df(x_flat[, -1, drop = FALSE], function(y){
    d <- data.frame(y = y[index], z = x_flat[index, 1, drop = TRUE])
    if(all(is.na(d$y))) {
      rep(NA_real_, length(x_range))
    } else {
      m <- stats::lm(y ~ z, data = d)
      stats::predict(m, newdata = data.frame(z = x_flat[x_range, 1, drop = TRUE]))
    }
  })

  x$spectra <- ir_stack(x_flat)$spectra
  x

}

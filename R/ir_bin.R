#' Bins infrared spectra.
#'
#' \code{ir_bin} bins intensity values of infrared spectra into bins of a
#' defined width or into a defined number of bins.
#'
#' If the last bin contains fewer input values than the remaining bins, it
#' will be dropped and a warning will be printed.
#'
#' @param x An object of class \code{\link[ir:ir_new_ir]{ir}} with integer wavenumber
#' values increasing by 1.
#' @param width An integer value indicating the wavenumber width of
#' each resulting bin. Must be set to \code{NULL} if \code{n} is
#' specified.
#' @return An object of class \code{ir} representing a binned version of \code{x}.
#' @export
ir_bin <- function(x,
                   width = 10) {

  # checks
  if(!inherits(x, "ir")) {
    rlang::abort(paste0("`x` must be of class ir, not ", class(x)[[1]], "."))
  }
  if(!is.numeric(width)) {
    rlang::abort("`width` must be numeric.")
  }
  if(length(width) != 1) {
    stop("width must be of length 1.")
  }
  if(width %% 1 != 0) {
    stop("width must be an integer.")
  }

  x_flat <- ir_flatten(x)

  # define bins
  nbins <- diff(range(x_flat$x)) %/% width
  bins_wn <- tibble::tibble(start = seq(0, nbins * width, width) + min(x_flat$x),
                        end = .data$start + width - 1)

  bins_index <- purrr::map2_df(bins_wn$start, bins_wn$end, function(x, y){
    tibble::tibble(start = which(x_flat$x >= x)[[1]], end = rev(which(x_flat$x <= y))[[1]])
  })

  # check how many last entries of x must be dropped
  n_drop <- nrow(x_flat) - bins_index$end[nrow(bins_index)]
  if(n_drop > 0) {
    rlang::warn(paste0("Dropping the last ", n_drop, " values of `x` during binning."))
  }

  # perform binning
  x_binned <- cbind(x = apply(bins_wn, 1, mean),
                    t(apply(bins_index, 1, function(z1){
                      apply(x_flat[,-1], 2, function(z2){
                        mean(as.numeric(z2[z1[1]:z1[2]]), na.rm = TRUE)
                      })
                    }))
                    )

  x$spectra <- ir_stack(x_binned)$spectra
  x

}

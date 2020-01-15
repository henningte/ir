#' Smoothes infrared spectra.
#'
#' \code{ir_smooth} applies smoothing functions to infrared spectra.
#' \code{ir_smooth} either performs
#' Savitzky-Golay smoothing, based on \code{\link[signal:sgolayfilt]{sgolayfilt}},
#' or Fourier smoothing using a user-defined smoothing function.
#'
#' @param x An object of class \code{\link[ir:ir_new_ir]{ir}}.
#' @param method A character value specifying which smoothing method to apply.
#' If \code{method = "sg"}, a Savitzky-Golay filter will be applied on the
#' spectra. The Savitzky-Golay smoothing will be performed using the function
#' \code{\link[signal:sgolayfilt]{sgolayfilt}}.
#' If \code{method = "fourier"}, Fourier smoothing will be performed.
#' Fourier transformation of the spectra is performed using the fast
#' discrete Fourier transformation (FFT) as implemented in
#' \code{\link[stats:fft]{fft}}. A smoothing function can be defined by the
#' argment \code{f}.
#' @param k A positive odd integer representing the number of Fourier
#' basis functions to use as smoothed representation of the spectra
#' if \code{method = "fourier"}.
#' @param p An integer value representing the filter order (i.e. the degree of the
#' polynom) of the Savitzky-Golay filter if \code{method = "sg"}.
#' @param n An odd integer value representing the length (i.e. the
#' number of wavenumber values used to construct the polynom) of the
#' Savitzky-Golay filter if \code{method = "sg"}.
#' @param ts time scaling factor. See \code{link[signal:sgolayfilt]{sgolayfilt}}.
#' @param ... additional arguments (ignored).
#' @return An object of class \code{ir} containing the smoothed
#' spectra.
#' @export
ir_smooth <- function(x,
                      method = "sg",
                      p = 3,
                      n = p + 3 - p %% 2,
                      ts = 1,
                      k = 111) {

  # checks
  if(!inherits(x, "ir")) {
    rlang::abort(paste0("`x` must be of class ir, not ", class(x)[[1]],"."))
  }
  if(!(is.character(method) & method %in% c("sg", "fourier"))){
    stop("`method` must be one of 'sg' or 'fourier'.")
  }

  x_flat <- ir_flatten(x)

  # smooth the spectra
  switch(method,
         sg = {
           x_flat[,-1] <- apply(x_flat[,-1], 2, function(x){signal::sgolayfilt(x = x, p = p, n = n, ts = ts)})
           },
         fourier = {

           # define Fourier basis object
           fourier_basis <- fda::create.fourier.basis(rangeval = range(x_flat$x), nbasis = k)

           # functional data object
           fd <- fda::smooth.basis(argvals = x_flat$x,
                                   y = as.matrix(x_flat[, -1]),
                                   fdParobj = fourier_basis)$fd

           # smoothing
           x_flat[, -1] <- fda::eval.fd(x_flat$x, fd)
         })

  # return the smoothed x
  x$spectra <- ir_stack(x_flat)$spectra
  x

}

#' Smooths infrared spectra in an `ir` object
#'
#' `ir_smooth` applies smoothing functions to infrared spectra.
#' `ir_smooth` either performs Savitzky-Golay smoothing, using on
#' [signal::sgolayfilt()], or Fourier smoothing using
#' [fda::smooth.basis()]. Savitzky-Golay smoothing can
#' also be used to compute derivatives of spectra.
#'
#' @details When `x` contains spectra with different wavenumber values, the
#' filters are applied for each spectra only on existing wavenumber values. This
#' means that the filter window (if `method == "sg"`) will be different for
#' these different spectra.
#'
#' @param x An object of class [`ir`][ir_new_ir()].
#'
#' @param method A character value specifying which smoothing method to apply.
#' If `method = "sg"`, a Savitzky-Golay filter will be applied on the
#' spectra. The Savitzky-Golay smoothing will be performed using the function
#' [signal::sgolayfilt()]. If `method = "fourier"`,
#' Fourier smoothing will be performed. Fourier transformation of the spectra is
#' performed using the fast discrete Fourier transformation (FFT) as implemented
#' in [fda::smooth.basis()]. A smoothing function can be
#' defined by the argment `f`.
#'
#' @param k A positive odd integer representing the number of Fourier basis
#' functions to use as smoothed representation of the spectra if
#' `method = "fourier"`.
#'
#' @param p An integer value representing the filter order (i.e. the degree of
#' the polynom) of the Savitzky-Golay filter if `method = "sg"`.
#'
#' @param n An odd integer value representing the length (i.e. the number of
#' wavenumber values used to construct the polynom) of the Savitzky-Golay filter
#' if `method = "sg"`.
#'
#' @param ts time scaling factor. See [signal::sgolayfilt()].
#'
#' @param m An integer value representing the mth derivative to compute. This
#' option can be used to compute derivatives of spectra. See
#' [signal::sgolayfilt()].
#'
#' @param ... additional arguments (ignored).
#'
#' @return `x` with smoothed spectra.
#'
#' @examples
#' #' # Savitzky-Golay smoothing
#' x1 <-
#'    ir::ir_sample_data %>%
#'    ir::ir_smooth(method = "sg", p = 3, n = 51, ts = 1, m = 0)
#'
#' # Fourier smoothing
#' x2 <-
#'    ir::ir_sample_data %>%
#'    ir::ir_smooth(method = "fourier", k = 21)
#'
#' # computing derivative spectra with Savitzky-Golay smoothing (here: first
#' # derivative)
#' x3 <-
#'    ir::ir_sample_data %>%
#'    ir::ir_smooth(method = "sg", p = 3, n = 51, ts = 1, m = 1)
#'
#' @export
ir_smooth <- function(x,
                      method = "sg",
                      p = 3,
                      n = p + 3 - p %% 2,
                      ts = 1,
                      m = 0,
                      k = 111,
                      ...) {

  # checks
  if(!inherits(x, "ir")) {
    rlang::abort(paste0("`x` must be of class ir, not ", class(x)[[1]],"."))
  }
  if(!(is.character(method) & method %in% c("sg", "fourier"))){
    rlang::abort("`method` must be one of 'sg' or 'fourier'.")
  }

  # smooth the spectra
  switch(
    method,
    sg = {
      x$spectra <-
        purrr::map(x$spectra, function(y) {
          index <- !is.na(y$y)
          y$y[index] <-
            signal::sgolayfilt(x = y$y[index], p = p, n = n, ts = ts, m = m)
          y
        })
    },
    fourier = {
      x$spectra <-
        purrr::map(x$spectra, function(y) {

          index <- !is.na(y$y)

          # define Fourier basis object
          fourier_basis <- fda::create.fourier.basis(rangeval = range(y$x), nbasis = k)

          # functional data object
          fd <-
            fda::smooth.basis(
              argvals = y$x[index],
              y = as.matrix(y$y[index], ncol = 1),
              fdParobj = fourier_basis
            )$fd

          # smoothing
          y$y[index] <- fda::eval.fd(y$x[index], fd)
          y

        })
    },
    {rlang::abort("Unknown method!")}
  )

  x

}

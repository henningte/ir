#' Normalises infrared spectra.
#'
#' \code{ir_normalise} normalises the intensity values of infrared spectra.
#' Spectra can be normalised in three ways:
#' \enumerate{
#'   \item Normalisation so that the intensity values range in [0;1]. Note that
#'   for different spectra, for different wavenumber values the intensity may be
#'   1 after normalisation, depending on the location of the peak with the maximum
#'   height.
#'   \item Normalisation so that the area under the spectral curve sums to 1. Note
#'   that in the case of negative intensities values, these will be count as negative
#'   values during summation.
#'   \item Normalisation so that the intensity at a specified wavenumber value
#'   has value 1 and the minimum intensitiy value is 0.
#' }
#'
#' @param x An object of class \code{\link[ir:ir_new_ir]{ir}}.
#' @param method A character value specifying which normalisation method to apply.
#' If \code{method = "zeroone"}, all intensity values will be normalised to [0;1].
#' If \code{method = "area"}, all intensity values will be divided by the sum of
#' the intensity values at all wavenumber values of the spectrum. If
#' \code{method} is convertable to a numeric value, e.g. \code{method = "980"},
#' the intensity of all spectra at a wavenumber value of 980 will be set to 1 and
#' the minimum intensity value of each spectrum will be set to 0, i.e. the spectra
#' will be normalised refering to a specific wavenumber value.
#' @return An object of class \code{ir} representing a normalised version of \code{x}.
#' @export
ir_normalise <- function(x,
                         method = "area") {

  # checks
  if(!inherits(x, "ir")) {
    rlang::abort(paste0("`x` must be of class ir, not ", class(x)[[1]],"."))
  }
  if(!((is.character(method)|
        is.numeric(method)) &
       length(method) == 1)){
    rlang::abort("`method`` must be a character value or a numeric value.")
  }
  if(is.character(method)){
    if(!(method %in% c("zeroone", "area"))){
      rlang::abort("If specified as character value, `method` must be one of 'zeroone' or 'area'.")
    }
  }
  if(is.numeric(method)) {
    method_wn <- method
    method <- "wavenumber"
  }

  x_flat <- ir_flatten(x)

  # normalising functions
  switch(method,
         # normalise to [0;1]
         zeroone = {
           index <- NULL
           f <- plyr::colwise(function(y, ...){
             y <- y - min(y, na.rm = TRUE)
             y/max(y, na.rm = TRUE)
           })
         },
         # normalise to the area
         area = {
           index <- NULL
           f <- plyr::colwise(function(y, ...){
             y/sum(y, na.rm = TRUE)
           })
         },
         # normalise to a specific wavenumber
         wavenumber = {
           index <- ir_get_wavenumberindex(x_flat,
                                           wavenumber = method_wn,
                                           warn = TRUE)
           f <- plyr::colwise(function(y, ...){
             y <- y - min(y, na.rm = TRUE)
             y/y[index]
           })
         })

  x_flat[,-1] <- f(x_flat[,-1], index)
  x$spectra <- ir_stack(x_flat)$spectra
  x

}

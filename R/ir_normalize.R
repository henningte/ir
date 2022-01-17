#' Normalizes infrared spectra
#'
#' \code{ir_normalise} normalizes the intensity values of infrared spectra.
#' Spectra can be normalized in three ways (value for argument \code{method}):
#' \describe{
#'   \item{"zeroone"}{Normalization so that the intensity values range in [0;1].
#'   Note that for different spectra, for different wavenumber values the
#'   intensity may be 1 after normalization, depending on the location of the
#'   peak with the maximum height.}
#'   \item{"area"}{Normalization so that the area under the spectral curve sums
#'   to 1. Note that in the case of negative intensities values, these will be
#'   count as negative values during summation.}
#'   \item{A numeric value}{Normalization so that the intensity at a specified
#'   wavenumber value has value 1 and the minimum intensity value is 0.}
#' }
#'
#' @param x An object of class \code{\link[ir:ir_new_ir]{ir}}.
#' @param method A character value specifying which normalization method to
#' apply. If \code{method = "zeroone"}, all intensity values will be normalized
#' to [0;1]. If \code{method = "area"}, all intensity values will be divided by
#' the sum of the intensity values at all wavenumber values of the spectrum. If
#' \code{method} is convertible to a numeric value, e.g. \code{method = "980"},
#' the intensity of all spectra at a wavenumber value of 980 will be set to 1
#' and the minimum intensity value of each spectrum will be set to 0, i.e. the
#' spectra will be normalized referring to a specific wavenumber value.
#' @return An object of class \code{ir} representing a normalized version of
#' \code{x}.
#' @examples
#' # with method = "area"
#' x <-
#'    ir::ir_sample_data %>%
#'    ir::ir_normalize(method = "area")
#'
#' # normalizing to a specific peak
#' x <-
#'    ir::ir_sample_data %>%
#'    ir::ir_normalize(method = 1090)
#'
#' @export
ir_normalise <- function(x,
                         method = "area") {

  # checks
  if(!inherits(x, "ir")) {
    rlang::abort(paste0("`x` must be of class ir, not ", class(x)[[1]],"."))
  }
  if(!((is.character(method)| is.numeric(method)) & length(method) == 1)) {
    rlang::abort("`method`` must be a character value or a numeric value.")
  }
  if(is.character(method)) {
    if(!(method %in% c("zeroone", "area"))){
      rlang::abort("If specified as character value, `method` must be one of 'zeroone' or 'area'.")
    }
  }
  if(is.numeric(method)) {
    method_wn <- method
    method <- "wavenumber"
  }

  # normalizing functions
  switch(
    method,
    # normalize to [0;1]
    zeroone = {
      index <- NULL
      f <-
        function(y, ...) {
          y <- y - min(y, na.rm = TRUE)
          y/max(y, na.rm = TRUE)
        }
    },
    # normalize to the area
    area = {
      index <- NULL
      f <-
        function(y, ...) {
          y/sum(y, na.rm = TRUE)
        }
    },
    # normalize to a specific wavenumber
    wavenumber = {
      x_flat <- ir_flatten(x)
      index <-
        ir_get_wavenumberindex(x_flat,
                               wavenumber = method_wn,
                               warn = TRUE)
      f <-
        function(y, ...){
          y <- y - min(y, na.rm = TRUE)
          y/y[index]
        }
    },
    {rlang::abort("Unknown method.")})

  # normalize
  x %>%
    dplyr::mutate(
      spectra = purrr::map(.data$spectra, function(z) {
        z %>%
          dplyr::mutate(
            y = f(.data$y, index)
          )
      })
    )

}

#' @rdname ir_normalise
#' @export
ir_normalize <- ir_normalise

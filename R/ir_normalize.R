#' Normalizes infrared spectra in an `ir` object
#'
#' `ir_normalize` normalizes the intensity values of infrared spectra. Different
#' methods for normalization are available.
#'
#' @name ir_normalize
#'
#' @param x An object of class [`ir`][ir_new_ir()].
#'
#' @param method A character value specifying which normalization method to
#' apply:
#' \describe{
#'   \item{`"zeroone"`}{All intensity values will be normalized to \[0;1\].}
#'   \item{`"area"`}{All intensity values will be divided by the sum of the
#'      intensity values at all wavenumber values of the spectrum.}
#'   \item{`"area_absolute"`}{All intensity values will be divided by the sum
#'      of the intensity values at all wavenumber values of the spectrum.}
#'   \item{`"vector"`}{All intensity values will be divided by the norm of
#'      the intensity vector (vector normalization).}
#'   \item{"snv"}{Standard Normal Variate correction: For each spectrum, the
#'      average intensity value is subtracted and then divided by the standard
#'      deviation.}
#'   \item{A numeric value}{If `method` is convertible to a numeric value, e.g.
#'      `method = "980"`, the intensity of all spectra at a wavenumber value of
#'      980 will be set to 1 and the minimum intensity value of each spectrum
#'      will be set to 0, i.e. the spectra will be normalized referring to a
#'      specific wavenumber value.}
#' }
#'
#' @return An object of class `ir` representing a normalized version of
#' `x`.
#'
#' @examples
#' # with method = "area"
#' x1 <-
#'    ir::ir_sample_data |>
#'    ir::ir_normalize(method = "area")
#'
#' # second derivative spectrum with method = "area" or method = "area_absolute"
#' x2 <-
#'    ir::ir_sample_data |>
#'    ir::ir_smooth(method = "sg", n = 31, m = 2) |>
#'    ir::ir_normalize(method = "area")
#'
#' x3 <-
#'    ir::ir_sample_data |>
#'    ir::ir_smooth(method = "sg", n = 31, m = 2) |>
#'    ir::ir_normalize(method = "area_absolute")
#'
#' # with method = "zeroone"
#' x4 <-
#'    ir::ir_sample_data |>
#'    ir::ir_normalize(method = "zeroone")
#'
#' # with method = "vector"
#' x5 <-
#'    ir::ir_sample_data |>
#'    ir::ir_normalize(method = "vector")
#'
#' # with method = "snv"
#' x6 <-
#'    ir::ir_sample_data |>
#'    ir::ir_normalize(method = "snv")
#'
#' # normalizing to a specific peak
#' x7 <-
#'    ir::ir_sample_data |>
#'    ir::ir_normalize(method = 1090)
#'
#' @export
ir_normalize <- function(x, method = "area") {

  # checks
  if(!inherits(x, "ir")) {
    rlang::abort(paste0("`x` must be of class ir, not ", class(x)[[1]],"."))
  }
  if(!((is.character(method)| is.numeric(method)) & length(method) == 1)) {
    rlang::abort("`method`` must be a character value or a numeric value.")
  }
  if(is.character(method)) {
    if(!(method %in% c("zeroone", "area", "area_absolute", "vector", "snv"))){
      rlang::abort("If specified as character value, `method` must be one of 'zeroone', 'area', 'area_absolute', 'vector', or 'snv'.")
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
    # normalize to the absolute area
    area_absolute = {
      index <- NULL
      f <-
        function(y, ...) {
          y/sum(abs(y), na.rm = TRUE)
        }
    },
    # vector normalization
    vector = {
      index <- NULL
      f <-
        function(y, ...) {
          y/sqrt(as.vector(y %*% y))
        }
    },
    # Standard Normal Variate correction
    snv = {
      index <- NULL
      f <-
        function(y, ...) {
          (y - mean(y, na.rm = TRUE))/stats::sd(y, na.rm = TRUE)
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
        if(nrow(z) == 0) {
          return(z)
        }
        z %>%
          dplyr::mutate(
            y = f(.data$y, index)
          )
      })
    )

}

#' @rdname ir_normalize
#' @export
ir_normalise <- ir_normalize

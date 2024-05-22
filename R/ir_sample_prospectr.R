#' Wrapper to sampling functions from the 'prospectr' package
#'
#' Wrapper functions that allows to directly use 'ir' objects with sampling
#' functions from the 'prospectr' package.
#'
#' @param x An object of class 'ir' containing the spectra based on which
#' to sample measurements.
#'
#' @param sampling_function A function from the 'prospectr' package to perform
#' sampling based on spectra (`naes()`, `kenStone()`, `duplex()`, `puchwein()`,
#' `shenkWest()`, `honig()`). See the 'prospectr' package for details.
#'
#' @param ... Arguments passed to `sampling_function`. See the 'prospectr'
#' package for details.
#'
#' @param return_prospectr_output Logical value. If `TRUE`, the output of
#' `sampling_function` is returned. If `FALSE`, values of elements `model`
#' and `test` are included as columns in `x` and `x` is returned.
#'
#' @return
#' If `return_prospectr_output = TRUE`, the output of `sampling_function`. See
#' the 'prospectr' package for details. If `return_prospectr_output = FALSE`,`x`
#' with the following additional columns:
#' \describe{
#'   \item{for_prospectr_model}{Logical value indicating whether the spectrum is
#'      listed in element `model` of the prospectr output (`TRUE`) or not
#'      (`FALSE`).}
#'   \item{for_prospectr_test}{Logical value indicating whether the spectrum is
#'      listed in element `test` of the prospectr output (`TRUE`) or not
#'      (`FALSE`).}
#'   \item{prospectr_model}{Integer representing the order in which spectra are
#'      listed in element `model` of the prospectr output.}
#'   \item{prospectr_test}{Integer representing the order in which spectra are
#'      listed in element `test` of the prospectr output.}
#' }
#'
#' @examples
#' if(requireNamespace("prospectr", quietly = TRUE)) {
#'   x <-
#'     ir_sample_prospectr(
#'       ir::ir_sample_data,
#'       prospectr::kenStone,
#'       metric = "euclid",
#'       k = 30,
#'       return_prospectr_output = FALSE
#'   )
#'
#'   x <-
#'     ir_sample_prospectr(
#'       ir::ir_sample_data,
#'       prospectr::kenStone,
#'       metric = "euclid",
#'       k = 30,
#'       return_prospectr_output = TRUE
#'     )
#' }
#'
#' @export
ir_sample_prospectr <- function(x, sampling_function, ..., return_prospectr_output = FALSE) {

  if(! requireNamespace("prospectr", quietly = TRUE)) {
    rlang::abort(paste0("You have to install the 'prospectr' package to use this function."))
  }
  ir_check_ir(x)
  if(! is.function(sampling_function)) {
    rlang::abort(paste0("`sampling_function` must be one of the sampling functions from the 'prospectr' package, but is of class ", class(sampling_function)[[1]], "."))
  }

  # convert x to spectral matrix
  x_or <- x
  x <- ir_flatten(x)
  x_wavenumbers <- x$x
  x <- t(x[, -1])
  colnames(x) <- x_wavenumbers

  res <- sampling_function(x, ...)

  if(return_prospectr_output) {
    res
  } else {
    index <- seq_len(nrow(x_or))
    x_or$for_prospectr_model <- ifelse(index %in% res$model, TRUE, FALSE)
    x_or$for_prospectr_test <- ifelse(index %in% res$test, TRUE, FALSE)
    x_or$prospectr_model <- NA_integer_
    x_or$prospectr_model[res$model] <- seq_along(res$model)
    x_or$prospectr_test <- NA_integer_
    x_or$prospectr_test[res$test] <- seq_along(res$test)
    x_or
  }

}

#' Stacks a matrix or data frame with spectra into a list column
#'
#' `ir_stack` takes a matrix or data frame with infrared spectra and
#' converts it into a  list column corresponding to the column `spectra` in
#' objects of class `ir`.
#'
#' @param x A matrix or data frame with a first column (`x`) containing "x
#' axis values" of the spectra (e.g. wavenumbers) and all remaining columns
#' containing intensity values of spectra.
#'
#' @return A [tibble::tibble()] with the stacked spectra in column `spectra`.
#'
#' @examples
#' # from data frame
#' x1 <-
#'    ir::ir_sample_data %>%
#'    ir::ir_flatten() %>%
#'    ir::ir_stack()
#'
#' # from matrix
#' x2 <-
#'    ir::ir_sample_data %>%
#'    ir::ir_flatten() %>%
#'    as.matrix() %>%
#'    ir::ir_stack()
#'
#' @export
ir_stack <- function(x) {

  if(is.matrix(x)) {
    x <- as.data.frame(x)
  }
  if(!(is.matrix(x) || is.data.frame(x))) {
    rlang::abort(paste0("`x` must be a matrix, not ", class(x)[[1]], "."))
  }

  template <-
    tibble::tibble(
      x = x[, 1, drop = TRUE],
      y = rep(NA_real_, length(x))
    )

  spectra <-
    lapply(x[,-1, drop = FALSE], function(y) {
      d <- template
      d$y <- as.numeric(y)
      d[! is.na(d$y), ]
    })
  tibble::tibble(spectra = spectra)

}



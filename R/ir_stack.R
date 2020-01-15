#' Stacks a matrix with spectra into an object of class \code{ir}.
#'
#' \code{ir_stack} takes a matrix with infrared spectra and converts it
#' into a list column corresponding to the column \code{spectra} in objects
#' of class \code{ir}.
#'
#' @param x A matrix or data.frame with a first column (\code{x}) containing x axis values
#' of the spectra (e.g. wavenumbers) and all remaining columns containing
#' intensity values of spectra. Column names are assumed to represent
#' @return A \code{\link[tibble:tibble]{tibble}} with the stacked spectra.
#' @export
ir_stack <- function(x) {

  if(is.matrix(x)) {
    x <- as.data.frame(x)
  }
  if(!(is.matrix(x) || is.data.frame(x))) {
    rlang::abort(paste0("`x` must be a matrix, not ", class(x)[[1]], "."))
  }

  spectra <- lapply(x[,-1], function(y){
    d <- tibble::tibble(x = x[, 1, drop = TRUE],
                        y = y)
    d[!is.na(d$y), ]
  })
  tibble::tibble(spectra = spectra)

}



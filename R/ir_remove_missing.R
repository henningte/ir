#' Removes empty data values in objects of class \code{ir}.
#'
#' \code{ir_remove_missing} takes and object of class \code{ir} and removes
#' all rows in the \code{data.frame}s of the list column \code{spectra} that
#' have \code{NA} intensity values (column \code{y}). Additionally, one
#' can specify to remove rows in the \code{ir} object to discard if they
#' contain empty spectra.
#'
#' @param x An object of class \code{\link[ir:ir_new_ir]{ir}}.
#' @param remove_rows A logical value indicating if rows in \code{x}
#' with empty spectra should be discarded (\code{remove_rows = TRUE})
#' or not (\code{remove_rows = FALSE}).
#' @return \code{x} with cleaned spectra.
#' @export
ir_remove_missing <- function(x,
                              remove_rows = FALSE) {

  ir_check_ir(x)
  if(!is.logical(remove_rows)) {
    rlang::abort(paste0("`remove_rows` must be a logical vector, not", class(remove_rows)[[1]], "."))
  }
  if(length(remove_rows) != 1) {
    rlang::abort("`remove_rows` must be of length 1, not ", length(remove_rows), ".")
  }

  x$spectra <- purrr::map(x$spectra, function(x) x[!is.na(x$y),])

  if(remove_rows) {
    x <- x[purrr::map_int(x$spectra, nrow) != 0,]
  }
  x

}

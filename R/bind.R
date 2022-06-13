#' Bind rows of `ir` objects
#'
#' @name bind
#'
#' @param ... Objects to bind together. For `cbind`, only the first of the
#' objects is allowed to be of class `ir`.
#'
#' @param deparse.level An integer value; see [rbind()].
#'
#' @return An object of class `ir`. `rbind` returns all input
#' `ir` objects combined row-wise. `cbind` returns the input `ir`
#' object and the other objects combined column-wise.
#'
#' @examples
#' # rbind
#' rbind(ir_sample_data, ir_sample_data)
#' rbind(ir_sample_data %>% dplyr::select(spectra),
#'       ir_sample_data %>% dplyr::select(spectra))
#'
#' @export
rbind.ir <- function(..., deparse.level = 1) {

  dots <- list(...)
  stopifnot(all(vapply(dots, inherits, "ir", FUN.VALUE = TRUE)))
  res_spectra <- unlist(purrr::map(dots, function(.x) dplyr::pull(.x, .data$spectra)), recursive = FALSE)
  res_metadata <- purrr::map_dfr(dots, dplyr::select, -.data$spectra)
  ir_new_ir(spectra = res_spectra, metadata = res_metadata)

}

#' @rdname bind
#'
#' @examples
#' # cbind
#' cbind(ir_sample_data, a = seq_len(nrow(ir_sample_data)))
#'
#' @export
cbind.ir <- function(..., deparse.level = 1) {

  dots <- list(...)
  is_ir <- vapply(dots, inherits, "ir", FUN.VALUE = TRUE)
  if(sum(is_ir) != 1) {
    rlang::abort("In `...`, only the first element is allowed to be of class `ir`.")
  }

  res <- dplyr::bind_cols(lapply(dots, function(x) structure(x, class = setdiff(class(x), "ir"))))
  ir_as_ir(res)

}

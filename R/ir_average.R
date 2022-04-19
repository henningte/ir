#' Averages infrared spectra within groups
#'
#' \code{ir_average} averages infrared spectra within
#' a user-defined group. \code{NA} values are omitted by default
#'
#' @param x An object of class \code{\link[ir:ir_new_ir]{ir}}.
#' @param ... Variables in \code{x} to use as groups.
#' @return An object of class \code{ir}.
#' @examples
#' # average the sample data specta across sample types
#' x <-
#'   ir::ir_sample_data %>%
#'   ir::ir_average(sample_type)
#' @export
ir_average <- function(x,
                       ...) {

  spectra <- NULL

  x %>%
    ir_check_ir() %>%
    dplyr::group_by(...) %>%
    dplyr::summarise(
      spectra = {
        x_flat <- .data$spectra
        purrr::map2(x_flat, seq_along(.data$spectra), function(x, y) {
          colnames(x)[[2]] <- y
          x
        })
        x_flat <- purrr::reduce(x_flat, dplyr::full_join, by = "x")
        list(data.frame(x = x_flat$x,
                        y = apply(x_flat[, -1, drop = FALSE], 1, mean, na.rm = TRUE)))
      }) %>%
    ir_as_ir()

}

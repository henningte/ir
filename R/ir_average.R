#' Averages infrared spectra within groups
#'
#' `ir_average` averages infrared spectra within a user-defined group. `NA`
#' values are omitted by default.
#'
#' @param x An object of class [`ir`][ir_new_ir()].
#'
#' @param ... Variables in `x` to use as groups.
#'
#' @param na.rm A logical value indicating if `NA` values should be dropped
#' (`TRUE`) or not (`FALSE`).
#'
#' @return An object of class `ir` where spectra have been averaged within
#' groups defined by `...`.
#'
#' @examples
#' # average the sample data spectra across sample types
#' x <-
#'   ir::ir_sample_data %>%
#'   ir::ir_average(sample_type)
#' @export
ir_average <- function(x, ..., na.rm = TRUE) {

  spectra <- NULL

  x %>%
    ir_check_ir() %>%
    dplyr::group_by(...) %>%
    dplyr::summarize(
      spectra = {
        x_flat <- .data$spectra
        purrr::map2(x_flat, seq_along(.data$spectra), function(x, y) {
          colnames(x)[[2]] <- y
          x
        })
        x_flat <- purrr::reduce(x_flat, dplyr::full_join, by = "x")
        list(data.frame(x = x_flat$x,
                        y = apply(x_flat[, -1, drop = FALSE], 1, mean, na.rm = na.rm)))
      }) %>%
    ir_as_ir()

}

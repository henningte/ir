#' Converts objects of class `ir` to objects of class `ir_flat`
#'
#' `ir_flatten` takes and object of class `ir`, extracts the
#' `spectra` column and combines the spectra into an object of class
#' [`ir_flat`][ir::ir_new_ir_flat]. Metadata are not retained during flattening.
#'
#' @param x An object of class [`ir`][ir_new_ir()].
#'
#' @param measurement_id A character vector an element for each row in `x`
#' that contains the names to use as column names for the spectra in the
#' `ir_flat` object to create.
#'
#' @return An object of class [`ir_flat`][ir::ir_new_ir_flat].
#'
#' @examples
#' x_flat <-
#'    ir::ir_sample_data %>%
#'    ir::ir_flatten()
#'
#' @export
ir_flatten <- function(x, measurement_id = as.character(seq_len(nrow(x)))) {

  if(!is.character(measurement_id)) {
    rlang::abort(paste0("`measurement_id` must be a character vector, not", class(measurement_id)[[1]], "."))
  }
  if(length(measurement_id) != nrow(x)) {
    rlang::abort("The number of rows in `x` must equal the length of `measurement_id`.")
  }

  ir_check_ir(x)

  # collect common wavenumbers
  x_wavenumbers <-
    tibble::tibble(x = sort(unique(unlist(purrr::map(x$spectra, function(.x) .x$x)))))

  # combine
  res <-
    dplyr::bind_cols(
      x_wavenumbers,
      purrr::map2(x$spectra, measurement_id, function(.x, .y) {
        stats::setNames(.x[match(x_wavenumbers$x, .x$x), 2, drop = FALSE], nm = .y)
      })
    )

  ir_new_ir_flat(res)

}


#' Cleans objects of class `ir_flat`
#'
#' `ir_flatten_clean` takes an object of class `ir_flat` and either
#' returns all non-empty spectra or all empty spectra as object of class
#' `ir_flat`.
#'
#' @param x An object of class [`ir_flat`][ir::ir_new_ir_flat].
#'
#' @param return_empty A logical value indicating if the empty spectra should be
#' returned (`return_empty = TRUE`) or the non-empty spectra
#' (`return_empty = FALSE`).
#'
#' @return `x` where empty spectra are dropped (if `return_empty = TRUE`) or
#' only empty spectra are returned (`return_empty = FALSE`).
#'
#' @export
ir_flat_clean <- function(x,
                          return_empty = FALSE) {

  ir_flat_check(x)

  x_is_empty <- purrr::map_lgl(x, function(x) all(is.na(x)))

  if(return_empty) {
    x_is_empty[[1]] <- TRUE
    x[, x_is_empty, drop = FALSE]
  } else {
    x[, !x_is_empty, drop = FALSE]
  }

}




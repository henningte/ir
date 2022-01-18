#' Get the range of x axis values or intensity values of infrared spectra
#'
#' \code{range.ir} extracts the range of x axis values (e.g. wavenumbers) or
#' intensity values of infrared spectra.
#'
#' @name range
#' @param x An object of class \code{\link{ir}}.
#' @param ... Further arguments, ignored.
#' @param na.rm A logical value. See \code{\link{max}}.
#' @param .dimension A character value. Must be one of the following:
#' \describe{
#'   \item{"x"}{In this case, the minimum/maximum/range of x axis values of the
#'   spectra in \code{x} are extracted.}
#'   \item{"y"}{In this case, the minimum/maximum/range of intensity values of
#'   the spectra in \code{x} are extracted.}
#' }
#' @param .col_names A character vector of length 2 representing the names of the
#' columns in which to store the extracted values. The first element is the name
#' for the column with minima values, the second the name for the column with
#' maxima values.
#' @return \code{x} with the extracted values.
#' @examples
#' # range of intensity values
#' x1 <-
#'    ir::ir_sample_data %>%
#'    range(.dimension = "y")
#' @export
range.ir <- function(x, ..., na.rm = FALSE, .dimension = "y", .col_names = c("y_min", "y_max")) {

  stopifnot(is.character(.dimension) && length(.dimension) == 1 && .dimension %in% c("x", "y"))
  stopifnot(is.character(.col_names) && length(.col_names) == 2)

  x %>%
    min(na.rm = na.rm, .dimension = .dimension, .col_name = .col_names[[1]]) %>%
    max(na.rm = na.rm, .dimension = .dimension, .col_name = .col_names[[2]])

}

#' Get the minimum x axis value or intensity value of infrared spectra
#'
#' \code{min.ir} extracts the minimum x axis value (e.g. wavenumber) or
#' intensity value of infrared spectra.
#'
#' @rdname range
#' @param .col_name A character value representing the name of the column in
#' which to store the extracted values.
#' @examples
#' # minimum intensity values
#' x1 <-
#'    ir::ir_sample_data %>%
#'    min(.dimension = "y")
#'
#' @export
min.ir <- function(x, ..., na.rm = FALSE, .dimension = "y", .col_name = "y_min") {

  stopifnot(is.character(.dimension) && length(.dimension) == 1 && .dimension %in% c("x", "y"))
  stopifnot(is.character(.col_name) && length(.col_name) == 1)

  res <-
    tibble::tibble(
      res =
        x$spectra %>%
        purrr::map_dbl(function(z) min(z[, .dimension, drop = TRUE], na.rm = na.rm))
    )

  colnames(res)[[1]] <- .col_name

  dplyr::bind_cols(x, res)

}


#' Get the maximum x axis value or intensity value of infrared spectra
#'
#' \code{max.ir} extracts the maximum x axis value (e.g. wavenumber) or
#' intensity value of infrared spectra.
#'
#' @rdname range
#' @examples
#' # maximum intensity values
#' x1 <-
#'    ir::ir_sample_data %>%
#'    max(.dimension = "y")
#'
#' @export
max.ir <- function(x, ..., na.rm = FALSE, .dimension = "y", .col_name = "y_max") {

  stopifnot(is.character(.dimension) && length(.dimension) == 1 && .dimension %in% c("x", "y"))
  stopifnot(is.character(.col_name) && length(.col_name) == 1)

  res <-
    tibble::tibble(
      res =
        x$spectra %>%
        purrr::map_dbl(function(z) max(z[, .dimension, drop = TRUE], na.rm = na.rm))
    )

  colnames(res)[[1]] <- .col_name

  dplyr::bind_cols(x, res)

}

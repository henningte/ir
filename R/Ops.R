#' Subtract infrared spectra
#'
#' `ir_subtract` takes two objects of class `ir`, `x` and
#' `y`, and subtracts the intensity values of spectra in matching rows from
#' `y` from that of `x`. Alternatively, takes an object of class
#' `ir`, `x`, and a numeric value, `y`, and subtracts `y`
#' from all intensity values in `x`.
#'
#' @param x An object of class [`ir`][ir_new_ir()].
#'
#' @param y An object of class [`ir`][ir_new_ir()] or a numeic value. If `y`
#' is an object of class `ir`, it must have the same number of rows as
#' `x` and the same x axis values (e.g. wavenumber values) in each matching
#' spectrum as in `x`.
#'
#' @return `x` where for each spectrum the respective intensity values in
#' `y` are subtracted (if `y` is an object of class `ir`), or
#' where for each spectrum `y` has been subtracted from the intensity
#' values.
#'
#' @examples
#' # subtracting two objects of class ir
#' x1 <-
#'   ir::ir_subtract(ir::ir_sample_data, ir::ir_sample_data)
#' x2 <-
#'   ir::ir_subtract(ir::ir_sample_data, ir::ir_sample_data[1, ])
#'
#' # subtracting a numeric value from an object of class `ir`.
#' x3 <-
#'   ir::ir_subtract(ir::ir_sample_data, 20)
#'
#' # subtracting a numeric vector from an object of class `ir`.
#' x4 <-
#'   ir::ir_subtract(
#'      ir::ir_sample_data,
#'      seq(from = 0, to = 2, length.out = nrow(ir::ir_sample_data))
#'   )
#'
#' @export
ir_subtract <- function(x, y) {

  # checks
  res <- ir_prepare_Ops(x, y)
  x <- res$x
  y <- res$y

  x %>%
    dplyr::mutate(
      spectra =
        if(inherits(y, "ir")) {
          purrr::map(seq_along(.data$spectra), function(i) {
            z <- .data$spectra[[i]]
            z$y <- z$y - y$spectra[[i]]$y
            z
          })
        } else {
          purrr::map(seq_along(.data$spectra), function(i) {
            z <- .data$spectra[[i]]
            z$y <- z$y - y[[i]]
            z
          })
        }
    )

}

#' Add infrared spectra
#'
#' `ir_add` takes two objects of class `ir`, `x` and
#' `y`, and adds the intensity values of spectra in matching rows from
#' `y` to that of `x`.
#'
#' @inheritParams ir_subtract
#'
#' @return `x` where for each spectrum the respective intensity values in
#' `y` are added.
#'
#' @examples
#' x1 <-
#'   ir::ir_add(ir::ir_sample_data, ir::ir_sample_data)
#' x2 <-
#'   ir::ir_add(ir::ir_sample_data, ir::ir_sample_data[1, ])
#'
#' # adding a numeric value to an object of class `ir`.
#' x3 <-
#'   ir::ir_add(ir::ir_sample_data, 1)
#'
#' # adding a numeric vector from an object of class `ir`.
#' x4 <-
#'   ir::ir_add(
#'      ir::ir_sample_data,
#'      seq(from = 0, to = 2, length.out = nrow(ir::ir_sample_data))
#'   )
#'
#' @export
ir_add <- function(x, y) {

  # checks
  res <- ir_prepare_Ops(x, y)
  x <- res$x
  y <- res$y

  x %>%
    dplyr::mutate(
      spectra =
        if(inherits(y, "ir")) {
          purrr::map(seq_along(.data$spectra), function(i) {
            z <- .data$spectra[[i]]
            z$y <- z$y + y$spectra[[i]]$y
            z
          })
        } else {
          purrr::map(seq_along(.data$spectra), function(i) {
            z <- .data$spectra[[i]]
            z$y <- z$y + y[[i]]
            z
          })
        }
    )

}

#' Multiply infrared spectra or multiply infrared spectra with a numeric value
#'
#' `ir_multiply` takes two objects of class `ir`, `x` and
#' `y`, and multiplies their intensity values, or it takes one object of
#' class `ir`, `x`, and one numeric value, `y`, and multiplies
#' all intensity values in `x` with `y`.
#'
#' @inheritParams ir_subtract
#'
#' @return `x` where for each spectrum intensity values are multiplied with
#' the respective intensity values in `y` (if `y` is an object of
#' class `ir`), or where all intensity values are multiplied with `y`
#' if `y` is a numeric value.
#'
#' @examples
#' # multiplication with y as ir object
#' x1 <-
#'   ir::ir_multiply(ir::ir_sample_data, ir::ir_sample_data)
#' x2 <-
#'   ir::ir_multiply(ir::ir_sample_data, ir::ir_sample_data[1, ])
#'
#' # multiplication with y being a numeric value
#' x3 <-
#'   ir::ir_multiply(ir::ir_sample_data, y = -1)
#'
#' # multiplication with y being a numeric vector
#' x4 <-
#'   ir::ir_multiply(
#'      ir::ir_sample_data,
#'      seq(from = 0, to = 2, length.out = nrow(ir::ir_sample_data))
#'   )
#'
#' @export
ir_multiply <- function(x, y) {

  # checks
  res <- ir_prepare_Ops(x, y)
  x <- res$x
  y <- res$y

  x %>%
    dplyr::mutate(
      spectra =
        if(inherits(y, "ir")) {
          purrr::map(seq_along(.data$spectra), function(i) {
            z <- .data$spectra[[i]]
            z$y <- z$y * y$spectra[[i]]$y
            z
          })
        } else {
          purrr::map(seq_along(.data$spectra), function(i) {
            z <- .data$spectra[[i]]
            z$y <- z$y * y[[i]]
            z
          })
        }
    )

}


#' Divide infrared spectra or divide infrared spectra by a numeric value
#'
#' `ir_divide` takes two objects of class `ir`, `x` and
#' `y`, and divides their intensity values, or it takes one object of
#' class `ir`, `x`, and one numeric value, `y`, and divides
#' all intensity values in `x` by `y`.
#'
#' @inheritParams ir_subtract
#'
#' @return `x` where for each spectrum intensity values are divided by
#' the respective intensity values in `y` (if `y` is an object of
#' class `ir`), or where all intensity values are divided by `y` if
#' `y` is a numeric value.
#'
#' @examples
#' # division with y as ir object
#' x1 <-
#'   ir::ir_divide(ir::ir_sample_data, ir::ir_sample_data)
#' x2 <-
#'   ir::ir_divide(ir::ir_sample_data, ir::ir_sample_data[1, ])
#'
#' # division with y being a numeric value
#' x3 <-
#'   ir::ir_divide(ir::ir_sample_data, y = 20)
#'
#' # division with y being a numeric vector
#' x4 <-
#'   ir::ir_divide(
#'      ir::ir_sample_data,
#'      seq(from = 0.1, to = 2, length.out = nrow(ir::ir_sample_data))
#'   )
#'
#' @export
ir_divide <- function(x, y) {

  # checks
  res <- ir_prepare_Ops(x, y)
  x <- res$x
  y <- res$y

  x %>%
    dplyr::mutate(
      spectra =
        if(inherits(y, "ir")) {
          purrr::map(seq_along(.data$spectra), function(i) {
            z <- .data$spectra[[i]]
            z$y <- z$y / y$spectra[[i]]$y
            z
          })
        } else {
          purrr::map(seq_along(.data$spectra), function(i) {
            z <- .data$spectra[[i]]
            z$y <- z$y / y[[i]]
            z
          })
        }
    )

}


#' Arithmetic operations for `ir` objects
#'
#' @param e1 An object of class `ir`.
#'
#' @param e2 An object of class `ir` or a numeric value.
#'
#' @return `e1` with intensity values of the spectra added to/subtracted
#' with/multiplied with/divided by those in `e2`:
#' + If `e2` is a numeric value, all intensity values in the spectra of `e1` are
#'   added/subtracted/multiplied/divided by `e2`.
#' + If `e2` is an `ir` object with one row, it is replicated (see [rep.ir]) so
#'   that the row numbers match to those of `e1` and intensity values are
#'   added/subtracted/multiplied/divided row-wise.
#' + If `e2` is an `ir` object with the same number of rows as `e1`, intensity
#'   values are added/subtracted/multiplied/divided row-wise.
#'
#' @examples
#' ## addition
#' ir::ir_sample_data + ir::ir_sample_data
#' ir::ir_sample_data + 2
#' ir::ir_sample_data +
#'    seq(from = 0, to = 2, length.out = nrow(ir::ir_sample_data))
#'
#' ## subtraction
#' ir::ir_sample_data - ir::ir_sample_data
#' ir::ir_sample_data - 2
#' ir::ir_sample_data -
#'    seq(from = 0, to = 2, length.out = nrow(ir::ir_sample_data))
#'
#' ## multiplication
#' ir::ir_sample_data * ir::ir_sample_data
#' ir::ir_sample_data * 2
#' ir::ir_sample_data *
#'    seq(from = 0, to = 2, length.out = nrow(ir::ir_sample_data))
#'
#' ## division
#' ir::ir_sample_data / ir::ir_sample_data
#' ir::ir_sample_data / 2
#' ir::ir_sample_data /
#'    seq(from = 0.1, to = 2, length.out = nrow(ir::ir_sample_data))
#'
#' @export
Ops.ir <- function(e1, e2) {

  switch(
    .Generic,
    "+" = ir_add(x = e1, y = e2),
    "-" = ir_subtract(x = e1, y = e2),
    "*" = ir_multiply(x = e1, y = e2),
    "/" = ir_divide(x = e1, y = e2),
    "^" =,
    "%%" =,
    "&" =,
    "!" =,
    "|" =,
    "%/%" =,
    "==" =,
    "!=",
    "<"=,
    ">"=,
    "<="=,
    ">="= rlang::abort("This method is not implemented yet for `ir` objects.")
  )

}




### Helpers ###

#' Prepares objects for arithmetic operations with an `ir` object
#'
#' Internal helper function for Ops for objects of class `ir`.
#'
#' @inheritParams ir_subtract
#'
#' @return A list with three elements:
#' \describe{
#'   \item{`x`}{`x` (with empty spectra modified so that arithmetic operations
#'   cause no errors).}
#'   \item{`y`}{`y` (with empty spectra modified so that arithmetic operations
#'   cause no errors).}
#' }
#'
#' @keywords internal
#' @noRd
ir_prepare_Ops <- function(x, y) {

  ir_check_ir(x)
  stopifnot(inherits(y, "ir") || (is.numeric(y) && (length(y) == 1L || length(y) == nrow(x))))
  if(inherits(y, "ir")) {
    ir_check_ir(y)

    if(nrow(y) != 1 && nrow(y) != nrow(x)) {
      rlang::abort("`y` must either have only one row or as many rows as `x`.")
    }
    if(nrow(y) == 1) {
      y <- rep(y, nrow(x))
    }

    x_spectrum_is_empty <- ir_check_for_empty_spectra(x)
    y_spectrum_is_empty <- ir_check_for_empty_spectra(y)
    if(all(y_spectrum_is_empty) || all(x_spectrum_is_empty)) { # in case all spectra in x or y are empty, return NA (in analogy to 1 - NA)
      x$spectra <- purrr::map(x$spectra, function(.x) {
        .x %>%
          dplyr::mutate(
            y = NA_real_
          )
      })
      return(x)
    } else { # otherwise, impute gaps
      if(any(y_spectrum_is_empty)) {
        y$spectra[y_spectrum_is_empty] <-
          purrr::map(seq_len(sum(y_spectrum_is_empty)), function(i) {
            y$spectra[!y_spectrum_is_empty][[1]] %>%
              dplyr::mutate(
                y = NA_real_
              )
          })
      }
      if(any(x_spectrum_is_empty)) {
        x$spectra[x_spectrum_is_empty] <-
          purrr::map(seq_len(sum(x_spectrum_is_empty)), function(i) {
            x$spectra[!x_spectrum_is_empty][[1]] %>%
              dplyr::mutate(
                y = NA_real_
              )
          })
      }
    }

    cond <-
      purrr::map_lgl(seq_len(nrow(x)), function(i) {
        !identical(x$spectra[[i]]$x, y$spectra[[i]]$x)
      })
    if(any(cond)) {
      rlang::abort(paste0("Not all spectra in `y` have x axis values matching those in `x`. Mismatches have been found for spectra ", paste(which(cond), collapse = ", "), "."))
    }

  } else {
    if(length(y) == 1) {
      y <- rep(y, nrow(x))
    }
  }

  list(
    x = x,
    y = y
  )

}

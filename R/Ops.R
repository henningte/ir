#' Subtract infrared spectra
#'
#' `ir_subtract` takes two objects of class `ir`, `x` and
#' `y`, and subtracts the intensity values of spectra in matching rows from
#' `y` from that of `x`. Alternatively, takes an object of class
#' `ir`, `x`, and a numeric value, `y`, and subtracts `y`
#' from all intensity values in `x`.
#'
#' @param x An object of class [`ir`][ir_new_ir()].
#' @param y An object of class [`ir`][ir_new_ir()] or a numeic value. If `y`
#' is an object of class `ir`, it must have the same number of rows as
#' `x` and the same x axis values (e.g. wavenumber values) in each matching
#' spectrum as in `x`.
#' @return `x` where for each spectrum the respective intensity values in
#' `y` are subtracted (if `y` is an object of class `ir`), or
#' where for each spectrum `y` has been subtracted from the intensity
#' values.
#' @examples
#' # subtracting two objects of class ir
#' x1 <-
#'   ir::ir_subtract(ir::ir_sample_data, ir::ir_sample_data)
#' x1 <-
#'   ir::ir_subtract(ir::ir_sample_data, ir::ir_sample_data[1, ])
#'
#' # subtracting a numeric value from an object of class `ir`.
#' x2 <-
#'   ir::ir_subtract(ir::ir_sample_data, 20)
#' @export
ir_subtract <- function(x, y) {

  # checks
  ir_check_ir(x)
  stopifnot(inherits(y, "ir") || (is.numeric(y) && length(y) == 1))
  if(inherits(y, "ir")) {
    ir_check_ir(y)

    if(nrow(y) != 1 && nrow(y) != nrow(x)) {
      rlang::abort("`y` must either have only one row or as many rows as `x`.")
    }
    if(nrow(y) == 1) {
      y <- rep(y, nrow(x))
    }

    cond <-
      purrr::map_lgl(seq_len(nrow(x)), function(i) {
        !identical(x$spectra[[i]]$x, y$spectra[[i]]$x)
      })
    if(any(cond)) {
      rlang::abort(paste0("Not all spectra in `y` have x axis values matching those in `x`. Mismatches have been found for spectra ", paste(which(cond), collapse = ", "), "."))
    }
    y_is_ir <- TRUE

  } else {
    y_is_ir <- FALSE
  }

  x %>%
    dplyr::mutate(
      spectra =
        if(y_is_ir) {
          purrr::map(seq_along(.data$spectra), function(i) {
            z <- .data$spectra[[i]]
            z$y <- z$y - y$spectra[[i]]$y
            z
          })
        } else {
          purrr::map(seq_along(.data$spectra), function(i) {
            z <- .data$spectra[[i]]
            z$y <- z$y - y
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
#' @return `x` where for each spectrum the respective intensity values in
#' `y` are added.
#' @examples
#' x1 <-
#'   ir::ir_add(ir::ir_sample_data, ir::ir_sample_data)
#' x1 <-
#'   ir::ir_add(ir::ir_sample_data, ir::ir_sample_data[1, ])
#' @export
ir_add <- function(x, y) {

  # checks
  ir_check_ir(x)
  stopifnot(inherits(y, "ir") || (is.numeric(y) && length(y) == 1))
  if(inherits(y, "ir")) {
    ir_check_ir(y)

    if(nrow(y) != 1 && nrow(y) != nrow(x)) {
      rlang::abort("`y` must either have only one row or as many rows as `x`.")
    }
    if(nrow(y) == 1) {
      y <- rep(y, nrow(x))
    }

    cond <-
      purrr::map_lgl(seq_len(nrow(x)), function(i) {
        !identical(x$spectra[[i]]$x, y$spectra[[i]]$x)
      })
    if(any(cond)) {
      rlang::abort(paste0("Not all spectra in `y` have x axis values matching those in `x`. Mismatches have been found for spectra ", paste(which(cond), collapse = ", "), "."))
    }
    y_is_ir <- TRUE

  } else {
    y_is_ir <- FALSE
  }

  x %>%
    dplyr::mutate(
      spectra =
        if(y_is_ir) {
          purrr::map(seq_along(.data$spectra), function(i) {
            z <- .data$spectra[[i]]
            z$y <- z$y + y$spectra[[i]]$y
            z
          })
        } else {
          purrr::map(seq_along(.data$spectra), function(i) {
            z <- .data$spectra[[i]]
            z$y <- z$y + y
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
#' @return `x` where for each spectrum intensity values are multiplied with
#' the respective intensity values in `y` (if `y` is an object of
#' class `ir`), or where all intensity values are multiplied with `y`
#' if `y` is a numeric value.
#' @examples
#' # multiplication with y as ir object
#' x1 <-
#'   ir::ir_multiply(ir::ir_sample_data, ir::ir_sample_data)
#' x1 <-
#'   ir::ir_multiply(ir::ir_sample_data, ir::ir_sample_data[1, ])
#'
#' # multiplication with y being a numeric value
#' x2 <-
#'   ir::ir_multiply(ir::ir_sample_data, y = -1)
#' @export
ir_multiply <- function(x, y) {

  # checks
  ir_check_ir(x)
  stopifnot(inherits(y, "ir") || (is.numeric(y) && length(y) == 1))
  if(inherits(y, "ir")) {
    ir_check_ir(y)

    if(nrow(y) != 1 && nrow(y) != nrow(x)) {
      rlang::abort("`y` must either have only one row or as many rows as `x`.")
    }
    if(nrow(y) == 1) {
      y <- rep(y, nrow(x))
    }

    cond <-
      purrr::map_lgl(seq_len(nrow(x)), function(i) {
        !identical(x$spectra[[i]]$x, y$spectra[[i]]$x)
      })
    if(any(cond)) {
      rlang::abort(paste0("Not all spectra in `y` have x axis values matching those in `x`. Mismatches have been found for spectra ", paste(which(cond), collapse = ", "), "."))
    }
    y_is_ir <- TRUE

  } else {
    y_is_ir <- FALSE
  }

  x %>%
    dplyr::mutate(
      spectra =
        if(y_is_ir) {
          purrr::map(seq_along(.data$spectra), function(i) {
            z <- .data$spectra[[i]]
            z$y <- z$y * y$spectra[[i]]$y
            z
          })
        } else {
          purrr::map(seq_along(.data$spectra), function(i) {
            z <- .data$spectra[[i]]
            z$y <- z$y * y
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
#' @return `x` where for each spectrum intensity values are divided by
#' the respective intensity values in `y` (if `y` is an object of
#' class `ir`), or where all intensity values are divided by `y` if
#' `y` is a numeric value.
#' @examples
#' # division with y as ir object
#' x1 <-
#'   ir::ir_divide(ir::ir_sample_data, ir::ir_sample_data)
#' x1 <-
#'   ir::ir_divide(ir::ir_sample_data, ir::ir_sample_data[1, ])
#'
#' # division with y being a numeric value
#' x2 <-
#'   ir::ir_divide(ir::ir_sample_data, y = 20)
#' @export
ir_divide <- function(x, y) {

  # checks
  ir_check_ir(x)
  stopifnot(inherits(y, "ir") || (is.numeric(y) && length(y) == 1))
  if(inherits(y, "ir")) {
    ir_check_ir(y)

    if(nrow(y) != 1 && nrow(y) != nrow(x)) {
      rlang::abort("`y` must either have only one row or as many rows as `x`.")
    }
    if(nrow(y) == 1) {
      y <- rep(y, nrow(x))
    }

    cond <-
      purrr::map_lgl(seq_len(nrow(x)), function(i) {
        !identical(x$spectra[[i]]$x, y$spectra[[i]]$x)
      })
    if(any(cond)) {
      rlang::abort(paste0("Not all spectra in `y` have x axis values matching those in `x`. Mismatches have been found for spectra ", paste(which(cond), collapse = ", "), "."))
    }
    y_is_ir <- TRUE

  } else {
    y_is_ir <- FALSE
  }

  x %>%
    dplyr::mutate(
      spectra =
        if(y_is_ir) {
          purrr::map(seq_along(.data$spectra), function(i) {
            z <- .data$spectra[[i]]
            z$y <- z$y / y$spectra[[i]]$y
            z
          })
        } else {
          purrr::map(seq_along(.data$spectra), function(i) {
            z <- .data$spectra[[i]]
            z$y <- z$y / y
            z
          })
        }
    )

}


#' Arithmetic operations for `ir` objects
#'
#' @param e1 An object of class `ir`.
#' @param e2 An object of class `ir` or a numeric value.
#'
#' @examples
#' ## addition
#' ir::ir_sample_data + ir::ir_sample_data
#' ir::ir_sample_data + 2
#'
#' ## subtraction
#' ir::ir_sample_data - ir::ir_sample_data
#' ir::ir_sample_data - 2
#'
#' ## multiplication
#' ir::ir_sample_data * ir::ir_sample_data
#' ir::ir_sample_data * 2
#'
#' ## division
#' ir::ir_sample_data / ir::ir_sample_data
#' ir::ir_sample_data / 2
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

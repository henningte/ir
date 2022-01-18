#' Subtract infrared spectra
#'
#' \code{ir_subtract} takes two objects of class \code{ir}, \code{x} and
#' \code{y}, and subtracts the intensity values of spectra in matching rows from
#' \code{y} from that of \code{x}. Alternatively, takes an object of class
#' \code{ir}, \code{x}, and a numeric value, \code{y}, and subtracts \code{y}
#' from all intensity values in \code{x}.
#'
#' @param x An object of class \code{\link{ir}}.
#' @param y An object of class \code{\link{ir}} or a numeic value. If \code{y}
#' is an object of class \code{ir}, it must have the same number of rows as
#' \code{x} and the same x axis values (e.g. wavenumber values) in each matching
#' spectrum as in \code{x}.
#' @return \code{x} where for each spectrum the respective intensity values in
#' \code{y} are subtracted (if \code{y} is an object of class \code{ir}), or
#' where for each spectrum \code{y} has been subtracted from the intensity
#' values.
#' @examples
#' # subtracting two objects of class ir
#' x1 <-
#'   ir::ir_subtract(ir::ir_sample_data, ir::ir_sample_data)
#'
#' # subtracting a numeric value from an object of class \code{ir}.
#' x2 <-
#'   ir::ir_subtract(ir::ir_sample_data, 20)
#' @export
ir_subtract <- function(x, y) {

  # checks
  ir_check_ir(x)
  stopifnot(inherits(y, "ir") || (is.numeric(y) && length(y) == 1))
  if(inherits(y, "ir")) {
    ir_check_ir(y)
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
#' \code{ir_add} takes two objects of class \code{ir}, \code{x} and
#' \code{y}, and adds the intensity values of spectra in matching rows from
#' \code{y} to that of \code{x}.
#'
#' @inheritParams ir_subtract
#' @return \code{x} where for each spectrum the respective intensity values in
#' \code{y} are added.
#' @examples
#' x1 <-
#'   ir::ir_add(ir::ir_sample_data, ir::ir_sample_data)
#' @export
ir_add <- function(x, y) {

  # checks
  ir_check_ir(x)
  stopifnot(inherits(y, "ir") || (is.numeric(y) && length(y) == 1))
  if(inherits(y, "ir")) {
    ir_check_ir(y)
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
#' \code{ir_multiply} takes two objects of class \code{ir}, \code{x} and
#' \code{y}, and multiplies their intensity values, or it takes one object of
#' class \code{ir}, \code{x}, and one numeric value, \code{y}, and multiplies
#' all intensity values in \code{x} with \code{y}.
#'
#' @inheritParams ir_subtract
#' @return \code{x} where for each spectrum intensity values are multiplied with
#' the respective intensity values in \code{y} (if \code{y} is an object of
#' class \code{ir}), or where all intensity values are multiplied with \code{y}
#' if \code{y} is a numeric value.
#' @examples
#' # multiplication with y as ir object
#' x1 <-
#'   ir::ir_multiply(ir::ir_sample_data, ir::ir_sample_data)
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
#' \code{ir_divide} takes two objects of class \code{ir}, \code{x} and
#' \code{y}, and divides their intensity values, or it takes one object of
#' class \code{ir}, \code{x}, and one numeric value, \code{y}, and divides
#' all intensity values in \code{x} by \code{y}.
#'
#' @inheritParams ir_subtract
#' @return \code{x} where for each spectrum intensity values are divided by
#' the respective intensity values in \code{y} (if \code{y} is an object of
#' class \code{ir}), or where all intensity values are divided by \code{y} if
#' \code{y} is a numeric value.
#' @examples
#' # division with y as ir object
#' x1 <-
#'   ir::ir_divide(ir::ir_sample_data, ir::ir_sample_data)
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

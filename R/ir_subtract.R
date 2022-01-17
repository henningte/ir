#' Subtract infrared spectra
#'
#' \code{ir_subtract} takes two objects of class \code{ir}, \code{x} and
#' \code{y}, and subtracts the intensity values of spectra in matching rows from
#' \code{y} from that of \code{x}.
#'
#' @param x An object of class \code{\link{ir}}.
#' @param y An object of class \code{\link{ir}} with the same number of rows as
#' \code{x} and the same x axis values (e.g. wavenumber values) in each matching
#' spectrum as in \code{x}.
#' @return \code{x} where for each spectrum the respective intensity values in
#' \code{y} are subtracted.
#' @examples
#' x1 <-
#'   ir::ir_subtract(ir::ir_sample_data, ir::ir_sample_data)
#' @export
ir_subtract <- function(x, y) {

  # checks
  ir_check_ir(x)
  ir_check_ir(y)
  cond <-
    purrr::map_lgl(seq_len(nrow(x)), function(i) {
      !identical(x$spectra[[i]]$x, y$spectra[[i]]$x)
    })
  if(any(cond)) {
    rlang::abort(paste0("Not all spectra in `y` have x axis values matching those in `x`. Mismatches have been found for spectra ", paste(which(cond), collapse = ", "), "."))
  }

  x %>%
    dplyr::mutate(
      spectra = purrr::map(seq_along(.data$spectra), function(i) {
        z <- .data$spectra[[i]]
        z$y <- z$y - y$spectra[[i]]$y
        z
      })
    )

}

#' Add infrared spectra
#'
#' \code{ir_add} takes two objects of class \code{ir}, \code{x} and
#' \code{y}, and adds the intensity values of spectra in matching rows from
#' \code{y} to that of \code{x}.
#'
#' @param x An object of class \code{\link{ir}}.
#' @param y An object of class \code{\link{ir}} with the same number of rows as
#' \code{x} and the same x axis values (e.g. wavenumber values) in each matching
#' spectrum as in \code{x}.
#' @return \code{x} where for each spectrum the respective intensity values in
#' \code{y} are added.
#' @examples
#' x1 <-
#'   ir::ir_add(ir::ir_sample_data, ir::ir_sample_data)
#' @export
ir_add <- function(x, y) {

  # checks
  ir_check_ir(x)
  ir_check_ir(y)
  cond <-
    purrr::map_lgl(seq_len(nrow(x)), function(i) {
      !identical(x$spectra[[i]]$x, y$spectra[[i]]$x)
    })
  if(any(cond)) {
    rlang::abort(paste0("Not all spectra in `y` have x axis values matching those in `x`. Mismatches have been found for spectra ", paste(which(cond), collapse = ", "), "."))
  }

  x %>%
    dplyr::mutate(
      spectra = purrr::map(seq_along(.data$spectra), function(i) {
        z <- .data$spectra[[i]]
        z$y <- z$y + y$spectra[[i]]$y
        z
      })
    )

}

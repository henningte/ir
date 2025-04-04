#' Gets the index of a defined wavenumber value for a spectrum
#'
#' `ir_get_wavenumberindex` gets for a defined wavenumber value or set of
#' wavenumber values the corresponding indices (row number) in an object of
#' class `ir` that has been flattened with [ir_flatten()]. If the
#' specified wavenumber values do not match exactly the wavenumber values in the
#' `ir` object, the indices for the next wavenumber values will be
#' returned, along with a warning.
#'
#' @param x A data.frame with a column x representing the x units of a spectrum
#' or several spectra (e.g. in the form of an object of class
#' [`ir_flat`][ir::ir_new_ir_flat]).
#'
#' @param wavenumber A numeric vector with wavenumber values for which to get
#' indices.
#'
#' @param warn logical value indicating if warnings should be displayed
#' (`TRUE`) or not (`FALSE`).
#'
#' @return An integer vector with the same length as `wavenumber` with the
#' row indices of `x` corresponding to the wavenumber values in `wavenumber`.
#'
#' @examples
#' x_index_1090 <-
#'    ir::ir_sample_data |>
#'    ir::ir_flatten() |>
#'    ir::ir_get_wavenumberindex(wavenumber = 1090)
#'
#' @export
ir_get_wavenumberindex <- function(x, wavenumber, warn = TRUE) {

  # checks
  if(!(is.numeric(wavenumber))) {
    rlang::abort("`wavenumber` must be numeric.")
  }
  if(!is.data.frame(x)) {
    rlang::abort(paste0("`x` must be a data.frame, but is of class ", class(x)[[1]], "."))
  }

  # get matching wavenumber values
  if(nrow(x) == 0) {
    return(NA_integer_)
  } else {
    res <-
      purrr::map_int(wavenumber, function(y){
        ifelse(is.na(y), NA_integer_, which.min(abs(y - x$x)))
      })
  }

  # get the corresponding wavenumber values
  d_wavenumber <- x$x[res]

  # check if the specified wavenumber equals the selected wavenumber value
  if(warn){
    match_wavenumber <-
      purrr::map2_lgl(d_wavenumber, wavenumber, function(x, y) all.equal(x, y) == TRUE)

    if(any(!match_wavenumber)) {
      rlang::warn(paste0(d_wavenumber[!match_wavenumber], " selected instead of ", wavenumber[!match_wavenumber],"."))
    }
  }

  res

}

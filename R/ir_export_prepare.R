#' Prepares `ir` objects for export to `csv`
#'
#' Prepares `ir` objects for export to `csv` files. To export `ir` objects to
#' `csv`, the `spectra` column has to be converted to an own data frame and be
#' exported separately from the metadata.
#' When preparing metadata for export, `ir_export_prepare` takes care of
#' separating measurement units and measurement errors in columns of class
#' [units::units], [errors::errors], and [quantities::quantities] (see the
#' examples).
#'
#' @inheritParams ir_flatten
#'
#' @param what A character value defining what to prepare for export.
#' If `"metadata"`, the metadata will be prepared for export and column
#' `spectra` will be dropped. If `"spectra"`, `x` is converted to an object of
#' class [`ir_flat`][ir::ir_new_ir_flat].
#'
#' @note This function superseded `irp_export_prepare()` from the
#' ['irpeat'](https://github.com/henningte/irpeat/) package.
#'
#' @return A data frame.
#'
#' @examples
#' x_spectra <- ir_export_prepare(ir_sample_data[1:5, ], what = "spectra")
#' x_metadata <- ir_export_prepare(ir_sample_data[1:5, ], what = "metadata")
#'
#' @export
ir_export_prepare <- function(x, what = "metadata", measurement_id = as.character(seq_len(nrow(x)))) {

  ir_check_ir(x)
  what_allowed_values <- c("metadata", "spectra")
  if(length(what) != 1L && !what %in% what_allowed_values) {
    rlang::abort(paste0("`what` must be one of ", paste(what_allowed_values, collapse = ", "), ", but is ", paste(what, collaspe = ", "), "."))
  }

  switch(
    what,
    "metadata" = ir_export_prepare_metadata(x),
    "spectra" = ir_export_prepare_spectra(x, measurement_id = measurement_id)
    )

}


#' @keywords internal
#' @noRd
ir_export_prepare_metadata <- function(x) {

  if(! requireNamespace("quantities", quietly = TRUE)) {
    rlang::abort("Package 'quantities' required, please install it first.")
  }

  d <- data.frame(errors = purrr::map_lgl(x, inherits, "errors"),
                  units = purrr::map_lgl(x, inherits, "units"),
                  quantities = purrr::map_lgl(x, inherits, "quantities"),
                  stringsAsFactors = FALSE)

  if(!any(d$errors)) {
    d_errors <- NULL
  } else {
    d_errors <- purrr::map2_dfc(d$errors, seq_along(d$errors), function(.y, .z) {
      if(.y) {
        d <- data.frame(x = quantities::quantities(x[, .z, drop = TRUE])$errors,
                        stringsAsFactors = FALSE)
        colnames(d) <- paste0(colnames(x)[[.z]], "_errors")
        d
      }
    })
  }

  if(!any(d$units  & !d$quantities)) {
    d_units_units <- NULL
  } else {
    d_units_units <- purrr::map2_dfc(d$units  & !d$quantities, seq_along(d$quantities), function(.y, .z) {
      if(.y) {
        d <- data.frame(x = rep(as.character(attr(x[, .z, drop = TRUE], "units")), nrow(x)),
                        stringsAsFactors = FALSE)
        colnames(d) <- paste0(colnames(x)[[.z]], "_units")
        d
      }
    })
  }

  if(!any(d$quantities)) {
    d_quantities_units <- NULL
  } else {
    d_quantities_units <- purrr::map2_dfc(d$quantities, seq_along(d$quantities), function(.y, .z) {
      if(.y) {
        d <- data.frame(x = rep(as.character(quantities::quantities(x[, .z, drop = TRUE])$units), nrow(x)),
                        stringsAsFactors = FALSE)
        colnames(d) <- paste0(colnames(x)[[.z]], "_units")
        d
      }
    })
  }

  dplyr::bind_cols(ir::ir_drop_spectra(x), d_errors, d_units_units, d_quantities_units)

}

#' @keywords internal
#' @noRd
ir_export_prepare_spectra <- function(x, measurement_id) {

  ir::ir_flatten(x, measurement_id = measurement_id)

}

#' Creates an object of class ir
#'
#' \code{ir_new_ir} is the constructor function for objects of class
#' \code{ir}.
#' An object of class \code{ir} is a \code{\link[tibble]{tibble}} with a
#' sample in each row and a list column containing spectra for each sample.
#'
#' @param spectra A named list in which each element contains spectral data
#' for one measurement. Each list element must be a \code{data.frame} with two
#' columns and a row for each wavenumber value in the spectra data. The first
#' column must contain unique wavenumber values and the second column intensity
#' values of the measured spectrum of the sample.
#'
#' @param sample_id A character vector with the same length as \code{spectra}
#' containing sample names for the measured samples in \code{spectra}.
#'
#' @param metadata A \code{data.frame} with a column
#' \code{sample_id} and a column \code{measurement_id} and optional additional
#' columns containing metadata for the samples. Optionally, an empty
#' \code{data.frame} can be defined if no metadata are available.
#'
#' @return An object of class \code{ir} with the following columns:
#' \describe{
#'   \item{measurement_id}{An integer starting with 1 and increasing by 1
#'   representing an identifier for individual measurements.}
#'   \item{sample_id}{A character identical to \code{sample_id} and
#'   representing an identifier for individual samples.}
#'   \item{spectra}{A list column identical to \code{spectra}.}
#' }
#' ... and additional columns contained in \code{metadata}.
#'
#' @export
ir_new_ir <- function(spectra,
                      sample_id,
                      metadata = tibble::tibble()) {

  # checks
  ir_check_spectra(spectra)
  ir_check_metadata(metadata)
  if(!is.character(sample_id))
    rlang::abort(paste0("`sample_id` must be a character, not, ", class(sample_id)[[1]], "."))
  if(length(sample_id) != length(spectra))
    rlang::abort(paste0("`sample_id` must have the same length as `spectra`."))

  # combine data
  x <-
    tibble::tibble(
      measurement_id = seq_along(spectra),
      sample_id = sample_id,
      spectra =
        lapply(spectra, function(x){
          colnames(x) <- c("x", "y")
          x
        })
    )

  # add metadata
  if(ncol(metadata) > 0) {
    x <-
      dplyr::full_join(metadata, x, by = c("sample_id", "measurement_id"))
  }

  structure(x, class = c("ir", class(x)))

}

#' Checks a list of spectra
#'
#' \code{ir_check_spectra} checks if a list of infrared spectra
#' matches the requirement of the argument \code{spectra} of
#' \code{\link{ir_new_ir}}.
#'
#' @param x A list in which each element contains spectral data for one
#' measurement. Each list element must be a \code{data.frame} with two columns
#' and a row for each wavenumber value in the spectra data. The first column
#' must contain unique wavenumber values and the second column intensity values
#' of the measured spectrum of the sample.
#'
#' @return A list that matches the requirements of the argument \code{spectra}
#' of \code{\link{ir_new_ir}}.
#'
#' @keywords Internal
#' @noRd
ir_check_spectra <- function(x) {

  if(!is.list(x)) {
    rlang::abort(paste0("`x` must be a list, not, ", class(x)[[1]], "."))
  }
  x_is_df <- vapply(x,
                    FUN = inherits, "data.frame",
                    FUN.VALUE = logical(1))
  if(!all(x_is_df)) {
    rlang::abort(paste0("`x` must be a list of data.frames.\n
                        Elements ", which(!x_is_df), " are no data.frames."))
  }
  x_ncol <- vapply(x,
                   FUN = ncol,
                   FUN.VALUE = numeric(1))
  if(!all(x_ncol == 2)) {
    rlang::abort(paste0("Each element of `x` must have two columns.\n
                        Elements ", which(x_ncol != 2), " have not two columns."))
  }
  columns_are_numeric <- do.call(rbind, lapply(x, function(x) vapply(x, is.numeric, FUN.VALUE = rlang::na_lgl)))
  if(!all(columns_are_numeric[, 1, drop = TRUE])) {
    rlang::abort(paste0("The first column of each element of `x` must be numeric.\n
                        Elements ", which(!columns_are_numeric[, 1, drop = TRUE]), " have non-numeric values in the first column."))
  }
  if(!all(columns_are_numeric[, 2, drop = TRUE])) {
    rlang::abort(paste0("The second column of each element of `x` must be numeric.\n
                        Elements ", which(!columns_are_numeric[, 2, drop = TRUE]), " have non-numeric values in the second column."))
  }
  x_values_duplicated <- vapply(x,
                                FUN = function(x) any(duplicated(x[, 1, drop = TRUE])),
                                FUN.VALUE = rlang::na_lgl)
  if(any(x_values_duplicated)) {
    rlang::abort(paste0("The first column of each element of `x` must not contain duplicated values.\n
                        Elements ", which(x_values_duplicated), " have duplicate values in the first column."))
  }

  x

}

#' Checks infrared spectra metadata
#'
#' \code{ir_check_metadata} checks if a \code{data.frame}
#' matches the requirement of the argument \code{metadata} of
#' \code{\link{ir_new_ir}}.
#'
#' @param x A \code{data.frame} with a column
#' \code{sample_id} and a column \code{measurement_id} and optional
#' additional columns containing
#' metadata for the samples. Optionally, an empty \code{data.frame}
#' can be defined if no metadata are available.
#'
#' @return A \code{data.frame} that matches the requirements of the
#' argument \code{metadata} of \code{\link{ir_new_ir}}.
#'
#' @keywords Internal
#' @noRd
ir_check_metadata <- function(x) {

  if(!inherits(x, "data.frame")) {
    rlang::abort(paste0("`metadata` must be a data.frame, not, ", class(x)[[1]], "."))
  }
  if(ncol(x) != 0) {
    if(!any(colnames(x) == "sample_id")) {
      rlang::abort("`metadata` must either contain a column `sample_id` or no columns.")
    }
    if(!any(colnames(x) == "measurement_id")) {
      rlang::abort("`metadata` must either contain a column `measurement_id` or no columns.")
    }
  }

  x

}

#' Checks if an object is of class \code{ir}
#'
#' \code{ir_check_ir} checks if an object is of class
#' \code{\link[ir:ir_new_ir]{ir}}.
#'
#' @param x An object.
#'
#' @return \code{x} if it is of class \code{ir}.
#'
#' @keywords Internal
#' @noRd
ir_check_ir <- function(x) {
  x_sym <- as.character(rlang::get_expr(rlang::enquo(x)))
  if(!inherits(x, "ir"))
   rlang::abort(paste0("`", x_sym, "` must be of class `ir`, not ", class(x)[[1]], "."))
  x
}

#' Drops the column \code{spectra} from an object is of class \code{ir}
#'
#' \code{ir_drop_spectra} removes the column \code{spectra} from an object
#' of class \code{ir} and removes the \code{"ir"} class attribute.
#'
#' @param x An object of class \code{\link[ir:ir_new_ir]{ir}}.
#'
#' @return \code{x} without column \code{spectra} and without \code{"ir"} class
#' attribute.
#'
#' @examples
#' ir::ir_sample_data %>%
#'   ir_drop_spectra()
#'
#' @export
ir_drop_spectra <- function(x) {
  ir_check_ir(x)
  x$spectra <- NULL
  class(x) <- setdiff(class(x), "ir")
  x
}


#### Casting: to ir ####

#' Generic to convert objects to class \code{ir}
#'
#' \code{ir_as_ir} ir the generic to convert an object to an object of class
#' \code{\link{ir}}.
#'
#' @param x An object.
#'
#' @param ... Further arguments passed to individual methods.
#' \itemize{
#' \item If \code{x} is a data frame or an object of class \code{ir}, these are
#'   ignored.
#' }
#' @return An object of class \code{ir}.
#'
#' @export
ir_as_ir <- function(x, ...) {
  UseMethod("ir_as_ir")
}

#' @rdname ir_as_ir
#' @examples
#' # conversion from an ir object
#' ir::ir_sample_data %>%
#'   ir_as_ir()
#'
#' @export
ir_as_ir.ir <- function(x, ...) {
  x
}

#' @rdname ir_as_ir
#' @examples
#' # conversion from a data frame
#' x_ir <- ir::ir_sample_data
#'
#' x_df <-
#'   x_ir %>%
#'   ir_drop_spectra() %>%
#'   dplyr::mutate(
#'     spectra = x_ir$spectra
#'   ) %>%
#'   ir_as_ir()
#'
#' @export
ir_as_ir.data.frame <- function(x, ...) {
  ir_new_ir(spectra = x$spectra, sample_id = x$sample_id, metadata = x[, -match("spectra", colnames(x))])
}

#### Casting: from ir ####

#### Replicate ir objects ####

#' Replicate ir objects
#'
#' \code{rep.ir} is the replicate method for \code{\link{ir}} objects.
#' Replicating and \code{ir} object means to replicate its rows and bind these
#' together to a larger \code{ir} object.
#'
#' @param x An object of class \code{\link{ir}}.
#' @param ... See \code{\link{rep}}.
#' @examples
#' # replicate the sample data
#' x <- rep(ir::ir_sample_data, times = 2)
#' x <- rep(ir::ir_sample_data, each = 2)
#' x <- rep(ir::ir_sample_data, length.out = 3)
#' @return An object of class \code{ir} with replicated spectra.
#' @export
rep.ir <- function(x, ...) {

  x %>%
    dplyr::slice(rep(dplyr::row_number(), ...)) %>%
    dplyr::mutate(
      measurement_id = seq_along(.data$spectra)
    )

}


#### Helper functions ####

#' Drop all non-required columns in an ir object
#'
#' @param x An object of class \code{\link{ir}}
#' @keywords internal
#' @examples
#' x1 <-
#'    ir::ir_sample_data %>%
#'    ir_drop_unneccesary_cols()
#' @export
ir_drop_unneccesary_cols <- function(x) {

  x %>%
    ir_check_ir() %>%
    dplyr::select(dplyr::any_of(c("sample_id", "measurement_id", "spectra")))
}

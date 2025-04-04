#' Creates an object of class `ir`
#'
#' `ir_new_ir` is the constructor function for objects of class
#' `ir`.
#' An object of class `ir` is a [tibble::tbl_df()] with a
#' sample in each row and a list column containing spectra for each sample.
#'
#' @param spectra A named list in which each element contains spectral data
#' for one measurement. Each list element must be a `data.frame` with two
#' columns and a row for each wavenumber value in the spectra data. The first
#' column must contain unique wavenumber values and the second column intensity
#' values of the measured spectrum of the sample.
#'
#' @param metadata An optional `data.frame` with additional
#' columns containing metadata for the spectra in `spectra`. Optionally, an
#' empty `data.frame` can be defined if no metadata are available.
#'
#' @return An object of class `ir` with the following columns:
#' \describe{
#'   \item{spectra}{A list column identical to `spectra`.}
#'   \item{...}{Additional columns contained in `metadata`.}
#' }
#'
#' @examples
#' ir_new_ir(
#'   spectra = ir_sample_data$spectra,
#'   metadata = ir_sample_data %>% dplyr::select(-spectra)
#' )
#'
#' @export
ir_new_ir <- function(spectra,
                      metadata = tibble::tibble()) {

  # checks
  ir_check_spectra(spectra)
  stopifnot(is.data.frame(metadata))
  stopifnot(nrow(metadata) == length(spectra))
  stopifnot(any(colnames(metadata) != "spectra") || ncol(metadata) == 0L)

  # combine data
  x <-
    metadata %>%
    dplyr::bind_cols(
      tibble::tibble(
        spectra =
          lapply(spectra, function(x){
            colnames(x) <- c("x", "y")
            x
          })
      )
    )

  structure(x, class = c("ir", class(x)))

}


#### Subsetting ####

#' Subsetting `ir` objects
#'
#' @name subsetting
#'
#' @inheritParams tibble::subsetting
#'
#' @param x An object of class `ir`.
#'
#' @return If the subsetting operation preserves a valid `spectra` column
#' (see [`ir()`][ir_new_ir]), an object of class `ir` with
#' accordingly subsetted rows or columns. Else a [tibble::tbl_df()] or
#' vector.
NULL

#' @rdname subsetting
#'
#' @examples
#' # subsetting rows
#' ir_sample_data[1, ]
#' ir_sample_data[10:15, ]
#' ir_sample_data[ir_sample_data$sample_type == "office paper", ]
#'
#' # subsetting columns
#' ir_sample_data[, "spectra"]
#' ir_sample_data[["spectra"]]
#' ir_sample_data$spectra
#'
#' # not explicitly selecting the spectra column drops the ir class
#' class(ir_sample_data[, 1])
#' class(ir_sample_data[, "spectra"])
#'
#' @export
"[.ir" <- function(x, i, j, ..., exact = TRUE) {
  ir_reclass_ir(NextMethod())
}

#' @rdname subsetting
#'
#' @export
"$.ir" <- function(x, i) {
  ir_reclass_ir(NextMethod())
}

#' @rdname subsetting
#'
#' @examples
#' # subsetting values
#' ir_sample_data[, 1] # drops the ir class
#' ir_sample_data[, c("id_sample", "spectra")]
#' ir_sample_data$id_sample
#' ir_sample_data[[1, 1]]
#'
#' @export
"[[.ir" <- function(x, i, j, ..., exact = TRUE) {
  ir_reclass_ir(NextMethod())
}

#' @rdname subsetting
#'
#' @examples
#' # setting and replacing columns
#' x <- ir::ir_sample_data
#' x$a <- 3
#' x[, "a"] <- 4
#' x$sample_type <- "a"
#' x[[1]] <- rev(x[[1]])
#'
#' # deleting the spectra column drops the ir class
#' x$spectra <- NULL
#' class(x)
#'
#' @export
"$<-.ir" <- function(x, i, j, ..., value) {
  ir_reclass_ir(NextMethod())
}

#' @rdname subsetting
#'
#' @examples
#' # setting and replacing rows
#' x <- ir::ir_sample_data
#' x[1, ] <- x[2, ]
#' class(x)
#'
#' # setting invalid values in the spectra column drops the ir class
#' x_replacement <- x[1, ]
#' x_replacement$spectra <- list(1)
#' x[1, ] <- x_replacement
#' class(x)
#'
#' @export
"[<-.ir" <- function(i, j, ..., exact = TRUE, value) {
  ir_reclass_ir(NextMethod())
}

#' @rdname subsetting
#'
#' @examples
#' # setting and replacing values
#' x <- ir::ir_sample_data
#' x[[1, 1]] <- 100
#'
#' # replacing an element in the spectra column by an invalid element drops the
#' # ir class attribute
#' x[[3, "spectra"]] <- list(1)
#' class(x)
#'
#' @export
"[[<-.ir" <- function(i, j, ..., exact = TRUE, value) {
  ir_reclass_ir(NextMethod())
}



#' Drops the column `spectra` from an object is of class `ir`
#'
#' `ir_drop_spectra` removes the column `spectra` from an object
#' of class `ir` and removes the `"ir"` class attribute.
#'
#' @param x An object of class [`ir`][ir_new_ir()].
#'
#' @return `x` without column `spectra` and without `"ir"` class
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
  x
}


#### Casting: to ir ####

#' Converts an object to class `ir`
#'
#' `ir_as_ir` converts an object to an object of class [`ir`][ir_new_ir()].
#'
#' @param x An object.
#'
#' @param ... Further arguments passed to individual methods.
#' \itemize{
#' \item If `x` is a data frame, an object of class `ir`, an object of class
#'   `hyperSpec` (from package 'hyperSpec'), or an object of class `Spectra`
#'   (from package 'ChemoSpec'), these are ignored.
#' }
#'
#' @return An object of class `ir` with available metadata from original
#' objects.
#'
#' @export
ir_as_ir <- function(x, ...) {
  UseMethod("ir_as_ir")
}

#' @rdname ir_as_ir
#'
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
#'
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
#' # check that ir_as_ir preserves the input class
#' ir_sample_data %>%
#'   structure(class = setdiff(class(.), "ir")) %>%
#'   dplyr::group_by(sample_type) %>%
#'   ir_as_ir()
#'
#'
#' @export
ir_as_ir.data.frame <- function(x, ...) {
  ir_new_ir(spectra = x$spectra, metadata = x[, -match("spectra", colnames(x))])
}

#' @rdname ir_as_ir
#'
#' @examples
#' # conversion from an ir_flat object
#' x_ir <-
#'   ir::ir_sample_data %>%
#'   ir::ir_flatten() %>%
#'   ir::ir_as_ir()
#'
#' @export
ir_as_ir.ir_flat <- function(x, ...) {
  ir_new_ir(spectra = ir_stack(x)$spectra, metadata = tibble::tibble(id_measurement = colnames(x)[-1]))
}

#' @rdname ir_as_ir
#'
#' @examples
#' # conversion from a hyperSpec object from package hyperSpec
#' if(requireNamespace("hyperSpec")) {
#'   x_hyperSpec <- hyperSpec::laser
#'   x_ir <- ir_as_ir(x_hyperSpec)
#' }
#'
#' @export
ir_as_ir.hyperSpec <- function(x, ...) {

  # spectra
  x_spectra <-
    purrr::map(seq_len(nrow(x@data$spc)), function(i) {
      tibble::tibble(
        x = x@wavelength,
        y = unlist(!!x@data$spc[i, ])
      )
    })

  # metadata
  x_metadata <-
    x@data
  x_metadata <- x_metadata[, colnames(x_metadata) != "spc"]

  ir_new_ir(spectra = x_spectra, metadata = x_metadata)
}

#' @rdname ir_as_ir
#'
#' @examples
#' # conversion from a Spectra object from class ChemoSpec
#' if(requireNamespace("ChemoSpec")) {
#'
#'   ## sample data
#'   x <- ir_sample_data
#'   x_flat <- ir_flatten(x)
#'
#'   ## creation of the object of class "Spectra" (the ChemoSpec package does
#'   ## not contain a sample Spectra object)
#'   n <- nrow(x)
#'   group_vector <- seq(from = 1, to = n, by = 1)
#'   color_vector <- rep("black", times = n)
#'   x_Spectra <- list() # dummy list
#'   x_Spectra$freq <- as.numeric(x_flat[,1, drop = TRUE]) # wavenumber vector
#'   x_Spectra$data <- as.matrix(t(x_flat[,-1])) # absorbance values as matrix
#'   x_Spectra$names <- as.character(seq_len(nrow(x))) # sample names
#'   x_Spectra$groups <- as.factor(group_vector) # grouping vector
#'   x_Spectra$colors <- color_vector # colors used for groups in plots
#'   x_Spectra$sym <- as.numeric(group_vector) # symbols used for groups in plots
#'   x_Spectra$alt.sym <- letters[as.numeric(group_vector)] # letters used for groups in plots
#'   x_Spectra$unit <- c("wavenumbers", "intensity") # unit of x and y axes
#'   x_Spectra$desc <- "NULL" # optional descriptions in plots
#'   attr(x_Spectra, "class") <- "Spectra"
#'
#'   # conversion to ir
#'   x_ir <- ir_as_ir(x_Spectra)
#' }
#'
#' @export
ir_as_ir.Spectra <- function(x, ...) {

  # spectra
  x_spectra <-
    purrr::map(seq_len(nrow(x$data)), function(i) {
      tibble::tibble(
        x = x$freq,
        y = !!x$data[i, ]
      )
    })

  # metadata
  x_metadata <-
    tibble::tibble(
      names = x$names,
      groups = x$groups,
      colors = x$colors,
      sym = x$sym,
      alt.sym = x$alt.sym,
      unit = rep(list(x$unit), length(x$names))
    )

  ir_new_ir(spectra = x_spectra, metadata = x_metadata)
}

#### Casting: from ir ####

#### Replicate ir objects ####

#' Replicate ir objects
#'
#' `rep.ir` is the replicate method for [`ir`][ir_new_ir()] objects.
#' Replicating and `ir` object means to replicate its rows and bind these
#' together to a larger `ir` object.
#'
#' @param x An object of class [`ir`][ir_new_ir()].
#'
#' @param ... See [rep()].
#'
#' @examples
#' # replicate the sample data
#' x <- rep(ir::ir_sample_data, times = 2)
#' x <- rep(ir::ir_sample_data, each = 2)
#' x <- rep(ir::ir_sample_data, length.out = 3)
#'
#' @return An object of class `ir` with replicated spectra.
#'
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
#' @param x An object of class [`ir`][ir_new_ir()]
#'
#' @return `x` with all column except the column `spectra` dropped.
#'
#' @examples
#' x1 <-
#'    ir::ir_sample_data %>%
#'    ir_drop_unneccesary_cols()
#'
#' @keywords internal
#' @noRd
ir_drop_unneccesary_cols <- function(x) {

  x %>%
    ir_check_ir() %>%
    dplyr::select(dplyr::any_of(c("spectra")))
}

#' Helper function for reclassing `ir` objects
#'
#' Reclasses `ir` objects to the correct new class after modification.
#' Checks if the `ir` object (the spectra column) gets invalidated.
#' If so, the `ir` class is dropped. If not, the object is reclassed to
#' an `ir` object.
#'
#' @param x An object to be reclassed to the [`ir()`][ir_new_ir] class.
#'
#' @return `x` as `ir` object if the `spectra` column is valid
#' and `x` if not.
#'
#' @keywords internal
#' @noRd
ir_reclass_ir <- function(x) {

  if(is.null(x)) {
    x
  } else if(! "spectra" %in% colnames(x)) { # spectra column not present
    structure(x, class = setdiff(class(x), "ir"))
  } else if(inherits(try(ir_check_spectra(x$spectra), silent = TRUE), "try-error")) { # spectra column present, but wrong format
    structure(x, class = setdiff(class(x), "ir"))
  } else { # spectra column with correct format present
    structure(x, class = c("ir", setdiff(class(x), "ir")))
  }

}


#' Checks a list of spectra
#'
#' `ir_check_spectra` checks if a list of infrared spectra
#' matches the requirement of the argument `spectra` of
#' [ir_new_ir()].
#'
#' @param x A list in which each element contains spectral data for one
#' measurement. Each list element must be a `data.frame` with two columns
#' and a row for each wavenumber value in the spectra data. The first column
#' must contain unique wavenumber values and the second column intensity values
#' of the measured spectrum of the sample.
#'
#' @return A list that matches the requirements of the argument `spectra`
#' of [ir_new_ir()].
#'
#' @keywords Internal
#' @noRd
ir_check_spectra <- function(x) {

  if(!is.list(x)) {
    rlang::abort(paste0("`x` must be a list, not, ", class(x)[[1]], "."))
  }
  x_is_df <- vapply(x,
                    FUN = function(.x) {inherits(.x, "data.frame") || is.null(.x)},
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
  x_colnames_match <- vapply(x,
                    FUN = function(.x) {all(colnames(.x) %in% c("x", "y")) || is.null(.x)},
                    FUN.VALUE = logical(1))
  if(!all(x_colnames_match)) {
    rlang::abort(paste0('Each element of `x` must have two columns named "x" and "y".\n
                        Elements ', which(!x_colnames_match), ' have different column names.'))
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

#' Checks if an object is of class `ir`
#'
#' `ir_check_ir` checks if an object is of class
#' [`ir`][ir_new_ir()].
#'
#' @param x An object.
#'
#' @return `x` if it is of class `ir`.
#'
#' @keywords Internal
#' @noRd
ir_check_ir <- function(x) {
  x_sym <- as.character(rlang::get_expr(rlang::enquo(x)))
  if(!inherits(x, "ir"))
    rlang::abort(paste0("`", x_sym, "` must be of class `ir`, not ", class(x)[[1]], "."))
  x
}

#' Identifies empty spectra in an `ir` object
#'
#' `ir_identify_empty_spectra()` identifies empty spectra in an object of class
#' [`ir`][ir_new_ir()]. An empty spectrum is a spectrum which has no data values
#' (no rows) or where all intensity values (column `y`) are `NA`.
#'
#' @param x An object of class `ir`.
#'
#' @return A logical vector indicating for each spectrum in `x` whether it is
#' empty (`TRUE`) or not (`FALSE`).
#'
#' @examples
#' ir_identify_empty_spectra(ir::ir_sample_data)
#'
#' @export
ir_identify_empty_spectra <- function(x) {
  purrr::map_lgl(x$spectra, function(.x) nrow(.x) == 0 || all(is.na(.x$y)))
}

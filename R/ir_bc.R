#' @importFrom baseline baseline
NULL
# This is required for `ChemoSpec::baselineSpectra()`

#' Performs baseline correction on infrared spectra
#'
#' `ir_bc` performs baseline correction for infrared spectra. Baseline
#' correction is either performed by using a polynomial with user defined
#' degree fitted to each spectrum (see [ChemoSpec::baselineSpectra()]), or by
#' using a rubberband function that is fitted to each spectrum (see
#' [hyperSpec::spc.rubberband()]), or using a Savitzky-Golay smoothed version of
#' the input spectra (see [ir_bc_sg()]).
#'
#' @name ir_bc
#'
#' @param x An object of class [`ir`][ir_new_ir()].
#'
#' @param method A character value indicating which method should be used
#' for baseline correction. If `method = "polynomial"`, a polynomial
#' is used for baseline correction. If `method = "rubberband"`, a
#' rubberband function is used for baseline correction. If `method = "sg"`,
#' a Savitzky-Golay smoothed version of the input spectra is used for baseline
#' correction.
#'
#' @param ... Further arguments passed to [ir_bc_polynomial()],
#' [ir_bc_rubberband()] or [ir_bc_sg()].
#'
#' @param return_bl A logical value indicating if for each spectrum the baseline
#' should be returned instead of the corrected intensity values
#' (`return_bl = TRUE`) or not (`return_bl = FALSE`).
#'
#' @return An object of class `ir` with the baseline corrected spectra, or if
#' `return_bl = TRUE`, the baselines instead of the spectra in column `spectra`.
#'
#' @examples
#' library(dplyr)
#'
#' # rubberband baseline correction
#' x1 <-
#'    ir::ir_sample_data %>%
#'    dplyr::slice(1:10) %>%
#'    ir::ir_bc(method = "rubberband")
#'
#' # polynomial baseline correction
#' x2 <-
#'    ir::ir_sample_data %>%
#'    dplyr::slice(1:10) %>%
#'    ir::ir_bc(method = "polynomial", degree = 2)
#'
#' # Savitzky-Golay baseline correction
#' x3 <-
#'    ir::ir_sample_data %>%
#'    dplyr::slice(1:10) %>%
#'    ir::ir_bc(method = "sg", p = 3, n = 199, ts = 1, m = 0)
#'
#' # return the baseline instead of the baseline corrected spectra
#' x1_bl <-
#'    ir::ir_sample_data %>%
#'    dplyr::slice(1:10) %>%
#'    ir::ir_bc(method = "rubberband", return_bl = TRUE)
#'
#' @export
ir_bc <- function(x,
                  method = "rubberband",
                  return_bl = FALSE,
                  ...) {

  # checks
  ir_check_ir(x)
  if(!(is.logical(return_bl) |
       length(return_bl) == 1)){
    rlang::abort("`return_bl` must be a logical value")
  }

  # perform baseline correction
  switch(
    method,
    polynomial = {
      ir_bc_polynomial(
        x,
        ...,
        return_bl = return_bl
      )
    },
    rubberband = {
      ir_bc_rubberband(
        x,
        return_bl = return_bl,
        ...
      )
    },
    sg = {
      ir_bc_sg(
        x,
        ...,
        return_bl = return_bl
      )
    },
    {rlang::abort("Unknown method.")}
  )

}

#' Performs baseline correction on infrared spectra using a polynomial
#'
#' `ir_bc_polynomial` performs baseline correction for infrared
#' spectra using a polynomial.
#' `ir_bc_polynomial` is an extended wrapper function
#' for [ChemoSpec::baselineSpectra()].
#'
#' @inheritParams ir_bc
#'
#' @param degree An integer value representing the degree of the polynomial
#' used for baseline correction.
#'
#' @return An object of class `ir` with the baseline corrected spectra if
#' `returnbl = FALSE` or the baselines if `returnbl = TRUE`.
#'
#' @param ... Ignored.
#'
#' @seealso
#' [ir_bc()]
#'
#' @examples
#' x2 <-
#'    ir::ir_sample_data %>%
#'    ir::ir_bc_polynomial(degree = 2, return_bl = FALSE)
#'
#' @export
ir_bc_polynomial <- function(x,
                             degree = 2,
                             return_bl = FALSE,
                             ...){

  spectrum_is_empty <- ir_check_for_empty_spectra(x)
  if(all(spectrum_is_empty)) {
    return(x)
  }
  x_or <- x
  x <- x[!spectrum_is_empty, ]

  # flatten x
  x_flat <- ir_flatten(x = x, measurement_id = as.character(seq_len(nrow(x))))

  # dummy grouping vector for samples
  n <- nrow(x)
  group_vector <- seq(from = 1, to = n, by = 1)

  # color vector
  color_vector <- rep("black", times = n)

  # creation of the object of class "Spectra"
  x_cs <- list() # dummy list
  x_cs$freq <- as.numeric(x_flat[,1, drop = TRUE]) # wavenumber vector
  x_cs$data <- as.matrix(t(x_flat[,-1])) # absorbance values as matrix
  x_cs$names <- seq_len(nrow(x)) # sample names
  x_cs$groups <- as.factor(group_vector) # grouping vector
  x_cs$colors <- color_vector # colors used for groups in plots
  x_cs$sym <- as.numeric(group_vector) # symbols used for groups in plots
  x_cs$alt.sym <- letters[as.numeric(group_vector)] # letters used for groups in plots
  x_cs$unit <- c("wavenumbers", "intensity") # unit of x and y axes
  x_cs$desc <- "NULL" # optional descriptions in plots

  # class assignement:
  attr(x_cs, "class") <- "Spectra"

  # compute the baseline and the corrected spectra
  x_bc <-
    ChemoSpec::baselineSpectra(
      x_cs,
      int = FALSE,
      method = "modpolyfit",
      degree = degree,
      retC = FALSE
    )
  grDevices::dev.off()

  # prepare the baseline as data.frame
  x_bl1 <- x_flat
  x_bl1[,-1] <- t(x_bc@baseline)

  # prepare the corrected spectra as data.frame
  x_bc1 <- x_flat
  x_bc1[,-1] <- t(x_bc@corrected)

  # replace the values in x by the baseline corrected values
  x$spectra <- ir_stack(x_bc1)$spectra

  # add baselines to x
  if(return_bl) {
    x$spectra <- ir_stack(x_bl1)$spectra
  }

  x_or$spectra[!spectrum_is_empty] <- x$spectra

  x_or

}


#' Performs baseline correction on infrared spectra using a rubberband algorithm
#'
#' `ir_bc_rubberband` performs baseline correction for infrared spectra
#' using a rubberband algorithm. `ir_bc_rubberband` is an extended wrapper
#' function for [hyperSpec::spc.rubberband()].
#'
#' @inheritParams ir_bc
#'
#' @param do_impute A logical value indicating whether the in baseline the first
#' and last values should be imputed with the second first and second last
#' values, respectively (`TRUE`) or not (`FALSE`). This can be useful in case
#' baseline correction without imputation causes artifacts which sometimes
#' happens with this method.
#'
#' @return An object of class `ir` with the baseline corrected spectra and,
#' if `returnbl = TRUE`,  the baselines.
#'
#' @param ... Ignored.
#'
#' @seealso
#' [ir_bc()]
#'
#' @examples
#' x1 <-
#'    ir::ir_sample_data %>%
#'    ir::ir_bc_rubberband(return_bl = FALSE)
#'
#' @export
ir_bc_rubberband <- function(x,
                             do_impute = FALSE,
                             return_bl = FALSE,
                             ...) {

  spectrum_is_empty <- ir_check_for_empty_spectra(x)
  if(all(spectrum_is_empty)) {
    return(x)
  }
  x_bl <-
    x %>%
    dplyr::mutate(
      spectra =
        purrr::map2(.data$spectra, spectrum_is_empty, function(z, .y) {
          if(.y) {
            return(z)
          }

          # create a hyperSpec object
          z_hs <-
            methods::new(
              "hyperSpec",
              spc = t(z$y),
              wavelength = z$x
            )

          # calculate the baseline
          z_bl <-
            hyperSpec::spc.rubberband(
              z_hs,
              spline = FALSE,
              df = 30
            )@data$spc

          # remove NAs at the beginning and end
          z_bl[is.na(z_bl)] <- 0 # ---todo: remove if bug in hyperSpec is fixe

          res <-
            z %>%
            dplyr::mutate(
              y = z_bl[1, ]
            )

          # impute first and last values
          if(do_impute) {
            res <-
              res %>%
              dplyr::mutate(
                y = c(.data$y[[2]], .data$y[-c(1, length(.data$y))], .data$y[[length(.data$y) - 1L]])
              )
          }

          res

        })
    )

  if(return_bl) {
    x_bl
  } else {
    ir_subtract(x, x_bl)
  }

}


#' Performs baseline correction on infrared spectra using a Savitzky-Golay baseline
#'
#' `ir_bc_sg` computes a smoothed version of spectra using
#' [ir_smooth()] with `method = "sg"` and uses this as baseline
#' which is subtracted from the spectra to perform a baseline correction
#' \insertCite{Lasch.2012}{ir}.
#'
#' @inheritParams ir_bc
#'
#' @param ... Arguments passed to [ir_smooth()] (except for
#' `method` which is always set to `"sg"`).
#'
#' @references
#' \insertAllCited{}
#'
#' @return An object of class `ir` with the baseline corrected spectra and,
#' if `returnbl = TRUE`,  the baselines.
#'
#' @examples
#' x <-
#'    ir::ir_sample_data %>%
#'    ir::ir_bc_sg(p = 3, n = 199, ts = 1, m = 0, return_bl = FALSE)
#'
#' @export
ir_bc_sg <- function(x, ..., return_bl = FALSE) {

  x_bl <- ir_smooth(x, method = "sg", ...)
  if(return_bl) {
    x_bl
  } else {
    ir_subtract(x, x_bl)
  }

}

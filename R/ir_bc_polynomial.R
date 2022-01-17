#' Performs baseline correction on infrared spectra using a polynomial
#'
#' \code{ir_bc_polynomial} performs baseline correction for infrared
#' spectra using a polynomial.
#' \code{ir_bc_polynomial} is an extended wrapper function
#' for \code{\link[ChemoSpec:baselineSpectra]{baselineSpectra}}.
#'
#' @param x An object of class \code{\link[ir:ir_new_ir]{ir}}.
#' @param degree An integer value representing the degree of the polynomial
#' used for baseline correction.
#' @param return_bl A logical value indicating if for each spectrum the baseline
#' should be returned instead of the corrected intensity values
#' (\code{return_bl = TRUE}) or not (\code{return_bl = FALSE}).
#' @return An object of class \code{ir} with the baseline corrected spectra if
#' \code{returnbl = FALSE} or the baselines if \code{returnbl = TRUE}.
#' @seealso
#' \code{\link{ir_bc}}
#' @examples
#' x2 <-
#'    ir::ir_sample_data %>%
#'    ir::ir_bc_polynomial(degree = 2, return_bl = FALSE)
#' @export
ir_bc_polynomial <- function(x,
                             degree = 2,
                             return_bl = FALSE){

  # flatten x
  x_flat <- ir_flatten(x = x, measurement_id = as.character(x$measurement_id))

  # dummy grouping vector for samples
  n <- nrow(x)
  group_vector <- seq(from = 1, to = n, by = 1)

  # color vector
  color_vector <- rep("black", times = n)

  # creation of the object of class "Spectra"
  x_cs <- list() # dummy list
  x_cs$freq <- as.numeric(x_flat[,1, drop = TRUE]) # wavenumber vector
  x_cs$data <- as.matrix(t(x_flat[,-1])) # absorbance values as matrix
  x_cs$names <- x$measurement_id # sample names
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
    x$baselines <- ir_stack(x_bl1)$spectra
  }

  x

}

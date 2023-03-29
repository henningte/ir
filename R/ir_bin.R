#' Bins infrared spectra
#'
#' `ir_bin` bins intensity values of infrared spectra into bins of a
#' defined width or into a defined number of bins.
#'
#' If a wavenumber value exactly matches the boundary of a bin window, the
#' respective intensity value will be assigned to both neighboring bins.
#'
#' @param x An object of class [`ir`][ir_new_ir()] with integer wavenumber
#' values increasing by 1.
#'
#' @param width An integer value indicating the wavenumber width of each
#' resulting bin.
#'
#' @param new_x_type A character value denoting how new wavenumber values for
#' the computed bins should be stored in the spectra of `x` after binning. Must
#' be one of:
#' \describe{
#'   \item{`"start"`}{New wavenumbers for binned intensities are the start
#'     wavenumber value which defines the start of each bin. The default
#'     (for historical reasons).}
#'   \item{`"mean"`}{New wavenumbers for binned intensities are the average
#'     of the start and end wavenumber values which define the start and end of
#'     each bin.}
#'   \item{`"end"`}{New wavenumbers for binned intensities are the end
#'     wavenumber value which defines the end of each bin.}
#' }
#'
#' @return An object of class `ir` where spectra have been binned.
#'
#' @examples
#' # new wavenumber values are the first wavenumber value for each bin
#' x1 <-
#'    ir::ir_sample_data %>%
#'    ir_bin(width = 50, new_x_type = "start")
#'
#' # new wavenumber values are the last wavenumber value for each bin
#' x2 <-
#'    ir::ir_sample_data %>%
#'    ir_bin(width = 50, new_x_type = "mean")
#'
#' # new wavenumber values are the average of the wavenumber values assigned to
#' # each bin
#' x3 <-
#'    ir::ir_sample_data %>%
#'    ir_bin(width = 50, new_x_type = "end")
#'
#' # compare wavenumber values for first spectra.
#' cbind(x1$spectra[[1]]$x, x2$spectra[[1]]$x, x3$spectra[[1]]$x)
#'
#' @export
ir_bin <- function(x, width = 10, new_x_type = "start") {

  # checks
  ir_check_ir(x)
  if(!is.numeric(width)) {
    rlang::abort("`width` must be numeric.")
  }
  if(length(width) != 1) {
    stop("width must be of length 1.")
  }
  if(width %% 1 != 0) {
    stop("width must be an integer.")
  }
  if(length(new_x_type) != 1 || !is.character(new_x_type)) {
    stop("`new_x_type` must be a character value and one of 'start', 'mean', 'end'.")
  }
  spectrum_is_empty <- ir_identify_empty_spectra(x)
  if(all(spectrum_is_empty)) {
    return(x)
  }

  x_flat <- ir_flatten(x)

  # define bins
  nbins <- diff(range(x_flat$x)) %/% width
  bins_wn <-
    tibble::tibble(
      start = seq(0, nbins * width, width) + min(x_flat$x),
      end = .data$start + width
    )

  # avoid overlapping bins
  index_overlaps <- bins_wn$end[-nrow(bins_wn)] - bins_wn$start[-1]
  bins_index <-
    purrr::map_df(seq_len(nrow(bins_wn)), function(i) {
      .x <- bins_wn$start[[i]]
      .y <- bins_wn$end[[i]]

      tibble::tibble(
        index_bin = i,
        start = which(x_flat$x >= .x)[[1]],
        end = rev(which(x_flat$x <= .y))[[1]],
        wn_start = .x,
        wn_end = .y,
        wn_mean = mean(c(.x, .y)),
        index_rows_x_flat = .data$start:.data$end
      )
    })

  # check how many last entries of x must be dropped
  n_drop <- nrow(x_flat) - bins_index$end[nrow(bins_index)]
  if(n_drop > 0) {
    rlang::warn(paste0("Dropping the last ", n_drop, " values of `x` during binning."))
  }

  # prepare x_flat for binning
  bins_index <- tidyr::unnest(bins_index, cols = .data$index_rows_x_flat)
  x_flat <- x_flat[bins_index$index_rows_x_flat, ]
  x_flat$index_bin <- bins_index$index_bin
  x_flat$x <-
    switch(
      new_x_type,
      "start" = bins_index$wn_start,
      "mean" = bins_index$wn_mean,
      "end" = bins_index$wn_end
    )

  # perform binning
  x_binned <-
    purrr::map_dfr(unique(bins_index$index_bin), function(i) {

      tibble::as_tibble(t(apply(x_flat[x_flat$index_bin == i, ], 2, mean))) %>%
        stats::setNames(nm = names(x_flat))

    }) %>%
    dplyr::select(!dplyr::any_of("index_bin"))

  x$spectra <- ir_stack(x_binned)$spectra
  x

}

#' Bins infrared spectra
#'
#' \code{ir_bin} bins intensity values of infrared spectra into bins of a
#' defined width or into a defined number of bins.
#'
#' If the last bin contains fewer input values than the remaining bins, it
#' will be dropped and a warning will be printed. If a wavenumber value exactly
#' matches the boundary of a bin window, the respective intensity value will be
#' assigned to both neighboring bins.
#'
#' @param x An object of class \code{\link[ir:ir_new_ir]{ir}} with integer
#' wavenumber values increasing by 1.
#' @param width An integer value indicating the wavenumber width of each
#' resulting bin. Must be set to \code{NULL} if \code{n} is specified.
#' @return An object of class \code{ir} representing a binned version of
#' \code{x}.
#' @examples
#' x <-
#'    ir::ir_sample_data %>%
#'    ir_bin(width = 50)
#' @export
ir_bin <- function(x,
                   width = 10) {

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
    purrr::map2_df(bins_wn$start, bins_wn$end, function(x, y){
      tibble::tibble(
        start = which(x_flat$x >= x)[[1]],
        end = rev(which(x_flat$x <= y))[[1]]
      )
    })

  # check how many last entries of x must be dropped
  n_drop <- nrow(x_flat) - bins_index$end[nrow(bins_index)]
  if(n_drop > 0) {
    rlang::warn(paste0("Dropping the last ", n_drop, " values of `x` during binning."))
  }

  # perform binning
  x_binned <-
    purrr::map_df(seq_len(nrow(bins_index)), function(i) {
      dplyr::summarise_all(x_flat[bins_index[i, 1, drop = TRUE]:bins_index[i, 2, drop = TRUE], -1], mean)
    })
  colnames(x_binned) <- as.character(seq_len(nrow(x)))
  x_binned_wn <-
    purrr::map_dbl(seq_len(nrow(bins_wn)), function(i) {
      mean(bins_wn[i, 1, drop = TRUE], bins_wn[i, 2, drop = TRUE])
    })
  x_binned <- dplyr::bind_cols(x = x_binned_wn, x_binned)

  x$spectra <- ir_stack(x_binned)$spectra
  x

}

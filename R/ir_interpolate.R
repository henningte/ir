#' Interpolates intensity values of infrared spectra for new wavenumber values.
#'
#' \code{ir_interpolate} interpolates intensity values for infrared spectra
#' for new wavenumber values.
#'
#' @param x An object of class \code{\link[ir:ir_new_ir]{ir}}.
#' @param start A numerical value indicating the start wavenumber value
#' relative to which new wavenumber values will be interpolated. The value
#' is not allowed to be < \code{floor(firstvalue) - 2}, whereby \code{firstvalue}
#' is the first wavenumber value within \code{x}. If \code{start = NULL},
#' \code{floor(firstvalue)} will be used as first wavenumber value.
#' @param dw A numerical value representing the desired wavenumber value
#' difference between adjacent values.
#' @return An object of class \code{ir} containing the interpolated
#' spectra.
#' @export
ir_interpolate <- function(x,
                           start = NULL,
                           dw = 1) {

  # checks
  ir_check_ir(x)
  x_flat <- ir_flatten(x)
  if(!is.null(start)) {
    if(!is.numeric(start)) {
      rlang::abort("`start` must be numeric, not ", class(start)[[1]], ".")
    }
    if(length(start) != 1) {
      rlang::abort("`start` must be of length 1, not ", length(start), ".")
    }
    if(min(x_flat[, 1, drop = TRUE]) <= start) {
      rlang::abort("`start` must not be smaller than the smallest x axis value of any spectrum in `x` (", min(x_flat[, 1, drop = TRUE]), ").")
    }
  } else {
    start <- floor(min(x_flat$x, na.rm = TRUE))
  }
  if(!is.numeric(dw)) {
    rlang::abort("`dw` must be numeric, not ", class(dw)[[1]], ".")
  }
  if(length(dw) != 1) {
    rlang::abort("`dw` must be of length 1, not ", length(dw), ".")
  }

  x_flat <- dplyr::arrange(x_flat, x)

  # fill NA values to support easy indexing
  x_flat <- as.data.frame(data.table::nafill(x_flat, type = "locf"))
  colnames(x_flat) <- c("x", x$measurement_id)

  # define the new wavenumber values
  d <- tibble::tibble(x = seq(from = start,
                              to = max(x_flat$x, na.rm = TRUE),
                              by = dw))

  index <- purrr::map_df(d$x, function(x){

    # index of the next smaller wavenumber value
    if(length(which(x_flat$x <= x)) > 0) {
      nextsmaller <- which(x_flat$x <= x)[length(which(x_flat$x <= x))]
    } else {
      nextsmaller <- NA
    }

    # index of the next larger wavenumber value
    if(length(which(x_flat$x >= x)) > 0) {
      nextlarger <- which(x_flat$x >= x)[[1]]
    } else {
      nextlarger <- nextsmaller
    }

    # difference to the next smaller wavenumber value
    differencenextsmaller <- ifelse(!is.na(nextsmaller),
                                    x - x_flat$x[nextsmaller],
                                    NA)
    differencenextsmaller <- ifelse(differencenextsmaller == 0, NA, differencenextsmaller)

    data.frame(previous = nextsmaller,
               following = nextlarger,
               difference_to_previous = differencenextsmaller,
               stringsAsFactors = FALSE)

  })

  # interpolate new wavenumber values
  new_x_flat <- cbind(d$x, interpolate_linear(x = x_flat$x, y = x_flat[,-1, drop = FALSE], index = index))
  x$spectra <- ir_stack(new_x_flat)$spectra
  x

}

# function definition in order to interpolat values
interpolate_linear <- function(x,
                               y,
                               index) {

  purrr::map_df(seq_len(nrow(index)), function(i){
    index <- unlist(index[i, ])
    if(is.na(index[3])){
      y[index[2], ]
    }else{
      gradient <- (y[index[2], ] - y[index[1], ])/(x[index[2]] - x[index[1]])
      y[index[1], ] + gradient * index[3]
    }
  })

}

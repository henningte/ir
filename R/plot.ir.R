#' Plots an object of class ir.
#'
#' \code{plot.ir} is the plot method for objects of class
#' \code{ir}.
#'
#' @param x An object of class \code{\link[ir:ir_new_ir]{ir}}.
#' @param ... Further arguments, will be ignored
#' @return An object of class \code{\link[ggplot2:ggplot]{ggplot2}}
#' @export
plot.ir <- function(x,
                    ...) {

  ir_check_ir(x)
  x_unnested <- tidyr::unnest(x, cols = .data$spectra)
  groups <- if("measurement_id" %in% colnames(x_unnested)) {
    x_unnested$measurement_id
  } else {
    NULL
  }
  ggplot2::ggplot(x_unnested) +
    ggplot2::geom_path(ggplot2::aes(x = .data$x,
                                    y = .data$y,
                                    group = groups))

}

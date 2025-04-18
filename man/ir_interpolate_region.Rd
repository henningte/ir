% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ir_interpolate_region.R
\name{ir_interpolate_region}
\alias{ir_interpolate_region}
\title{Interpolates selected regions in infrared spectra in an \code{ir} object}
\usage{
ir_interpolate_region(x, range)
}
\arguments{
\item{x}{An object of class \code{\link[=ir_new_ir]{ir}}.}

\item{range}{A \code{data.frame} with a row for each region to interpolate
linearly and two columns:
\describe{
\item{start}{A numeric vector with start values for regions to interpolate
linearly (x axis values).}
\item{end}{A numeric vector with end values for regions to interpolate
linearly (x axis values).}
}
For each row in \code{range}, the values in \code{range$start} have to be
smaller than the values in \code{range$end}.}
}
\value{
\code{x} with the defined wavenumber region(s) interpolated linearly.
}
\description{
\code{ir_interpolate_region} linearly interpolates a user-defined region in
infrared spectra.
}
\examples{
# interpolation range
range <- data.frame(start = 1000, end = 1500)

# do the interpolation
x <-
   ir::ir_sample_data |>
   ir::ir_interpolate_region(range = range)

}

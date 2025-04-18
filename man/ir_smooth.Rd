% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ir_smooth.R
\name{ir_smooth}
\alias{ir_smooth}
\title{Smooths infrared spectra in an \code{ir} object}
\usage{
ir_smooth(
  x,
  method = "sg",
  p = 3,
  n = p + 3 - p\%\%2,
  ts = 1,
  m = 0,
  k = 111,
  ...
)
}
\arguments{
\item{x}{An object of class \code{\link[=ir_new_ir]{ir}}.}

\item{method}{A character value specifying which smoothing method to apply.
If \code{method = "sg"}, a Savitzky-Golay filter will be applied on the
spectra. The Savitzky-Golay smoothing will be performed using the function
\code{\link[signal:sgolayfilt]{signal::sgolayfilt()}}. If \code{method = "fourier"},
Fourier smoothing will be performed. Fourier transformation of the spectra is
performed using the fast discrete Fourier transformation (FFT) as implemented
in \code{\link[fda:smooth.basis]{fda::smooth.basis()}}. A smoothing function can be
defined by the argment \code{f}.}

\item{p}{An integer value representing the filter order (i.e. the degree of
the polynom) of the Savitzky-Golay filter if \code{method = "sg"}.}

\item{n}{An odd integer value representing the length (i.e. the number of
wavenumber values used to construct the polynom) of the Savitzky-Golay filter
if \code{method = "sg"}.}

\item{ts}{time scaling factor. See \code{\link[signal:sgolayfilt]{signal::sgolayfilt()}}.}

\item{m}{An integer value representing the mth derivative to compute. This
option can be used to compute derivatives of spectra. See
\code{\link[signal:sgolayfilt]{signal::sgolayfilt()}}.}

\item{k}{A positive odd integer representing the number of Fourier basis
functions to use as smoothed representation of the spectra if
\code{method = "fourier"}.}

\item{...}{additional arguments (ignored).}
}
\value{
\code{x} with smoothed spectra.
}
\description{
\code{ir_smooth} applies smoothing functions to infrared spectra.
\code{ir_smooth} either performs Savitzky-Golay smoothing, using on
\code{\link[signal:sgolayfilt]{signal::sgolayfilt()}}, or Fourier smoothing using
\code{\link[fda:smooth.basis]{fda::smooth.basis()}}. Savitzky-Golay smoothing can
also be used to compute derivatives of spectra.
}
\details{
When \code{x} contains spectra with different wavenumber values, the
filters are applied for each spectra only on existing wavenumber values. This
means that the filter window (if \code{method == "sg"}) will be different for
these different spectra.
}
\examples{
#' # Savitzky-Golay smoothing
if(! requireNamespace("signal", quietly = TRUE)) {
  x1 <-
     ir::ir_sample_data[1:5, ] |>
     ir::ir_smooth(method = "sg", p = 3, n = 51, ts = 1, m = 0)
}

# Fourier smoothing
if(! requireNamespace("fda", quietly = TRUE)) {
  x2 <-
     ir::ir_sample_data[1:5, ] |>
     ir::ir_smooth(method = "fourier", k = 21)
}

# computing derivative spectra with Savitzky-Golay smoothing (here: first
# derivative)
if(! requireNamespace("signal", quietly = TRUE)) {
  x3 <-
     ir::ir_sample_data[1:5, ] |>
     ir::ir_smooth(method = "sg", p = 3, n = 51, ts = 1, m = 1)
}

}

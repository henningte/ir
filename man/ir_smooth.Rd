% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ir_smooth.R
\name{ir_smooth}
\alias{ir_smooth}
\title{Smoothes infrared spectra.}
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
\item{x}{An object of class \code{\link[ir:ir_new_ir]{ir}}.}

\item{method}{A character value specifying which smoothing method to apply.
If \code{method = "sg"}, a Savitzky-Golay filter will be applied on the
spectra. The Savitzky-Golay smoothing will be performed using the function
\code{\link[signal:sgolayfilt]{sgolayfilt}}.
If \code{method = "fourier"}, Fourier smoothing will be performed.
Fourier transformation of the spectra is performed using the fast
discrete Fourier transformation (FFT) as implemented in
\code{\link[fda:smooth.basis]{smooth.basis}}. A smoothing function can be defined by the
argment \code{f}.}

\item{p}{An integer value representing the filter order (i.e. the degree of the
polynom) of the Savitzky-Golay filter if \code{method = "sg"}.}

\item{n}{An odd integer value representing the length (i.e. the
number of wavenumber values used to construct the polynom) of the
Savitzky-Golay filter if \code{method = "sg"}.}

\item{ts}{time scaling factor. See \code{\link[signal:sgolayfilt]{sgolayfilt}}.}

\item{m}{An integer value representing the mth derivative to compute. This option
can be used to compute derivatives of spectra. See \code{\link[signal:sgolayfilt]{sgolayfilt}}.}

\item{k}{A positive odd integer representing the number of Fourier
basis functions to use as smoothed representation of the spectra
if \code{method = "fourier"}.}

\item{...}{additional arguments (ignored).}
}
\value{
An object of class \code{ir} containing the smoothed
spectra.
}
\description{
\code{ir_smooth} applies smoothing functions to infrared spectra.
\code{ir_smooth} either performs
Savitzky-Golay smoothing, based on \code{\link[signal:sgolayfilt]{sgolayfilt}},
or Fourier smoothing using \code{\link[fda:smooth.basis]{smooth.basis}}.
Savitzky-Golay smoothing can also be used to compute derivatives of spectra.
}

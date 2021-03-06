% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ir.R
\name{ir_drop_spectra}
\alias{ir_drop_spectra}
\title{Drops the column \code{spectra} from an object is of class \code{ir}.}
\usage{
ir_drop_spectra(x)
}
\arguments{
\item{x}{An object of class \code{\link[ir:ir_new_ir]{ir}}.}
}
\value{
A \code{data.frame} identical to \code{x}, but without
column \code{spectra}.
}
\description{
\code{ir_drop_spectra} removes the column \code{spectra} from an object
of class \code{ir} and removes the \code{"ir"} class label.
}

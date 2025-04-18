% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ir_import_csv.R
\name{ir_import_csv}
\alias{ir_import_csv}
\title{Imports infrared spectra from various files}
\usage{
ir_import_csv(filenames, sample_id = "from_filenames", ...)
}
\arguments{
\item{filenames}{A character vector representing the complete paths to
the \code{.csv} files to import.}

\item{sample_id}{Either:
\itemize{
\item \code{NULL}: Nothing additional happens.
\item A character vector with the same length as \code{filenames}: This
vector will be added as column \code{sample_id} to the \code{ir} object.
\item \code{"from_filenames"}: The file name(s) will be used as values for
a new column \code{sample_id} to add (the default).
\item \code{"from_colnames"}: The header in the csv file will be used as
values for a new column \code{sample_id} to add.
}}

\item{...}{Further arguments passed to
\code{\link[utils:read.table]{read.csv()}}.}
}
\value{
An object of class \code{\link[=ir_new_ir]{ir}} containing the
infrared spectra extracted from the \code{.csv} file(s).
}
\description{
\code{ir_import_csv} imports raw infrared spectra from one or more \code{.csv} file
that contains at least one spectrum, with x axis values (e.g. wavenumbers) in
the first column and intensity values of spectra in remaining columns. Note
that the function does not perform any checks for the validity of the content
read from the .csv file.
}
\examples{

# import data from csv files
d <-
  ir::ir_import_csv(
    system.file(package = "ir", "extdata/klh_hodgkins_mir.csv"),
    sample_id = "from_colnames"
  )

}

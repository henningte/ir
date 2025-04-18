% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidyverse.R
\name{select.ir}
\alias{select.ir}
\title{Subset columns in \code{ir} objects using column names and types}
\source{
\code{\link[dplyr:select]{dplyr::select()}}
}
\usage{
select.ir(.data, ...)
}
\arguments{
\item{.data}{An object of class \code{ir}.}

\item{...}{<\code{\link[dplyr:dplyr_tidy_select]{tidy-select}}> One or more unquoted
expressions separated by commas. Variable names can be used as if they
were positions in the data frame, so expressions like \code{x:y} can
be used to select a range of variables.}
}
\value{
\code{.data} with the selected columns. If the \code{spectra} column is dropped,
the \code{ir} class is dropped, else the object is of class \code{ir}.
}
\description{
Subset columns in \code{ir} objects using column names and types
}
\examples{
## select
dplyr::select(ir_sample_data, spectra)
dplyr::select(ir_sample_data, holocellulose) # drops ir class


}
\seealso{
Other tidyverse: 
\code{\link{arrange.ir}()},
\code{\link{distinct.ir}()},
\code{\link{extract.ir}()},
\code{\link{filter-joins}},
\code{\link{filter.ir}()},
\code{\link{group_by}},
\code{\link{mutate}},
\code{\link{mutate-joins}},
\code{\link{nest}},
\code{\link{pivot_longer.ir}()},
\code{\link{pivot_wider.ir}()},
\code{\link{rename}},
\code{\link{rowwise.ir}()},
\code{\link{separate.ir}()},
\code{\link{separate_rows.ir}()},
\code{\link{slice}},
\code{\link{summarize}},
\code{\link{unite.ir}()}
}
\concept{tidyverse}

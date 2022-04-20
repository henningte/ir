#' Sample object of class `ir`
#'
#' A sample object of class [`ir`][ir_new_ir()]. The data set
#' contains ATR-MIR spectra for a set of organic reference materials
#' along with their metadata (types of samples and a description) and
#' accessory data (Klason lignin mass fraction and holocellulose mass fraction).
#'
#' @format A data frame with 58 rows and 7 variables:
#' \describe{
#'   \item{id_measurement}{See [ir_new_ir()].}
#'   \item{id_sample}{See [ir_new_ir()].}
#'   \item{sample_type}{A character vector containing class labels
#'   for the types of reference materials.}
#'   \item{sample_comment}{A character vector containing comments to
#'   each sample.}
#'   \item{klason_lignin}{A numeric vector with the mass fractions of
#'   Klason lignin in each sample.}
#'   \item{holocellulose}{A numeric vector with the mass fractions of
#'   holocellulose in each sample.}
#'   \item{spectra}{See [ir_new_ir()].}
#' }
#' @source The data set was derived from <https://www.nature.com/articles/s41467-018-06050-2>
#' and published by \insertCite{Hodgkins.2018;textual}{ir} under the CC BY 4.0 license <https://creativecommons.org/licenses/by/4.0/>.
#' \insertCite{Hodgkins.2018;textual}{ir} originally derived the data on Klason Lignin and Holocellulose content from
#' \insertCite{LaCruz.2016;textual}{ir} <https://www.liebertpub.com/doi/full/10.1089/ees.2014.0402>.
#' @references
#'   \insertAllCited{}
"ir_sample_data"

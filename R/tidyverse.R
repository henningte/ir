#### Tidyverse methods for ir objects ####

#' Subset rows in `ir` objects using column values
#'
#' @inheritParams dplyr::filter
#'
#' @param .data An object of class `ir`.
#'
#' @source [dplyr::filter()]
#'
#' @examples
#' ## filter
#' dplyr::filter(ir_sample_data, sample_type == "office paper")
#'
#' @export
filter.ir <- function(.data, ..., .preserve = FALSE) {
  ir_reclass_ir(NextMethod())
}


#' Arrange rows in `ir` objects by column values
#'
#' @inheritParams dplyr::arrange
#'
#' @param .data An object of class `ir`.
#'
#' @source [dplyr::arrange()]
#'
#' @examples
#' ## arrange
#' dplyr::arrange(ir_sample_data, dplyr::desc(sample_type))
#'
#' @export
arrange.ir <- function(.data, ..., .by_group = FALSE) {
  ir_reclass_ir(NextMethod())
}


#' Group rows in `ir` objects by one or more variables
#'
#' @name group_by
#'
#' @inheritParams dplyr::group_by
#'
#' @param .data An object of class `ir`.
#'
#' @source [dplyr::group_by()]
#'
#' @examples
#' ## group_by
#' dplyr::group_by(ir_sample_data, sample_type)
#'
#' @export
group_by.ir <- function(.data, ..., .add = FALSE, .drop = dplyr::group_by_drop_default(.data)) {
  ir_reclass_ir(NextMethod())
}


#' @rdname group_by
#'
#' @examples
#' ## ungroup
#' dplyr::ungroup(dplyr::group_by(ir_sample_data, sample_type))
#'
#' @export
ungroup.ir <- function(.data, ...) {
  ir_reclass_ir(NextMethod())
}


#' Group input `ir` objects by rows
#'
#' @inheritParams dplyr::rowwise
#'
#' @param data An object of class `ir`.
#'
#' @source [dplyr::rowwise()]
#'
#' @examples
#' ## rowwise
#' dplyr::rowwise(ir_sample_data) %>%
#'   dplyr::mutate(hkl = mean(klason_lignin, holocellulose))
#'
#' @export
rowwise.ir <- function(.data, ...) {
  ir_reclass_ir(NextMethod())
}


#' Mutate an `ir` object by adding new or replacing existing columns
#'
#' @name mutate
#'
#' @inheritParams dplyr::mutate
#'
#' @param .data An object of class `ir`.
#'
#' @source [dplyr::mutate()]
#'
#' @examples
#' ## mutate
#' dplyr::mutate(ir_sample_data, hkl = klason_lignin + holocellulose)
#'
#' @export
mutate.ir <- function(
  .data,
  ...,
  .keep = c("all", "used", "unused", "none"),
  .before = NULL,
  .after = NULL
) {
  ir_reclass_ir(NextMethod())
}

#' @rdname mutate
#'
#' @examples
#' ## transmute
#' dplyr::transmute(ir_sample_data, hkl = klason_lignin + holocellulose)
#'
#' @export
transmute.ir <- function(.data, ...) {
  ir_reclass_ir(NextMethod())
}


#' Subset columns in `ir` objects using column names and types
#'
#' @inheritParams dplyr::select
#'
#' @param .data An object of class `ir`.
#'
#' @source [dplyr::select()]
#'
#' @examples
#' ## select
#' dplyr::select(ir_sample_data, spectra)
#' dplyr::select(ir_sample_data, holocellulose) # drops ir class
#'
#' @export
select.ir <- function(.data, ...) {
  ir_reclass_ir(NextMethod())
}


#' Rename columns in `ir` objects
#'
#' @inheritParams dplyr::rename
#'
#' @param .data An object of class `ir`.
#'
#' @source [dplyr::rename()]
#'
#' @name rename
#'
#' @examples
#' ## rename
#' dplyr::rename(ir_sample_data, hol = "holocellulose")
#' dplyr::rename(ir_sample_data, spec = "spectra") # drops ir class
#'
#' @export
rename.ir <- function(.data, ...) {
  ir_reclass_ir(NextMethod())
}

#' @rdname rename
#'
#' @examples
#' ## rename_with
#' dplyr::rename(ir_sample_data, dplyr::starts_with("id_"))
#' dplyr::rename(ir_sample_data, toupper) # drops ir class
#'
#' @export
rename_with.ir <- function(.data, .fn, .cols = everything(), ...) {
  ir_reclass_ir(NextMethod())
}


#' Subset rows in `ir` objects using their positions
#'
#' @name slice
#'
#' @inheritParams dplyr::slice
#'
#' @param .data An object of class `ir`.
#'
#' @source [dplyr::slice()]
#'
#' @examples
#' ## slice
#' dplyr::slice(ir_sample_data, 1:5)
#'
#' @export
slice.ir <- function(.data, ..., .preserve = FALSE) {
  ir_reclass_ir(NextMethod())
}


#' @rdname slice
#'
#' @examples
#' ## slice_head
#' dplyr::slice_head(ir_sample_data, 5)
#'
#' @export
slice_head.ir <- function(.data, ..., n, prop) {
  ir_reclass_ir(NextMethod())
}


#' @rdname slice
#'
#' @examples
#' ## slice_tail
#' dplyr::slice_tail(ir_sample_data, 5)
#'
#' @export
slice_tail.ir <- function(.data, ..., n, prop) {
  ir_reclass_ir(NextMethod())
}


#' @rdname slice
#'
#' @examples
#' ## slice_min
#' dplyr::slice_min(ir_sample_data, holocellulose, n = 3)
#'
#' @export
slice_min.ir <- function(.data, order_by, ..., n, prop, with_ties = TRUE) {
  ir_reclass_ir(NextMethod())
}


#' @rdname slice
#'
#' @examples
#' ## slice_max
#' dplyr::slice_max(ir_sample_data, holocellulose, n = 3)
#'
#' @export
slice_max.ir <- function(.data, order_by, ..., n, prop, with_ties = TRUE) {
  ir_reclass_ir(NextMethod())
}


#' @rdname slice
#'
#' @examples
#' ## slice_sample
#' set.seed(234)
#' dplyr::slice_sample(ir_sample_data, n = 3)
#'
#' @export
slice_sample.ir <- function(.data, ..., n, prop, weight_by = NULL, replace = FALSE) {
  ir_reclass_ir(NextMethod())
}


#' Summarize each group in a `ir` object to fewer rows
#'
#' @name summarize
#' @aliases summarise
#'
#' @inheritParams dplyr::summarize
#'
#' @param .data An object of class `ir`.
#'
#' @source [dplyr::summarize()]
#'
#' @examples
#' ## summarize
#'
#' # select in each sample_type groups the first spectrum
#' ir_sample_data %>%
#'   dplyr::group_by(sample_type) %>%
#'   dplyr::summarize(spectra = spectra[[1]])
#'
#' @export
summarize.ir <- function(.data, ..., .groups = NULL) {
  ir_reclass_ir(NextMethod())
}

#' @rdname summarize
#'
#' @export
summarise.ir <- summarize.ir


#' Subset distinct/unique rows in `ir` objects
#'
#' @inheritParams dplyr::distinct
#'
#' @param .data An object of class `ir`.
#'
#' @source [dplyr::distinct()]
#'
#' @examples
#' ## distinct
#' dplyr::distinct(rep(ir_sample_data, 2))
#'
#' @export
distinct.ir <- function(.data, ..., .keep_all = FALSE) {
  ir_reclass_ir(NextMethod())
}


#' Pivot an `sf` object from wide to long
#'
#' @inheritParams tidyr::pivot_longer
#'
#' @param .data An object of class `ir`.
#'
#' @source [tidyr::pivot_longer()]
#'
#' @examples
#' ## pivot_longer
#' ir_sample_data %>%
#'   tidyr::pivot_longer(
#'     cols = dplyr::any_of(c("holocellulose", "klason_lignin"))
#'   )
#'
#' @export
pivot_longer.ir <- function(
  data,
  cols,
  names_to = "name",
  names_prefix = NULL,
  names_sep = NULL,
  names_pattern = NULL,
  names_ptypes = list(),
  names_transform = list(),
  names_repair = "check_unique",
  values_to = "value",
  values_drop_na = FALSE,
  values_ptypes = list(),
  values_transform = list(),
  ...)
{
  ir_reclass_ir(NextMethod())
}

#' Pivot an `sf` object from wide to long
#'
#' @inheritParams tidyr::pivot_wider
#'
#' @param .data An object of class `ir`.
#'
#' @source [tidyr::pivot_wider()]
#'
#' @examples
#' ## pivot_wider
#' ir_sample_data %>%
#'   tidyr::pivot_longer(
#'     cols = dplyr::any_of(c("holocellulose", "klason_lignin"))
#'   ) %>%
#'   tidyr::pivot_wider(names_from = name, values_from = value)
#'
#' @export
pivot_wider.ir <- function(
  data,
  id_cols = NULL,
  names_from = name,
  names_prefix = "",
  names_sep = "_",
  names_glue = NULL,
  names_sort = FALSE,
  names_repair = "check_unique",
  values_from = value,
  values_fill = NULL,
  values_fn = NULL,
  ...)
{
  ir_reclass_ir(NextMethod())
}


#' Nest and unnest an `sf` object
#'
#' @name nest
#'
#' @inheritParams tidyr::nest
#'
#' @param .data An object of class `ir`.
#'
#' @source [tidyr::nest()]
#'
#' @examples
#' ## nest
#' ir_sample_data %>%
#'   tidyr::nest(
#'     contents = c(holocellulose, klason_lignin)
#'   )
#'
#' @export
nest.ir <- function(.data, ..., .names_sep = NULL, .key = deprecated()) {
  ir_reclass_ir(NextMethod())
}


#' @rdname nest
#'
#' @examples
#' ## unnest
#' ir_sample_data %>%
#'   tidyr::nest(
#'     contents = c(holocellulose, klason_lignin)
#'   ) %>%
#'   tidyr::unnest("contents")
#'
#' @export
unnest.ir <- function(
  data,
  cols,
  ...,
  keep_empty = FALSE,
  ptype = NULL,
  names_sep = NULL,
  names_repair = "check_unique",
  .drop = deprecated(),
  .id = deprecated(),
  .sep = deprecated(),
  .preserve = deprecated())
{
  NextMethod()
}


#' Separate a character column in an `ir` object into multiple columns with a regular expression or numeric locations
#'
#' @inheritParams tidyr::separate
#'
#' @param data An object of class `ir`.
#'
#' @source [tidyr::separate()]
#'
#' @examples
#' ## separate
#' ir_sample_data %>%
#'   tidyr::separate(
#'     id_sample,  c("a", "b", "c")
#'   )
#'
#' @export
separate.ir <- function(
  data,
  col,
  into,
  sep = "[^[:alnum:]]+",
  remove = TRUE,
  convert = FALSE,
  extra = "warn",
  fill = "warn",
  ...)
{
  ir_reclass_ir(NextMethod())
}


#' Unite multiple columns in an `ir` object into one by pasting strings together
#'
#' @inheritParams tidyr::unite
#'
#' @param data An object of class `ir`.
#'
#' @source [tidyr::unite()]
#'
#' @examples
#' ## unite
#' ir_sample_data %>%
#'   tidyr::separate(
#'     id_sample,  c("a", "b", "c")
#'   ) %>%
#'   tidyr::unite(col = id_sample, a, b, c)
#'
#' @export
unite.ir <- function(data, col, ..., sep = "_", remove = TRUE, na.rm = FALSE) {
  ir_reclass_ir(NextMethod())
}


#' Extract a character column in an `ir` object into multiple columns using regular expression groups
#'
#' @inheritParams tidyr::extract
#'
#' @param data An object of class `ir`.
#'
#' @source [tidyr::extract()]
#'
#' @examples
#' ## extract
#' ir_sample_data %>%
#'   tidyr::extract(
#'     id_sample,  "a"
#'   )
#'
#' @export
extract.ir <- function(
  data,
  col,
  into,
  regex = "([[:alnum:]]+)",
  remove = TRUE,
  convert = FALSE,
  ...
) {
  ir_reclass_ir(NextMethod())
}


#' Separate a collapsed column in an `ir` object into multiple rows
#'
#' @inheritParams tidyr::separate_rows
#'
#' @param data An object of class `ir`.
#'
#' @source [tidyr::separate_rows()]
#'
#' @examples
#' ## separate_rows
#' ir_sample_data %>%
#'   tidyr::unite(
#'     content, holocellulose, klason_lignin
#'   ) %>%
#'   tidyr::separate_rows(
#'     content
#'   )
#'
#' @export
separate_rows.ir <- function(
  data,
  col,
  into,
  sep = "[^[:alnum:]]+",
  remove = TRUE,
  convert = FALSE,
  extra = "warn",
  fill = "warn",
  ...)
{
  ir_reclass_ir(NextMethod())
}


#### joins ####

#' Mutating joins for an `ir` object
#'
#' @name mutate-joins
#'
#' @param x An object of class `ir`.
#'
#' @inheritParams dplyr::left_join
#'
#' @source [dplyr::mutate-joins()]
NULL


#' @rdname mutate-joins
#'
#' @examples
#' ## inner_join
#' set.seed(234)
#' dplyr::inner_join(
#'   ir_sample_data,
#'   tibble::tibble(
#'     id_measurement = c(1:5, 101:105),
#'     nitrogen_content = rbeta(n = 10, 0.2, 0.1)
#'   ),
#'   by = "id_measurement"
#' )
#'
#' @export
inner_join.ir <- function(
  x,
  y,
  by = NULL,
  copy = FALSE,
  suffix = c(".x", ".y"),
  ...,
  keep = FALSE,
  na_matches = c("na", "never")
) {
  ir_reclass_ir(NextMethod())
}


#' @rdname mutate-joins
#'
#' @examples
#' ## left_join
#' set.seed(234)
#' dplyr::left_join(
#'   ir_sample_data,
#'   tibble::tibble(
#'     id_measurement = c(1:5, 101:105),
#'     nitrogen_content = rbeta(n = 10, 0.2, 0.1)
#'   ),
#'   by = "id_measurement"
#' )
#'
#' @export
left_join.ir <- function(
  x,
  y,
  by = NULL,
  copy = FALSE,
  suffix = c(".x", ".y"),
  ...,
  keep = FALSE,
  na_matches = c("na", "never")
) {
  ir_reclass_ir(NextMethod())
}


#' @rdname mutate-joins
#'
#' @examples
#' ## right_join
#' set.seed(234)
#' dplyr::right_join(
#'   ir_sample_data,
#'   tibble::tibble(
#'     id_measurement = c(1:5, 101:105),
#'     nitrogen_content = rbeta(n = 10, 0.2, 0.1)
#'   ),
#'   by = "id_measurement"
#' )
#'
#' @export
right_join.ir <- function(
  x,
  y,
  by = NULL,
  copy = FALSE,
  suffix = c(".x", ".y"),
  ...,
  keep = FALSE,
  na_matches = c("na", "never")
) {
  ir_reclass_ir(NextMethod())
}


#' @rdname mutate-joins
#'
#' @examples
#' ## full_join
#' set.seed(234)
#' dplyr::full_join(
#'   ir_sample_data,
#'   tibble::tibble(
#'     id_measurement = c(1:5, 101:105),
#'     nitrogen_content = rbeta(n = 10, 0.2, 0.1)
#'   ),
#'   by = "id_measurement"
#' )
#'
#' @export
full_join.ir <- function(
  x,
  y,
  by = NULL,
  copy = FALSE,
  suffix = c(".x", ".y"),
  ...,
  keep = FALSE,
  na_matches = c("na", "never")
) {
  ir_reclass_ir(NextMethod())
}


#' Filtering joins for an `ir` object
#'
#' @name filter-joins
#'
#' @param x An object of class `ir`.
#'
#' @inheritParams dplyr::semi_join
#'
#' @source [dplyr::filter-joins()]
NULL


#' @rdname filter-joins
#'
#' @examples
#' ## semi_join
#' set.seed(234)
#' dplyr::semi_join(
#'   ir_sample_data,
#'   tibble::tibble(
#'     id_measurement = c(1:5, 101:105),
#'     nitrogen_content = rbeta(n = 10, 0.2, 0.1)
#'   ),
#'   by = "id_measurement"
#' )
#'
#' @export
semi_join.ir <- function(x, y, by = NULL, copy = FALSE, ..., na_matches = c("na", "never")) {
  ir_reclass_ir(NextMethod())
}


#' @rdname filter-joins
#'
#' @examples
#' ## anti_join
#' set.seed(234)
#' dplyr::anti_join(
#'   ir_sample_data,
#'   tibble::tibble(
#'     id_measurement = c(1:5, 101:105),
#'     nitrogen_content = rbeta(n = 10, 0.2, 0.1)
#'   ),
#'   by = "id_measurement"
#' )
#'
#' @export
anti_join.ir <- function(x, y, by = NULL, copy = FALSE, ..., na_matches = c("na", "never")) {
  ir_reclass_ir(NextMethod())
}

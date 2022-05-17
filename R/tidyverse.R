#### Tidyverse methods for ir objects ####

#' Subset rows in `ir` objects using column values
#'
#' @family tidyverse
#'
#' @inheritParams dplyr::filter
#'
#' @param .data An object of class `ir`.
#'
#' @source [dplyr::filter()]
#'
#' @return `.data` with filtered rows.
#'
#' @examples
#' ## filter
#' dplyr::filter(ir_sample_data, sample_type == "office paper")
#'
#'
filter.ir <- function(.data, ..., .preserve = FALSE) {
  class(.data) <- setdiff(class(.data), "ir")
  ir_reclass_ir(NextMethod())
}


#' Arrange rows in `ir` objects by column values
#'
#' @family tidyverse
#'
#' @inheritParams dplyr::arrange
#'
#' @param .data An object of class `ir`.
#'
#' @return `.data` with arranged rows.
#'
#' @source [dplyr::arrange()]
#'
#' @examples
#' ## arrange
#' dplyr::arrange(ir_sample_data, dplyr::desc(sample_type))
#'
#'
arrange.ir <- function(.data, ..., .by_group = FALSE) {
  ir_reclass_ir(NextMethod())
}


#' Group rows in `ir` objects by one or more variables
#'
#' @family tidyverse
#'
#' @name group_by
#'
#' @inheritParams dplyr::group_by
#'
#' @param .data An object of class `ir`.
#'
#' @source [dplyr::group_by()]
#'
#' @return `.data` with grouped rows (`group_by.ir()`) or ungrouped rows
#' (`ungroup.ir()`).
#'
#' @examples
#' ## group_by
#' dplyr::group_by(ir_sample_data, sample_type)
#'
#'
group_by.ir <- function(.data, ..., .add = FALSE, .drop = dplyr::group_by_drop_default(.data)) {
  ir_reclass_ir(NextMethod())
}


#' @rdname group_by
#'
#' @examples
#' ## ungroup
#' dplyr::ungroup(dplyr::group_by(ir_sample_data, sample_type))
#'
#'
ungroup.ir <- function(.data, ...) {
  ir_reclass_ir(NextMethod())
}


#' Group input `ir` objects by rows
#'
#' @family tidyverse
#'
#' @inheritParams dplyr::rowwise
#'
#' @param data An object of class `ir`.
#'
#' @return `data` as row-wise data frame. See [dplyr::rowwise()].
#'
#' @source [dplyr::rowwise()]
#'
#' @examples
#' ## rowwise
#' dplyr::rowwise(ir_sample_data) %>%
#'   dplyr::mutate(
#'     hkl =
#'       mean(
#'         units::drop_units(klason_lignin),
#'         units::drop_units(holocellulose)
#'       )
#'   )
#'
#'
rowwise.ir <- function(.data, ...) {
  ir_reclass_ir(NextMethod())
}


#' Mutate an `ir` object by adding new or replacing existing columns
#'
#' @family tidyverse
#'
#' @name mutate
#'
#' @inheritParams dplyr::mutate
#'
#' @param .data An object of class `ir`.
#'
#' @return `.data` with modified columns. If the `spectra` column is dropped or
#' invalidated (see [ir_new_ir()]), the `ir` class is dropped, else the object
#' is of class `ir`.
#'
#' @source [dplyr::mutate()]
#'
#' @examples
#' ## mutate
#' dplyr::mutate(ir_sample_data, hkl = klason_lignin + holocellulose)
#'
#'
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
#'
transmute.ir <- function(.data, ...) {
  ir_reclass_ir(NextMethod())
}


#' Subset columns in `ir` objects using column names and types
#'
#' @family tidyverse
#'
#' @inheritParams dplyr::select
#'
#' @param .data An object of class `ir`.
#'
#' @return `.data` with the selected columns. If the `spectra` column is dropped,
#' the `ir` class is dropped, else the object is of class `ir`.
#'
#' @source [dplyr::select()]
#'
#' @examples
#' ## select
#' dplyr::select(ir_sample_data, spectra)
#' dplyr::select(ir_sample_data, holocellulose) # drops ir class
#'
#'
select.ir <- function(.data, ...) {
  ir_reclass_ir(NextMethod())
}


#' Rename columns in `ir` objects
#'
#' @family tidyverse
#'
#' @inheritParams dplyr::rename
#'
#' @param .data An object of class `ir`.
#'
#' @source [dplyr::rename()]
#'
#' @return `.data` with renamed columns. If the `spectra` column is renamed,
#' and no new valid `spectra` column is created, the `ir` class is dropped, else
#' the object is of class `ir`.
#'
#' @name rename
#'
#' @examples
#' ## rename
#' dplyr::rename(ir_sample_data, hol = "holocellulose")
#' dplyr::rename(ir_sample_data, spec = "spectra") # drops ir class
#'
#'
rename.ir <- function(.data, ...) {
  ir_reclass_ir(NextMethod())
}

#' @rdname rename
#'
#' @examples
#' ## rename_with
#' dplyr::rename_with(ir_sample_data, .cols = dplyr::starts_with("id_"),
#'   toupper)
#' dplyr::rename_with(ir_sample_data, toupper) # drops ir class
#'
#'
rename_with.ir <- function(.data, .fn, .cols = dplyr::everything(), ...) {
  ir_reclass_ir(NextMethod())
}


#' Subset rows in `ir` objects using their positions
#'
#' @family tidyverse
#'
#' @name slice
#'
#' @inheritParams dplyr::slice
#'
#' @param .data An object of class `ir`.
#'
#' @return `.data` with subsetted rows.
#'
#' @source [dplyr::slice()]
#'
#' @examples
#' ## slice
#' dplyr::slice(ir_sample_data, 1:5)
#' dplyr::slice_min(ir_sample_data, holocellulose, n = 3)
#' dplyr::slice_max(ir_sample_data, holocellulose, n = 3)
#' dplyr::slice_head(ir_sample_data, n = 5)
#' dplyr::slice_tail(ir_sample_data, n = 5)
#'
slice.ir <- function(.data, ..., .preserve = FALSE) {
  ir_reclass_ir(NextMethod())
}


#' @rdname slice
#'
#' @examples
#' ## slice_sample
#' set.seed(234)
#' dplyr::slice_sample(ir_sample_data, n = 3)
#'
#'
slice_sample.ir <- function(.data, ..., n, prop, weight_by = NULL, replace = FALSE) {
  ir_reclass_ir(NextMethod())
}


#' Summarize each group in a `ir` object to fewer rows
#'
#' @family tidyverse
#'
#' @name summarize
#' @aliases summarise
#'
#' @inheritParams dplyr::summarize
#'
#' @param .data An object of class `ir`.
#'
#' @return `.data` with summarized columns. If the `spectra` column is dropped
#' or invalidated (see [ir_new_ir()]), the `ir` class is dropped, else the
#' object is of class `ir`.
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
#'
summarize.ir <- function(.data, ..., .groups = NULL) {
  ir_reclass_ir(NextMethod())
}

#' @rdname summarize
#'
#'
summarise.ir <- summarize.ir


#' Subset distinct/unique rows in `ir` objects
#'
#' @family tidyverse
#'
#' @inheritParams dplyr::distinct
#'
#' @param .data An object of class `ir`.
#'
#' @return `.data` with distinct rows.
#'
#' @source [dplyr::distinct()]
#'
#' @examples
#' ## distinct
#' dplyr::distinct(rep(ir_sample_data, 2))
#'
#'
distinct.ir <- function(.data, ..., .keep_all = FALSE) {
  ir_reclass_ir(NextMethod())
}


#' Pivot an `ir` object from wide to long
#'
#' @family tidyverse
#'
#' @inheritParams tidyr::pivot_longer
#'
#' @param data An object of class `ir`.
#'
#' @return `data` in a long format. If the `spectra` column is dropped
#' or invalidated (see [ir_new_ir()]), the `ir` class is dropped, else the
#' object is of class `ir`.
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
#'
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

#' Pivot an `ir` object from wide to long
#'
#' @family tidyverse
#'
#' @inheritParams tidyr::pivot_wider
#'
#' @param data An object of class `ir`.
#'
#' @return `data` in a wide format. If the `spectra` column is dropped
#' or invalidated (see [ir_new_ir()]), the `ir` class is dropped, else the
#' object is of class `ir`.
#'
#' @source [tidyr::pivot_wider()]
#'
#' @examples
#' ## pivot_wider
#' ir_sample_data %>%
#'   tidyr::pivot_longer(
#'     cols = dplyr::any_of(c("holocellulose", "klason_lignin"))
#'   ) %>%
#'   tidyr::pivot_wider(names_from = "name", values_from = "value")
#'
#'
pivot_wider.ir <- function(
  data,
  id_cols = NULL,
  names_from = "name",
  names_prefix = "",
  names_sep = "_",
  names_glue = NULL,
  names_sort = FALSE,
  names_repair = "check_unique",
  values_from = "value",
  values_fill = NULL,
  values_fn = NULL,
  ...)
{
  ir_reclass_ir(NextMethod())
}


#' Nest and un-nest an `ir` object
#'
#' @family tidyverse
#'
#' @name nest
#'
#' @inheritParams tidyr::nest
#'
#' @param .data An object of class `ir`.
#'
#' @param names_sep,.names_sep If `NULL`, the default, the names will be left
#'   as is. In `nest()`, inner names will come from the former outer names;
#'   in `unnest()`, the new outer names will come from the inner names.
#'
#'   If a string, the inner and outer names will be used together. In
#'   `unnest()`, the names of the new outer columns will be formed by pasting
#'   together the outer and the inner column names, separated by `names_sep`. In
#'   `nest()`, the new inner names will have the outer names + `names_sep`
#'   automatically stripped. This makes `names_sep` roughly symmetric between
#'   nesting and unnesting.
#'
#' @return `.data` with nested or unnested columns. If the `spectra` column is
#' dropped or invalidated (see [ir_new_ir()]), the `ir` class is dropped, else
#' the object is of class `ir`.
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
#'
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
#'
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
  ir_reclass_ir(NextMethod())
}


#' Separate a character column in an `ir` object into multiple columns with a regular expression or numeric locations
#'
#' @family tidyverse
#'
#' @inheritParams tidyr::separate
#'
#' @param data An object of class `ir`.
#'
#' @return `.data` with separated columns. If the `spectra` column is
#' dropped or invalidated (see [ir_new_ir()]), the `ir` class is dropped, else
#' the object is of class `ir`.
#'
#' @source [tidyr::separate()]
#'
#' @examples
#' ## separate
#' ir_sample_data %>%
#'   tidyr::separate(
#'     col = "id_sample",  c("a", "b", "c")
#'   )
#'
#'
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
#' @family tidyverse
#'
#' @inheritParams tidyr::unite
#'
#' @param data An object of class `ir`.
#'
#' @return `.data` with united columns. If the `spectra` column is
#' dropped or invalidated (see [ir_new_ir()]), the `ir` class is dropped, else
#' the object is of class `ir`.
#'
#' @source [tidyr::unite()]
#'
#' @examples
#' ## unite
#' ir_sample_data %>%
#'   tidyr::separate(
#'     "id_sample",  c("a", "b", "c")
#'   ) %>%
#'   tidyr::unite(id_sample, a, b, c)
#'
#'
unite.ir <- function(data, col, ..., sep = "_", remove = TRUE, na.rm = FALSE) {
  ir_reclass_ir(NextMethod())
}


#' Extract a character column in an `ir` object into multiple columns using regular expression groups
#'
#' @family tidyverse
#'
#' @inheritParams tidyr::extract
#'
#' @param data An object of class `ir`.
#'
#' @return `data` with an extracted character column. See
#' [tidyr::extract()].
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
#'
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
#' @family tidyverse
#'
#' @inheritParams tidyr::separate_rows
#'
#' @param data An object of class `ir`.
#'
#' @return `data` with a collapsed column separated into multiple rows. See
#' [tidyr::separate_rows()].
#'
#' @source [tidyr::separate_rows()]
#'
#' @examples
#' ## separate_rows
#' ir_sample_data %>%
#'   tidyr::unite(
#'     col = content, holocellulose, klason_lignin
#'   ) %>%
#'   tidyr::separate_rows(
#'     col
#'   )
#'
#'
separate_rows.ir <- function(data, ..., sep = "[^[:alnum:].]+", convert = FALSE)
{
  ir_reclass_ir(NextMethod())
}


#### joins ####

#' Mutating joins for an `ir` object
#'
#' @family tidyverse
#'
#' @name mutate-joins
#'
#' @inheritParams dplyr::left_join
#'
#' @param x An object of class `ir`.
#'
#' @param y A data frame.
#'
#' @return `x` and `y` joined. If the `spectra` column is renamed, the `ir`
#' class is dropped. See [`mutate-joins`][dplyr::left_join].
#'
#' @source [`mutate-joins`][dplyr::left_join]
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
#'
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
#'
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
#'
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
#'
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
#' @family tidyverse
#'
#' @name filter-joins
#'
#' @inheritParams dplyr::semi_join
#'
#' @param x An object of class `ir`.
#'
#' @param y A data frame.
#'
#' @return `x` and `y` joined. If the `spectra` column is renamed, the `ir`
#' class is dropped. See [`filter-joins`][dplyr::semi_join].
#'
#' @source [`filter-joins`][dplyr::semi_join]
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
#'
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
#'
anti_join.ir <- function(x, y, by = NULL, copy = FALSE, ..., na_matches = c("na", "never")) {
  ir_reclass_ir(NextMethod())
}

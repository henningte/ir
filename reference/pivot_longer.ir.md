# Pivot an `ir` object from wide to long

Pivot an `ir` object from wide to long

## Usage

``` r
pivot_longer.ir(
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
  ...
)
```

## Source

[`tidyr::pivot_longer()`](https://tidyr.tidyverse.org/reference/pivot_longer.html)

## Arguments

- data:

  An object of class `ir`.

- cols:

  \<[`tidy-select`](https://tidyr.tidyverse.org/reference/tidyr_tidy_select.html)\>
  Columns to pivot into longer format.

- names_to:

  A character vector specifying the new column or columns to create from
  the information stored in the column names of `data` specified by
  `cols`.

  - If length 0, or if `NULL` is supplied, no columns will be created.

  - If length 1, a single column will be created which will contain the
    column names specified by `cols`.

  - If length \>1, multiple columns will be created. In this case, one
    of `names_sep` or `names_pattern` must be supplied to specify how
    the column names should be split. There are also two additional
    character values you can take advantage of:

    - `NA` will discard the corresponding component of the column name.

    - `".value"` indicates that the corresponding component of the
      column name defines the name of the output column containing the
      cell values, overriding `values_to` entirely.

- names_prefix:

  A regular expression used to remove matching text from the start of
  each variable name.

- names_sep, names_pattern:

  If `names_to` contains multiple values, these arguments control how
  the column name is broken up.

  `names_sep` takes the same specification as
  [`separate()`](https://tidyr.tidyverse.org/reference/separate.html),
  and can either be a numeric vector (specifying positions to break on),
  or a single string (specifying a regular expression to split on).

  `names_pattern` takes the same specification as
  [`extract()`](https://tidyr.tidyverse.org/reference/extract.html), a
  regular expression containing matching groups (`()`).

  If these arguments do not give you enough control, use
  `pivot_longer_spec()` to create a spec object and process manually as
  needed.

- names_ptypes, values_ptypes:

  Optionally, a list of column name-prototype pairs. Alternatively, a
  single empty prototype can be supplied, which will be applied to all
  columns. A prototype (or ptype for short) is a zero-length vector
  (like [`integer()`](https://rdrr.io/r/base/integer.html) or
  [`numeric()`](https://rdrr.io/r/base/numeric.html)) that defines the
  type, class, and attributes of a vector. Use these arguments if you
  want to confirm that the created columns are the types that you
  expect. Note that if you want to change (instead of confirm) the types
  of specific columns, you should use `names_transform` or
  `values_transform` instead.

- names_transform, values_transform:

  Optionally, a list of column name-function pairs. Alternatively, a
  single function can be supplied, which will be applied to all columns.
  Use these arguments if you need to change the types of specific
  columns. For example, `names_transform = list(week = as.integer)`
  would convert a character variable called `week` to an integer.

  If not specified, the type of the columns generated from `names_to`
  will be character, and the type of the variables generated from
  `values_to` will be the common type of the input columns used to
  generate them.

- names_repair:

  What happens if the output has invalid column names? The default,
  `"check_unique"` is to error if the columns are duplicated. Use
  `"minimal"` to allow duplicates in the output, or `"unique"` to
  de-duplicated by adding numeric suffixes. See
  [`vctrs::vec_as_names()`](https://vctrs.r-lib.org/reference/vec_as_names.html)
  for more options.

- values_to:

  A string specifying the name of the column to create from the data
  stored in cell values. If `names_to` is a character containing the
  special `.value` sentinel, this value will be ignored, and the name of
  the value column will be derived from part of the existing column
  names.

- values_drop_na:

  If `TRUE`, will drop rows that contain only `NA`s in the `value_to`
  column. This effectively converts explicit missing values to implicit
  missing values, and should generally be used only when missing values
  in `data` were created by its structure.

- ...:

  Additional arguments passed on to methods.

## Value

`data` in a long format. If the `spectra` column is dropped or
invalidated (see
[`ir_new_ir()`](https://henningte.github.io/ir/reference/ir_new_ir.md)),
the `ir` class is dropped, else the object is of class `ir`.

## See also

Other tidyverse:
[`arrange.ir()`](https://henningte.github.io/ir/reference/arrange.ir.md),
[`distinct.ir()`](https://henningte.github.io/ir/reference/distinct.ir.md),
[`extract.ir()`](https://henningte.github.io/ir/reference/extract.ir.md),
[`filter-joins`](https://henningte.github.io/ir/reference/filter-joins.md),
[`filter.ir()`](https://henningte.github.io/ir/reference/filter.ir.md),
[`group_by`](https://henningte.github.io/ir/reference/group_by.md),
[`mutate`](https://henningte.github.io/ir/reference/mutate.md),
[`mutate-joins`](https://henningte.github.io/ir/reference/mutate-joins.md),
[`nest`](https://henningte.github.io/ir/reference/nest.md),
[`pivot_wider.ir()`](https://henningte.github.io/ir/reference/pivot_wider.ir.md),
[`rename`](https://henningte.github.io/ir/reference/rename.md),
[`rowwise.ir()`](https://henningte.github.io/ir/reference/rowwise.ir.md),
[`select.ir()`](https://henningte.github.io/ir/reference/select.ir.md),
[`separate.ir()`](https://henningte.github.io/ir/reference/separate.ir.md),
[`separate_rows.ir()`](https://henningte.github.io/ir/reference/separate_rows.ir.md),
[`slice`](https://henningte.github.io/ir/reference/slice.md),
[`summarize`](https://henningte.github.io/ir/reference/summarize.md),
[`unite.ir()`](https://henningte.github.io/ir/reference/unite.ir.md)

## Examples

``` r
## pivot_longer
ir_sample_data |>
  tidyr::pivot_longer(
    cols = dplyr::any_of(c("holocellulose", "klason_lignin"))
  )
#> # A tibble: 116 × 7
#>    id_measurement id_sample sample_type sample_comment      spectra  name  value
#>  *          <int> <chr>     <chr>       <chr>               <named > <chr>   [1]
#>  1              1 GN 11-389 needles     Abies Firma Momi f… <tibble> holo… 0.308
#>  2              1 GN 11-389 needles     Abies Firma Momi f… <tibble> klas… 0.360
#>  3              2 GN 11-400 needles     Cupressocyparis le… <tibble> holo… 0.250
#>  4              2 GN 11-400 needles     Cupressocyparis le… <tibble> klas… 0.339
#>  5              3 GN 11-407 needles     Juniperus chinensi… <tibble> holo… 0.336
#>  6              3 GN 11-407 needles     Juniperus chinensi… <tibble> klas… 0.268
#>  7              4 GN 11-411 needles     Metasequoia glypto… <tibble> holo… 0.184
#>  8              4 GN 11-411 needles     Metasequoia glypto… <tibble> klas… 0.350
#>  9              5 GN 11-416 needles     Pinus strobus Toru… <tibble> holo… 0.309
#> 10              5 GN 11-416 needles     Pinus strobus Toru… <tibble> klas… 0.331
#> # ℹ 106 more rows

```

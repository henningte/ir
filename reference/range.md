# Get the minima/maxima/range/median of x axis values or intensity values of infrared spectra

`range.ir` extracts the range of x axis values (e.g. wavenumbers) or
intensity values of infrared spectra.

`min.ir` extracts the minimum x axis value (e.g. wavenumber) or
intensity value of infrared spectra.

`max.ir` extracts the maximum x axis value (e.g. wavenumber) or
intensity value of infrared spectra.

`median.ir` extracts the median x axis value (e.g. wavenumber) or
intensity value of infrared spectra.

## Usage

``` r
# S3 method for class 'ir'
range(
  x,
  ...,
  na.rm = FALSE,
  .dimension = "y",
  .col_names = c("y_min", "y_max")
)

# S3 method for class 'ir'
min(x, ..., na.rm = FALSE, .dimension = "y", .col_name = "y_min")

# S3 method for class 'ir'
max(x, ..., na.rm = FALSE, .dimension = "y", .col_name = "y_max")

# S3 method for class 'ir'
median(x, na.rm = FALSE, ..., .dimension = "y", .col_name = "y_median")
```

## Arguments

- x:

  An object of class
  [`ir`](https://henningte.github.io/ir/reference/ir_new_ir.md).

- ...:

  Further arguments, ignored.

- na.rm:

  A logical value. See [`max()`](https://rdrr.io/r/base/Extremes.html).

- .dimension:

  A character value. Must be one of the following:

  "x"

  :   In this case, the minimum/maximum/range/median of x axis values of
      the spectra in `x` are extracted.

  "y"

  :   In this case, the minimum/maximum/range/median of intensity values
      of the spectra in `x` are extracted.

- .col_names:

  A character vector of length 2 representing the names of the columns
  in which to store the extracted values. The first element is the name
  for the column with minima values, the second the name for the column
  with maxima values.

- .col_name:

  A character value representing the name of the column in which to
  store the extracted values.

## Value

`x` with the extracted values.

## Examples

``` r
# range of intensity values
x1 <-
   ir::ir_sample_data |>
   range(.dimension = "y")

# minimum intensity values
x1 <-
   ir::ir_sample_data |>
   min(.dimension = "y")

# maximum intensity values
x1 <-
   ir::ir_sample_data |>
   max(.dimension = "y")

# median intensity values
x1 <-
   ir::ir_sample_data |>
   stats::median(.dimension = "y")
```

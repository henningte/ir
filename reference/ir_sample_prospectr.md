# Wrapper to sampling functions from the 'prospectr' package

Wrapper functions that allows to directly use 'ir' objects with sampling
functions from the 'prospectr' package.

## Usage

``` r
ir_sample_prospectr(x, sampling_function, ..., return_prospectr_output = FALSE)
```

## Arguments

- x:

  An object of class 'ir' containing the spectra based on which to
  sample measurements.

- sampling_function:

  A function from the 'prospectr' package to perform sampling based on
  spectra (`naes()`, `kenStone()`, `duplex()`, `puchwein()`,
  `shenkWest()`, `honig()`). See the 'prospectr' package for details.

- ...:

  Arguments passed to `sampling_function`. See the 'prospectr' package
  for details.

- return_prospectr_output:

  Logical value. If `TRUE`, the output of `sampling_function` is
  returned. If `FALSE`, values of elements `model` and `test` are
  included as columns in `x` and `x` is returned.

## Value

If `return_prospectr_output = TRUE`, the output of `sampling_function`.
See the 'prospectr' package for details. If
`return_prospectr_output = FALSE`,`x` with the following additional
columns:

- for_prospectr_model:

  Logical value indicating whether the spectrum is listed in element
  `model` of the prospectr output (`TRUE`) or not (`FALSE`).

- for_prospectr_test:

  Logical value indicating whether the spectrum is listed in element
  `test` of the prospectr output (`TRUE`) or not (`FALSE`).

- prospectr_model:

  Integer representing the order in which spectra are listed in element
  `model` of the prospectr output.

- prospectr_test:

  Integer representing the order in which spectra are listed in element
  `test` of the prospectr output.

## Examples

``` r
if(requireNamespace("prospectr", quietly = TRUE)) {
  x <-
    ir_sample_prospectr(
      ir::ir_sample_data,
      prospectr::kenStone,
      metric = "euclid",
      k = 30,
      return_prospectr_output = FALSE
  )

  x <-
    ir_sample_prospectr(
      ir::ir_sample_data,
      prospectr::kenStone,
      metric = "euclid",
      k = 30,
      return_prospectr_output = TRUE
    )
}
```

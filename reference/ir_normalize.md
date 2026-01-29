# Normalizes infrared spectra in an `ir` object

`ir_normalize` normalizes the intensity values of infrared spectra.
Different methods for normalization are available.

## Usage

``` r
ir_normalize(x, method = "area")

ir_normalise(x, method = "area")
```

## Arguments

- x:

  An object of class
  [`ir`](https://henningte.github.io/ir/reference/ir_new_ir.md).

- method:

  A character value specifying which normalization method to apply:

  `"zeroone"`

  :   All intensity values will be normalized to \[0;1\].

  `"area"`

  :   All intensity values will be divided by the sum of the intensity
      values at all wavenumber values of the spectrum.

  `"area_absolute"`

  :   All intensity values will be divided by the sum of the intensity
      values at all wavenumber values of the spectrum.

  `"vector"`

  :   All intensity values will be divided by the norm of the intensity
      vector (vector normalization).

  "snv"

  :   Standard Normal Variate correction: For each spectrum, the average
      intensity value is subtracted and then divided by the standard
      deviation.

  A numeric value

  :   If `method` is convertible to a numeric value, e.g.
      `method = "980"`, the intensity of all spectra at a wavenumber
      value of 980 will be set to 1 and the minimum intensity value of
      each spectrum will be set to 0, i.e. the spectra will be
      normalized referring to a specific wavenumber value.

## Value

An object of class `ir` representing a normalized version of `x`.

## Examples

``` r
# with method = "area"
x1 <-
   ir::ir_sample_data |>
   ir::ir_normalize(method = "area")

# second derivative spectrum with method = "area" or method = "area_absolute"
x2 <-
   ir::ir_sample_data |>
   ir::ir_smooth(method = "sg", n = 31, m = 2) |>
   ir::ir_normalize(method = "area")

x3 <-
   ir::ir_sample_data |>
   ir::ir_smooth(method = "sg", n = 31, m = 2) |>
   ir::ir_normalize(method = "area_absolute")

# with method = "zeroone"
x4 <-
   ir::ir_sample_data |>
   ir::ir_normalize(method = "zeroone")

# with method = "vector"
x5 <-
   ir::ir_sample_data |>
   ir::ir_normalize(method = "vector")

# with method = "snv"
x6 <-
   ir::ir_sample_data |>
   ir::ir_normalize(method = "snv")

# normalizing to a specific peak
x7 <-
   ir::ir_sample_data |>
   ir::ir_normalize(method = 1090)
```

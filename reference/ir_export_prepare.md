# Prepares `ir` objects for export to `csv`

Prepares `ir` objects for export to `csv` files. To export `ir` objects
to `csv`, the `spectra` column has to be converted to an own data frame
and be exported separately from the metadata. When preparing metadata
for export, `ir_export_prepare` takes care of separating measurement
units and measurement errors in columns of class
[units::units](https://r-quantities.github.io/units/reference/units.html),
[errors::errors](https://r-quantities.github.io/errors/reference/errors.html),
and
[quantities::quantities](https://r-quantities.github.io/quantities/reference/quantities.html)
(see the examples).

## Usage

``` r
ir_export_prepare(
  x,
  what = "metadata",
  measurement_id = as.character(seq_len(nrow(x)))
)
```

## Arguments

- x:

  An object of class
  [`ir`](https://henningte.github.io/ir/reference/ir_new_ir.md).

- what:

  A character value defining what to prepare for export. If
  `"metadata"`, the metadata will be prepared for export and column
  `spectra` will be dropped. If `"spectra"`, `x` is converted to an
  object of class
  [`ir_flat`](https://henningte.github.io/ir/reference/ir_new_ir_flat.md).

- measurement_id:

  A character vector an element for each row in `x` that contains the
  names to use as column names for the spectra in the `ir_flat` object
  to create.

## Value

A data frame.

## Note

This function superseded `irp_export_prepare()` from the
['irpeat'](https://github.com/henningte/irpeat/) package.

## Examples

``` r
x_spectra <- ir_export_prepare(ir_sample_data[1:5, ], what = "spectra")
x_metadata <- ir_export_prepare(ir_sample_data[1:5, ], what = "metadata")
```

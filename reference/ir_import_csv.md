# Imports infrared spectra from various files

`ir_import_csv` imports raw infrared spectra from one or more `.csv`
file that contains at least one spectrum, with x axis values (e.g.
wavenumbers) in the first column and intensity values of spectra in
remaining columns. Note that the function does not perform any checks
for the validity of the content read from the .csv file.

## Usage

``` r
ir_import_csv(filenames, sample_id = "from_filenames", ...)
```

## Arguments

- filenames:

  A character vector representing the complete paths to the `.csv` files
  to import.

- sample_id:

  Either:

  - `NULL`: Nothing additional happens.

  - A character vector with the same length as `filenames`: This vector
    will be added as column `sample_id` to the `ir` object.

  - `"from_filenames"`: The file name(s) will be used as values for a
    new column `sample_id` to add (the default).

  - `"from_colnames"`: The header in the csv file will be used as values
    for a new column `sample_id` to add.

- ...:

  Further arguments passed to
  [`read.csv()`](https://rdrr.io/r/utils/read.table.html).

## Value

An object of class
[`ir`](https://henningte.github.io/ir/reference/ir_new_ir.md) containing
the infrared spectra extracted from the `.csv` file(s).

## Examples

``` r
# import data from csv files
d <-
  ir::ir_import_csv(
    system.file(package = "ir", "extdata/klh_hodgkins_mir.csv"),
    sample_id = "from_colnames"
  )
```

# Imports infrared spectra from Thermo Galactic's files

`ir_import_spc` imports raw infrared spectra from a Thermo Galactic's
`.spc` file or several of such files. `ir_import_spc` is a wrapper
function to
[`hyperSpec::read.spc()`](https://r-hyperspec.github.io/hyperSpec/reference/read-spc.html).

## Usage

``` r
ir_import_spc(filenames, log.txt = TRUE)
```

## Arguments

- filenames:

  A character vector representing the complete paths to the `.spc` files
  to import.

- log.txt:

  A logical value indicating whether to import metadata (`TRUE`) or not
  (`FALSE`). See the details section. If set to `FALSE`, only the
  metadata variables `exponentiation_factor` to `measurement_device`
  listed in the Value section below are included in the `ir` object.

## Value

An object of class
[`ir`](https://henningte.github.io/ir/reference/ir_new_ir.md) containing
the infrared spectra extracted from the `.spc` file(s) and the metadata
as extracted by
[`hyperSpec::read.spc()`](https://r-hyperspec.github.io/hyperSpec/reference/read-spc.html).
Metadata variables are:

- scan_number:

  An integer value representing the number of scans.

- detection_gain_factor:

  The detection gain factor.

- scan_speed:

  The scan speed \[kHz\].

- laser_wavenumber:

  The wavenumber of the laser.

- detector_name:

  The name of the detector.

- source_name:

  The name of the infrared radiation source.

- purge_delay:

  The duration of purge delay before a measurement \[s\].

- zero_filling_factor:

  A numeric value representing the zero filling factor.

- apodisation_function:

  The name of the apodisation function.

- exponentiation_factor:

  The exponentiation factor used for file compression.

- data_point_number:

  The number of data points in the spectrum

- x_variable_type:

  The type of the x variable.

- y_variable_type:

  The type of the y variable.

- measurement_date:

  A POSIXct representing the measurement date and time.

- measurement_device:

  The name of the measurement device.

## Details

Currently, `log.txt` must be set to `FALSE` due to a bug in
[`hyperSpec::read.spc()`](https://r-hyperspec.github.io/hyperSpec/reference/read-spc.html).
This bug fill be fixed in the upcoming weeks and currently can be
circumvented by using the development version of 'hyperSpec'. See
https://github.com/r-hyperspec/hyperSpec/issues/80.

## Examples

``` r
# import a sample .spc file
x <-
  ir::ir_import_spc(
    system.file("extdata/1.spc", package = "ir"),
    log.txt = FALSE
  )
```

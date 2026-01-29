# Converts absorbance spectra to transmittance spectra or vice versa

`ir_to_transmittance` converts absorbance spectra to transmittance
spectra. `ir_to_absorbance` converts transmittance spectra to absorbance
spectra. Note that neither function checks whether the input spectra are
absorbance or transmittance spectra.

## Usage

``` r
ir_to_transmittance(x)

ir_to_absorbance(x)
```

## Source

(Stuart 2004) .

## Arguments

- x:

  An object of class
  [`ir`](https://henningte.github.io/ir/reference/ir_new_ir.md).

## Value

`x` with y values fore each spectrum as transmittance values (in case of
`ir_to_transmittance`) or absorbance values (in case of
`ir_to_absorbance`).

## References

Stuart BH (2004). *Infrared Spectroscopy: Fundamentals and
Applications*, Analytical Techniques in the Sciences. John Wiley and
Sons, Ltd, Chichester, UK. ISBN 978-0-470-01114-0 978-0-470-85428-0,
[doi:10.1002/0470011149](https://doi.org/10.1002/0470011149) .

## Examples

``` r
# convert from absorbance to transmittance
x1 <-
    ir_sample_data |>
    ir_to_transmittance()

# convert from transmittance to absorbance
x2 <-
    x1 |>
    ir::ir_to_absorbance()

vapply(
  seq_along(x2$spectra),
  FUN = function(i) all.equal(x2$spectra[[i]], ir::ir_sample_data$spectra[[i]]),
  FUN.VALUE = logical(1L)
) |>
  all()
#> [1] TRUE
```

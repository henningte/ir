# Identifies empty spectra in an `ir` object

`ir_identify_empty_spectra()` identifies empty spectra in an object of
class [`ir`](https://henningte.github.io/ir/reference/ir_new_ir.md). An
empty spectrum is a spectrum which has no data values (no rows) or where
all intensity values (column `y`) are `NA`.

## Usage

``` r
ir_identify_empty_spectra(x)
```

## Arguments

- x:

  An object of class `ir`.

## Value

A logical vector indicating for each spectrum in `x` whether it is empty
(`TRUE`) or not (`FALSE`).

## Examples

``` r
ir_identify_empty_spectra(ir::ir_sample_data)
#>  GN.11.389  GN.11.400  GN.11.407  GN.11.411  GN.11.416  GN.11.419  GN.11.422 
#>      FALSE      FALSE      FALSE      FALSE      FALSE      FALSE      FALSE 
#>  GN.11.423  GN.11.428  GN.11.434  GN.11.435  GN.11.460  HW.07.151  HW.11.137 
#>      FALSE      FALSE      FALSE      FALSE      FALSE      FALSE      FALSE 
#>  HW.11.144  HW.11.146  HW.11.148  HW.11.149  HW.11.172  HW.11.173  HW.11.176 
#>      FALSE      FALSE      FALSE      FALSE      FALSE      FALSE      FALSE 
#>  HW.11.179  HW.11.184  LG.09.384  LG.11.309  LG.11.393  LG.11.395  LG.11.397 
#>      FALSE      FALSE      FALSE      FALSE      FALSE      FALSE      FALSE 
#>  LG.11.404  LG.11.412  LG.11.414  LG.11.418  LG.11.432  LG.11.433  LG.11.438 
#>      FALSE      FALSE      FALSE      FALSE      FALSE      FALSE      FALSE 
#>  LG.11.461  SW.11.138  SW.11.139  SW.11.154  SW.11.158  SW.11.178 OCC.11.457 
#>      FALSE      FALSE      FALSE      FALSE      FALSE      FALSE      FALSE 
#> OCC.11.462 OCC.11.463  OMG.08.82 OMG.11.454 OMG.11.455 OMG.11.456  ONP.08.78 
#>      FALSE      FALSE      FALSE      FALSE      FALSE      FALSE      FALSE 
#> ONP.09.388 ONP.11.450 ONP.11.451 ONP.11.458 ONP.11.459 OFF.10.506 OFF.13.144 
#>      FALSE      FALSE      FALSE      FALSE      FALSE      FALSE      FALSE 
#>  OFF.08.80 OFF.08.852 
#>      FALSE      FALSE 
```

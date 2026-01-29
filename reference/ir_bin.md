# Bins infrared spectra

`ir_bin` bins intensity values of infrared spectra into bins of a
defined width or into a defined number of bins.

## Usage

``` r
ir_bin(x, width = 10, new_x_type = "start", return_ir_flat = FALSE)
```

## Arguments

- x:

  An object of class
  [`ir`](https://henningte.github.io/ir/reference/ir_new_ir.md) with
  integer wavenumber values increasing by 1.

- width:

  An integer value indicating the wavenumber width of each resulting
  bin.

- new_x_type:

  A character value denoting how new wavenumber values for the computed
  bins should be stored in the spectra of `x` after binning. Must be one
  of:

  `"start"`

  :   New wavenumbers for binned intensities are the start wavenumber
      value which defines the start of each bin. The default (for
      historical reasons).

  `"mean"`

  :   New wavenumbers for binned intensities are the average of the
      start and end wavenumber values which define the start and end of
      each bin.

  `"end"`

  :   New wavenumbers for binned intensities are the end wavenumber
      value which defines the end of each bin.

- return_ir_flat:

  Logical value. If `TRUE`, the spectra are returned as
  [`ir_flat`](https://henningte.github.io/ir/reference/ir_new_ir_flat.md)
  object.

## Value

An object of class `ir` (or `ir_flat`, if `return_ir_flat = TRUE`),
where spectra have been binned.

## Details

If a wavenumber value exactly matches the boundary of a bin window, the
respective intensity value will be assigned to both neighboring bins.

## Examples

``` r
# new wavenumber values are the first wavenumber value for each bin
x1 <-
   ir::ir_sample_data |>
   ir_bin(width = 50, new_x_type = "start")

# new wavenumber values are the last wavenumber value for each bin
x2 <-
   ir::ir_sample_data |>
   ir_bin(width = 50, new_x_type = "mean")

# new wavenumber values are the average of the wavenumber values assigned to
# each bin
x3 <-
   ir::ir_sample_data |>
   ir_bin(width = 50, new_x_type = "end")

# compare wavenumber values for first spectra.
cbind(x1$spectra[[1]]$x, x2$spectra[[1]]$x, x3$spectra[[1]]$x)
#>       [,1] [,2] [,3]
#>  [1,]  650  675  700
#>  [2,]  700  725  750
#>  [3,]  750  775  800
#>  [4,]  800  825  850
#>  [5,]  850  875  900
#>  [6,]  900  925  950
#>  [7,]  950  975 1000
#>  [8,] 1000 1025 1050
#>  [9,] 1050 1075 1100
#> [10,] 1100 1125 1150
#> [11,] 1150 1175 1200
#> [12,] 1200 1225 1250
#> [13,] 1250 1275 1300
#> [14,] 1300 1325 1350
#> [15,] 1350 1375 1400
#> [16,] 1400 1425 1450
#> [17,] 1450 1475 1500
#> [18,] 1500 1525 1550
#> [19,] 1550 1575 1600
#> [20,] 1600 1625 1650
#> [21,] 1650 1675 1700
#> [22,] 1700 1725 1750
#> [23,] 1750 1775 1800
#> [24,] 1800 1825 1850
#> [25,] 1850 1875 1900
#> [26,] 1900 1925 1950
#> [27,] 1950 1975 2000
#> [28,] 2000 2025 2050
#> [29,] 2050 2075 2100
#> [30,] 2100 2125 2150
#> [31,] 2150 2175 2200
#> [32,] 2200 2225 2250
#> [33,] 2250 2275 2300
#> [34,] 2300 2325 2350
#> [35,] 2350 2375 2400
#> [36,] 2400 2425 2450
#> [37,] 2450 2475 2500
#> [38,] 2500 2525 2550
#> [39,] 2550 2575 2600
#> [40,] 2600 2625 2650
#> [41,] 2650 2675 2700
#> [42,] 2700 2725 2750
#> [43,] 2750 2775 2800
#> [44,] 2800 2825 2850
#> [45,] 2850 2875 2900
#> [46,] 2900 2925 2950
#> [47,] 2950 2975 3000
#> [48,] 3000 3025 3050
#> [49,] 3050 3075 3100
#> [50,] 3100 3125 3150
#> [51,] 3150 3175 3200
#> [52,] 3200 3225 3250
#> [53,] 3250 3275 3300
#> [54,] 3300 3325 3350
#> [55,] 3350 3375 3400
#> [56,] 3400 3425 3450
#> [57,] 3450 3475 3500
#> [58,] 3500 3525 3550
#> [59,] 3550 3575 3600
#> [60,] 3600 3625 3650
#> [61,] 3650 3675 3700
#> [62,] 3700 3725 3750
#> [63,] 3750 3775 3800
#> [64,] 3800 3825 3850
#> [65,] 3850 3875 3900
#> [66,] 3900 3925 3950
#> [67,] 3950 3975 4000
#> [68,] 4000 4025 4050
```

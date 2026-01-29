# Bind rows of `ir` objects

Bind rows of `ir` objects

## Usage

``` r
# S3 method for class 'ir'
rbind(..., deparse.level = 1)

# S3 method for class 'ir'
cbind(..., deparse.level = 1)
```

## Arguments

- ...:

  Objects to bind together. For `cbind`, only the first of the objects
  is allowed to be of class `ir`.

- deparse.level:

  An integer value; see [`rbind()`](https://rdrr.io/r/base/cbind.html).

## Value

An object of class `ir`. `rbind` returns all input `ir` objects combined
row-wise. `cbind` returns the input `ir` object and the other objects
combined column-wise.

## Examples

``` r
# rbind
rbind(ir_sample_data, ir_sample_data)
#> # A tibble: 116 × 7
#>    id_measurement id_sample sample_type sample_comment             klason_lignin
#>  *          <int> <chr>     <chr>       <chr>                      <units>      
#>  1              1 GN 11-389 needles     Abies Firma Momi fir       0.359944     
#>  2              2 GN 11-400 needles     Cupressocyparis leylandii… 0.339405     
#>  3              3 GN 11-407 needles     Juniperus chinensis Chine… 0.267552     
#>  4              4 GN 11-411 needles     Metasequoia glyptostroboi… 0.350016     
#>  5              5 GN 11-416 needles     Pinus strobus Torulosa     0.331100     
#>  6              6 GN 11-419 needles     Pseudolarix amabili Golde… 0.279360     
#>  7              7 GN 11-422 needles     Sequoia sempervirens Cali… 0.329672     
#>  8              8 GN 11-423 needles     Taxodium distichum Cascad… 0.356950     
#>  9              9 GN 11-428 needles     Thuja occidentalis Easter… 0.369360     
#> 10             10 GN 11-434 needles     Tsuga caroliniana Carolin… 0.289050     
#> # ℹ 106 more rows
#> # ℹ 2 more variables: holocellulose <units>, spectra <named list>
rbind(ir_sample_data |> dplyr::select(spectra),
      ir_sample_data |> dplyr::select(spectra))
#> # A tibble: 116 × 1
#>    spectra             
#>  * <named list>        
#>  1 <tibble [3,351 × 2]>
#>  2 <tibble [3,351 × 2]>
#>  3 <tibble [3,351 × 2]>
#>  4 <tibble [3,351 × 2]>
#>  5 <tibble [3,351 × 2]>
#>  6 <tibble [3,351 × 2]>
#>  7 <tibble [3,351 × 2]>
#>  8 <tibble [3,351 × 2]>
#>  9 <tibble [3,351 × 2]>
#> 10 <tibble [3,351 × 2]>
#> # ℹ 106 more rows

# cbind
cbind(ir_sample_data, a = seq_len(nrow(ir_sample_data)))
#> # A tibble: 58 × 8
#>    id_measurement id_sample sample_type sample_comment             klason_lignin
#>  *          <int> <chr>     <chr>       <chr>                      <units>      
#>  1              1 GN 11-389 needles     Abies Firma Momi fir       0.359944     
#>  2              2 GN 11-400 needles     Cupressocyparis leylandii… 0.339405     
#>  3              3 GN 11-407 needles     Juniperus chinensis Chine… 0.267552     
#>  4              4 GN 11-411 needles     Metasequoia glyptostroboi… 0.350016     
#>  5              5 GN 11-416 needles     Pinus strobus Torulosa     0.331100     
#>  6              6 GN 11-419 needles     Pseudolarix amabili Golde… 0.279360     
#>  7              7 GN 11-422 needles     Sequoia sempervirens Cali… 0.329672     
#>  8              8 GN 11-423 needles     Taxodium distichum Cascad… 0.356950     
#>  9              9 GN 11-428 needles     Thuja occidentalis Easter… 0.369360     
#> 10             10 GN 11-434 needles     Tsuga caroliniana Carolin… 0.289050     
#> # ℹ 48 more rows
#> # ℹ 3 more variables: holocellulose <units>, a <integer>, spectra <named list>
```

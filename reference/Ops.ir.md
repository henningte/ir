# Arithmetic operations for `ir` objects

Arithmetic operations for `ir` objects

## Usage

``` r
# S3 method for class 'ir'
Ops(e1, e2)
```

## Arguments

- e1:

  An object of class `ir`.

- e2:

  An object of class `ir` or a numeric value.

## Value

`e1` with intensity values of the spectra added to/subtracted
with/multiplied with/divided by those in `e2`:

- If `e2` is a numeric value, all intensity values in the spectra of
  `e1` are added/subtracted/multiplied/divided by `e2`.

- If `e2` is an `ir` object with one row, it is replicated (see
  [rep.ir](https://henningte.github.io/ir/reference/rep.ir.md)) so that
  the row numbers match to those of `e1` and intensity values are
  added/subtracted/multiplied/divided row-wise.

- If `e2` is an `ir` object with the same number of rows as `e1`,
  intensity values are added/subtracted/multiplied/divided row-wise.

## Examples

``` r
## addition
ir::ir_sample_data + ir::ir_sample_data
#> # A tibble: 58 × 7
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
#> # ℹ 2 more variables: holocellulose <units>, spectra <list>
ir::ir_sample_data + 2
#> # A tibble: 58 × 7
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
#> # ℹ 2 more variables: holocellulose <units>, spectra <list>
ir::ir_sample_data +
   seq(from = 0, to = 2, length.out = nrow(ir::ir_sample_data))
#> # A tibble: 58 × 7
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
#> # ℹ 2 more variables: holocellulose <units>, spectra <list>

## subtraction
ir::ir_sample_data - ir::ir_sample_data
#> # A tibble: 58 × 7
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
#> # ℹ 2 more variables: holocellulose <units>, spectra <list>
ir::ir_sample_data - 2
#> # A tibble: 58 × 7
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
#> # ℹ 2 more variables: holocellulose <units>, spectra <list>
ir::ir_sample_data -
   seq(from = 0, to = 2, length.out = nrow(ir::ir_sample_data))
#> # A tibble: 58 × 7
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
#> # ℹ 2 more variables: holocellulose <units>, spectra <list>

## multiplication
ir::ir_sample_data * ir::ir_sample_data
#> # A tibble: 58 × 7
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
#> # ℹ 2 more variables: holocellulose <units>, spectra <list>
ir::ir_sample_data * 2
#> # A tibble: 58 × 7
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
#> # ℹ 2 more variables: holocellulose <units>, spectra <list>
ir::ir_sample_data *
   seq(from = 0, to = 2, length.out = nrow(ir::ir_sample_data))
#> # A tibble: 58 × 7
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
#> # ℹ 2 more variables: holocellulose <units>, spectra <list>

## division
ir::ir_sample_data / ir::ir_sample_data
#> # A tibble: 58 × 7
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
#> # ℹ 2 more variables: holocellulose <units>, spectra <list>
ir::ir_sample_data / 2
#> # A tibble: 58 × 7
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
#> # ℹ 2 more variables: holocellulose <units>, spectra <list>
ir::ir_sample_data /
   seq(from = 0.1, to = 2, length.out = nrow(ir::ir_sample_data))
#> # A tibble: 58 × 7
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
#> # ℹ 2 more variables: holocellulose <units>, spectra <list>
```

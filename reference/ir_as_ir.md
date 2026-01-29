# Converts an object to class `ir`

`ir_as_ir` converts an object to an object of class
[`ir`](https://henningte.github.io/ir/reference/ir_new_ir.md).

## Usage

``` r
ir_as_ir(x, ...)

# S3 method for class 'ir'
ir_as_ir(x, ...)

# S3 method for class 'data.frame'
ir_as_ir(x, ...)

# S3 method for class 'ir_flat'
ir_as_ir(x, ...)

# S3 method for class 'hyperSpec'
ir_as_ir(x, ...)

# S3 method for class 'Spectra'
ir_as_ir(x, ...)
```

## Arguments

- x:

  An object.

- ...:

  Further arguments passed to individual methods.

  - If `x` is a data frame, an object of class `ir`, an object of class
    `hyperSpec` (from package 'hyperSpec'), or an object of class
    `Spectra` (from package 'ChemoSpec'), these are ignored.

## Value

An object of class `ir` with available metadata from original objects.

## Examples

``` r
# conversion from an ir object
ir::ir_sample_data |>
  ir_as_ir()
#> # A tibble: 58 × 7
#>    id_measurement id_sample sample_type sample_comment             klason_lignin
#>             <int> <chr>     <chr>       <chr>                      <units>      
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
#> # ℹ 2 more variables: holocellulose <units>, spectra <named list>

# conversion from a data frame
x_ir <- ir::ir_sample_data

x_df <-
  x_ir |>
  ir_drop_spectra() |>
  dplyr::mutate(
    spectra = x_ir$spectra
  ) |>
  ir_as_ir()

# check that ir_as_ir preserves the input class
ir_sample_data |>
  structure(class = setdiff(class(ir_sample_data), "ir")) |>
  dplyr::group_by(sample_type) |>
  ir_as_ir()
#> # A tibble: 58 × 7
#> # Groups:   sample_type [8]
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
#> # ℹ 2 more variables: holocellulose <units>, spectra <named list>


# conversion from an ir_flat object
x_ir <-
  ir::ir_sample_data |>
  ir::ir_flatten() |>
  ir::ir_as_ir()

# conversion from a hyperSpec object from package hyperSpec
if(requireNamespace("hyperSpec")) {
  x_hyperSpec <- hyperSpec::laser
  x_ir <- ir_as_ir(x_hyperSpec)
}

# conversion from a Spectra object from class ChemoSpec
if(requireNamespace("ChemoSpec")) {

  ## sample data
  x <- ir_sample_data
  x_flat <- ir_flatten(x)

  ## creation of the object of class "Spectra" (the ChemoSpec package does
  ## not contain a sample Spectra object)
  n <- nrow(x)
  group_vector <- seq(from = 1, to = n, by = 1)
  color_vector <- rep("black", times = n)
  x_Spectra <- list() # dummy list
  x_Spectra$freq <- as.numeric(x_flat[,1, drop = TRUE]) # wavenumber vector
  x_Spectra$data <- as.matrix(t(x_flat[,-1])) # absorbance values as matrix
  x_Spectra$names <- as.character(seq_len(nrow(x))) # sample names
  x_Spectra$groups <- as.factor(group_vector) # grouping vector
  x_Spectra$colors <- color_vector # colors used for groups in plots
  x_Spectra$sym <- as.numeric(group_vector) # symbols used for groups in plots
  x_Spectra$alt.sym <- letters[as.numeric(group_vector)] # letters used for groups in plots
  x_Spectra$unit <- c("wavenumbers", "intensity") # unit of x and y axes
  x_Spectra$desc <- "NULL" # optional descriptions in plots
  attr(x_Spectra, "class") <- "Spectra"

  # conversion to ir
  x_ir <- ir_as_ir(x_Spectra)
}
#> Loading required namespace: ChemoSpec
```


<!-- README.md is generated from README.Rmd. Please edit that file -->

# ir <img src='man/figures/logo-hex.png' align="right" height="139" alt="logo" style="float:right; height:200px;" />

<!-- badges: start -->

[![DOI](https://zenodo.org/badge/234117897.svg)](https://zenodo.org/badge/latestdoi/234117897)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![CRAN
status](https://www.r-pkg.org/badges/version/ir)](https://CRAN.R-project.org/package=ir)
<!-- badges: end -->

## Overview

‘ir’ is an R package to import, handle and preprocess infrared spectra.
Infrared spectra are stored as list columns in data frames which enables
efficient storage of metadata along with the spectra and using
‘tidyverse’ functions for data operations.

**Spectra in the following formats can be imported:**

1.  `csv` files with individual spectra.
2.  Thermo Galactic’s `spc` files with individual spectra.

**Functions for spectral preprocessing and data handling:**

1.  baseline correction with:
    - a polynomial baseline
    - a convex hull baseline
    - a Savitzky-Golay baseline (Lasch 2012)
2.  binning
3.  clipping
4.  interpolating (resampling, linearly)
5.  replacing selected parts of a spectrum by a straight line
6.  averaging spectra within specified groups
7.  normalizing spectra:
    - to the maximum intensity
    - to the intensity at a specific x value
    - so that all intensity values sum to 1
    - vector normalization
8.  smoothing:
    - Savitzky-Golay smoothing
    - Fourier smoothing
9.  computing derivatives of spectra using Savitzky-Golay smoothing
10. spectral arithmetic (addition, subtraction, multiplication,
    division)
11. computing the variance of intensity values (optionally after
    subtracting reference spectra)
12. computing maxima, minima, median, and ranges of intensity values of
    spectra
13. Atmospheric background correction (Perez-Guaita et al. 2013)
14. Scaling intensity values in spectra
15. plotting
16. [tidyverse](https://www.tidyverse.org/) methods

### How to install

You can install ‘ir’ from CRAN:

``` r
install.packages("ir")
```

You can install the development version of ‘ir’ from GitHub:

``` r
remotes::install_github(repo = "henningte/ir", ref = "dev")
```

### How to use

Load ‘ir’:

``` r
# load ir package
library(ir)

# load additional packages needed for this tutorial
library(ggplot2)
```

For brief introductions, see below and the two vignettes:

1.  [Introduction to the ‘ir’
    package](https://henningte.github.io/ir/articles/ir-introduction.html)
2.  [Introduction to the
    `ir`class](https://henningte.github.io/ir/articles/ir-class.html)

#### Sample workflow

Here is an example preprocessing pipeline for the sample data in the
package (`ir_sample_data`) which does baseline correction, binning (bin
width of 10), normalization (so that the maximum intensity value is 1
and the minimum intensity value is 0), and finally plots the
preprocessed spectra for each sample and sample type:

``` r
ir_sample_data %>%                                      # data
  ir::ir_bc(method = "rubberband") %>%                  # baseline correction
  ir::ir_bin(width = 10) %>%                            # binning
  ir::ir_normalize(method = "zeroone") %>%              # normalization
  plot() + ggplot2::facet_wrap(~ sample_type)           # plot
```

![](man/figures/README-sample_data_workflow-1.png)<!-- -->

#### Data structure

You can load the sample data with:

``` r
ir::ir_sample_data
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
#> # … with 48 more rows, and 2 more variables: holocellulose <units>,
#> #   spectra <named list>
```

`ir_sample_data` is an object of class `ir`. An Object of class `ir` is
a data frame where each row represents one infrared measurement and
column `spectra` contains the infrared spectra (one per row) as list
column. This allows to store metadata along each spectrum (for example
the N content for each sample) and to manipulate `ir` objects with
‘tidyverse’ functions.

The column `spectra` is a list column of data frames, meaning that each
cell of `spectra` contains a data frame which contains the data for one
spectrum. For example, here are the first rows of the first spectrum in
`ir_smaple_data`:

``` r
# View the first ten rows of the first spectrum in ir_sample_data
head(ir_sample_data$spectra[[1]])
#> # A tibble: 6 × 2
#>       x        y
#>   <int>    <dbl>
#> 1  4000 0.000361
#> 2  3999 0.000431
#> 3  3998 0.000501
#> 4  3997 0.000571
#> 5  3996 0.000667
#> 6  3995 0.000704
```

Column `x` contains values for the spectral channel (in this case
wavenumbers \[cm<sup>-1</sup>\]) and column `y` the corresponding
intensity values.

### How to cite

Please cite this R package as:

> Henning Teickner (2023). *ir: Functions to Handle and Preprocess
> Infrared Spectra*. DOI: 10.5281/zenodo.5747169. Accessed 18 Mrz 2023.
> Online at <https://zenodo.org/record/5747169>.

### Companion packages

The [irpeat](https://github.com/henningte/irpeat/) package provides
functions to analyze infrared spectra of peat (humification indices,
prediction models) and uses the ‘ir’ package to handle spectral data.

### Licenses

**Text and figures :** [CC BY
4.0](https://creativecommons.org/licenses/by/4.0/)

**Code :** See the [DESCRIPTION](DESCRIPTION) file

**Data :** [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/)
attribution requested in reuse. See the sources section for data sources
and how to give credit to the original author(s) and the source.

### Contributions

We welcome contributions from everyone. Before you get started, please
see our [contributor guidelines](CONTRIBUTING.md). Please note that this
project is released with a [Contributor Code of Conduct](CONDUCT.md). By
participating in this project you agree to abide by its terms.

### Sources

`ir_sample_data` is derived from Hodgkins et al. (2018) and was
reformatted to match the requirements of ‘ir’. The original article
containing the data can be downloaded from
<https://www.nature.com/articles/s41467-018-06050-2> and is distributed
under the Creative Commons Attribution 4.0 International License
(<https://creativecommons.org/licenses/by/4.0/>). The data on Klason
lignin and holocellulose content are originally from De La Cruz,
Florentino B., Osborne, and Barlaz (2016).

This packages was developed in R (R version 4.2.0 (2022-04-22 ucrt)) (R
Core Team 2019) using functions from ‘devtools’ (Wickham, Hester, and
Chang 2019), ‘usethis’ (Wickham and Bryan 2019), and ‘roxygen2’ (Wickham
et al. 2019).

### References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-LaCruz.2016" class="csl-entry">

De La Cruz, Florentino B., Jason Osborne, and Morton A. Barlaz. 2016.
“<span class="nocase">Determination of Sources of Organic Matter in
Solid Waste by Analysis of Phenolic Copper Oxide Oxidation Products of
Lignin</span>.” *Journal of Environmental Engineering* 142 (2):
04015076. <https://doi.org/10.1061/(ASCE)EE.1943-7870.0001038>.

</div>

<div id="ref-Hodgkins.2018" class="csl-entry">

Hodgkins, Suzanne B., Curtis J. Richardson, René Dommain, Hongjun Wang,
Paul H. Glaser, Brittany Verbeke, B. Rose Winkler, et al. 2018. “<span
class="nocase">Tropical peatland carbon storage linked to global
latitudinal trends in peat recalcitrance</span>.” *Nature
Communications* 9 (1): 3640.
<https://doi.org/10.1038/s41467-018-06050-2>.

</div>

<div id="ref-Lasch.2012" class="csl-entry">

Lasch, Peter. 2012. “<span class="nocase">Spectral Pre-Processing for
Biomedical Vibrational Spectroscopy and Microspectroscopic
Imaging</span>.” *Chemometrics and Intelligent Laboratory Systems* 117
(August): 100–114. <https://doi.org/10.1016/j.chemolab.2012.03.011>.

</div>

<div id="ref-PerezGuaita.2013" class="csl-entry">

Perez-Guaita, David, Julia Kuligowski, Guillermo Quintás, Salvador
Garrigues, and Miguel de La Guardia. 2013. “<span
class="nocase">Atmospheric compensation in Fourier transform infrared
(FT-IR) spectra of clinical samples</span>.” *Applied Spectroscopy* 67
(11): 1339–42. <https://doi.org/10.1366/13-07159>.

</div>

<div id="ref-RCoreTeam.2019" class="csl-entry">

R Core Team. 2019. “<span class="nocase">R: A Language and Environment
for Statistical Computing</span>.” Vienna, Austria: R Foundation for
Statistical Computing. <https://www.R-project.org/>.

</div>

<div id="ref-Wickham.2019b" class="csl-entry">

Wickham, Hadley, and Jennifer Bryan. 2019. “<span
class="nocase">usethis: Automate Package and Project Setup</span>.”
<https://CRAN.R-project.org/package=usethis>.

</div>

<div id="ref-Wickham.2019c" class="csl-entry">

Wickham, Hadley, Peter Danenberg, Gábor Csárdi, and Manuel Eugster.
2019. “<span class="nocase">roxygen2: In-Line Documentation for
R</span>.” <https://CRAN.R-project.org/package=roxygen2>.

</div>

<div id="ref-Wickham.2019" class="csl-entry">

Wickham, Hadley, Jim Hester, and Winston Chang. 2019. “<span
class="nocase">devtools: Tools to Make Developing R Packages
Easier</span>.” <https://CRAN.R-project.org/package=devtools>.

</div>

</div>

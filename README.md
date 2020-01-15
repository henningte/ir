
<!-- README.md is generated from README.Rmd. Please edit that file -->
ir
==

ir is an R package that contains simple functions to import, handle and preprocess infrared spectra. Infrared spectra are stored as list columns in `data.frame`s to enable efficient storage of metadata along with the spectra and support further analyses containing other data for the same samples.

Supported file formats for import currently are:

1.  .csv files with indiidual spectra.
2.  Thermo Galactics .spc files with individual spectra.

Provided functions for preprocessing and general handling are:

1.  baseline correction with:
    -   a polynomial
    -   a convex hull procedure.
2.  binning.
3.  clipping.
4.  interpolating (resampling, linearly).
5.  replacing selected parts of a spectrum by a straight line.
6.  averaging spectra within specified groups.
7.  normalise spectra:
    -   to the maximum intensity
    -   to the intensity at a specific x value
    -   so that all itensity values sum to 1.
8.  smoothing:
    -   Savitzky-Golay smoothing
    -   Fourier smoothing.

### How to install

You can install ir from GitHub using R via:

``` r
remotes::install_github(repo = "henningte/ir")
```

### How to use

You can load ir in R with:

``` r
library(ir)

# load additional packages needed for this tutorial
library(ggplot2)
library(magrittr)
```

You can load the sample data with:

``` r
ir::ir_sample_data
```

`ir_sample_data` is an object of class `ir`. An Object of class `ir` is basically a `data.frame` where each row represents one infrared measurement and column `spectra` contains the infrared spectra (one per row) and columns `measurement_id` and `sample_id` represent identifiers for each measurement and sample, respectively. This allows effectively storing repeated measurements for the same sample in the same table, as-well-as any metadata and accessory data (e.g. nitrogen content of the sample).

The column `spectra` is a list column of `data.frame`s, meaning that each cell in `sample_data` contains for column `spectra` a `data.frame`. For example, the first element of `ir_sample_data$spectra` represents the first spectrum as a `data.frame`:

``` r
ir::ir_get_spectrum(ir_sample_data, what = 1)[[1]] %>% 
  head(10)
```

Column `x` represents the x values (in this case wavenumbers \[cm<sup>-1</sup>\]) and column `y` the corresponding intensity values.

A simple workflow would be, for example, to baseline correct the spectra, then bin them to bins with a width of 10 wavenumber units, then normalise them so that the maximum intensity value is 1 and the minimum intensity value is 0 and then plot the baseline corrected samples for each sample:

``` r
ir::ir_bc(ir_sample_data, method = "rubberband") %>%    # baseline correction
  ir::ir_bin(width = 10) %>%                         # binning
  ir::ir_normalise(method = "zeroone") %>%           # normalisation
  plot() + ggplot2::facet_wrap(~ sample_type)        # plot
```

### How to cite

Please cite this R package as:

> Henning Teickner, (2020). *ir: A Simple Package to Handle and Preprocess Infrared Spectra'*. Accessed 15 Jan 2020. Online at <https://github.com/henningte/ir>.

### Licenses

**Text and figures :** [CC-BY-4.0](http://creativecommons.org/licenses/by/4.0/)

**Code :** See the [DESCRIPTION](DESCRIPTION) file

**Data :** [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/) attribution requested in reuse. See the sources section for data sources and how to give credit to the original author(s) and the source.

### Contributions

We welcome contributions from everyone. Before you get started, please see our [contributor guidelines](CONTRIBUTING.md). Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.

### Sources

The complete data in this package is derived from Hodgkins et al. (2018) and was restructured to match the requirements of ir. The original article containing the data can be downloaded from <https://www.nature.com/articles/s41467-018-06050-2> and is distributed under the Creative Commons Attribution 4.0 International License (<http://creativecommons.org/licenses/by/4.0/>). The data was originally derived from F. B. de La Cruz et al. (2015).

This packages was developed in R (R version 3.5.3 (2019-03-11)) (R Core Team 2019) using functions from devtools (Wickham, Hester, and Chang 2019), usethis (Wickham and Bryan 2019) and rrtools (Marwick 2019).

### References

Hodgkins, Suzanne B., Curtis J. Richardson, René Dommain, Hongjun Wang, Paul H. Glaser, Brittany Verbeke, B. Rose Winkler, et al. 2018. “Tropical peatland carbon storage linked to global latitudinal trends in peat recalcitrance.” *Nature communications* 9 (1): 3640. doi:[10.1038/s41467-018-06050-2](https://doi.org/10.1038/s41467-018-06050-2).

La Cruz, Florentino Banaag de, Thorsten Dittmar, Jutta Niggemann, Christopher L. Osburn, and Morton A. Barlaz. 2015. “Evaluation of Copper Oxide Oxidation for Quantification of Lignin in Municipal Solid Waste.” *Environmental Engineering Science* 32 (6): 486–96. doi:[10.1089/ees.2014.0402](https://doi.org/10.1089/ees.2014.0402).

Marwick, Ben. 2019. “rrtools: Creates a Reproducible Research Compendium.” <https://github.com/benmarwick/rrtools>.

R Core Team. 2019. “R: A Language and Environment for Statistical Computing.” Vienna, Austria: R Foundation for Statistical Computing. <https://www.R-project.org/>.

Wickham, Hadley, and Jennifer Bryan. 2019. “usethis: Automate Package and Project Setup.” <https://CRAN.R-project.org/package=usethis>.

Wickham, Hadley, Jim Hester, and Winston Chang. 2019. “devtools: Tools to Make Developing R Packages Easier.” <https://CRAN.R-project.org/package=devtools>.

# Sample object of class `ir`

A sample object of class
[`ir`](https://henningte.github.io/ir/reference/ir_new_ir.md). The data
set contains ATR-MIR spectra for a set of organic reference materials
along with their metadata (types of samples and a description) and
accessory data (Klason lignin mass fraction and holocellulose mass
fraction).

## Usage

``` r
ir_sample_data
```

## Format

A data frame with 58 rows and 7 variables:

- id_measurement:

  An integer vector with a unique id for each spectrum.

- id_sample:

  A character vector with a unique id for each sample.

- sample_type:

  A character vector containing class labels for the types of reference
  materials.

- sample_comment:

  A character vector containing comments to each sample.

- klason_lignin:

  A numeric vector with the mass fractions of Klason lignin in each
  sample.

- holocellulose:

  A numeric vector with the mass fractions of holocellulose in each
  sample.

- spectra:

  See
  [`ir_new_ir()`](https://henningte.github.io/ir/reference/ir_new_ir.md).

## Source

The data set was derived from
<https://www.nature.com/articles/s41467-018-06050-2> and published by
Hodgkins et al. (2018) under the CC BY 4.0 license
<https://creativecommons.org/licenses/by/4.0/>. Hodgkins et al. (2018)
originally derived the data on Klason Lignin and Holocellulose content
from De la Cruz et al. (2016) .

## References

De la Cruz FB, Osborne J, Barlaz MA (2016). “Determination of Sources of
Organic Matter in Solid Waste by Analysis of Phenolic Copper Oxide
Oxidation Products of Lignin.” *Journal of Environmental Engineering*,
**142**(2), 04015076. ISSN 0733-9372, 1943-7870.
[doi:10.1061/(ASCE)EE.1943-7870.0001038](https://doi.org/10.1061/%28ASCE%29EE.1943-7870.0001038)
.  
  
Hodgkins SB, Richardson CJ, Dommain R, Wang H, Glaser PH, Verbeke B,
Winkler BR, Cobb AR, Rich VI, Missilmani M, Flanagan N, Ho M, Hoyt AM,
Harvey CF, Vining SR, Hough MA, Moore TR, Richard PJH, De La Cruz FB,
Toufaily J, Hamdan R, Cooper WT, Chanton JP (2018). “Tropical Peatland
Carbon Storage Linked to Global Latitudinal Trends in Peat
Recalcitrance.” *Nature Communications*, **9**(1), 3640. ISSN 2041-1723.
[doi:10.1038/s41467-018-06050-2](https://doi.org/10.1038/s41467-018-06050-2)
.

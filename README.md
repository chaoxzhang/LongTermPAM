# LongTermPAM

The LongTermPAM package provides a means to automatically identify and filter spurious observations in F', Fm' and quantum yield of photosystem II (Y(II) or ΦP) caused by such as snow and condensation in long-term Monitoring PAM measurements, and calculate different chlorophyll fluorescence (ChlF) parameters. Detailed instruction of this package can be found in XX.

## Install LongTermPAM R package using:

`{remotes::install_github("chaoxzhang/LongTermPAM", build_vignettes = TRUE)}`

## The workflow consists of three steps:

1.  Read and prepare the data using `readPAM` and/or `correctF` function(s)

2.  Filter and remove spurious data using filtering functions:

    `filter1.lowF`, `filter2.night`, `filter3.day`, `filter4.FVFM`, `filter5.expand`, and `filter6.adjacent`

3.  Calculate ChlF parameters using functions:

    `ChlFRef`, `diurnalParams`, and `seasonalParams`.

## Data visualization

All data outputs from this package can be visualized using functions start with plot:

`plotPAM`, `plotCheckFilter`, `plotFVFM`, `plotDiurnQuench`, `plotDiurnYield`, `plotSeasonPara`.

## Examples and instruction

Detailed instructions on how to use this R package can be found in the package documentation, along with two example datasets: <https://github.com/chaoxzhang/LongTermPAM/blob/main/vignettes/Intro_to_LongTermPAM.Rmd>

Users can download and use this introduction file directly to process their own data by replacing the example datasets.

## Cite this R package using:

**add later**

**All output data and figures can be accessed here:**

add later

## Reference:

Reference will be added later on.

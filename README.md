# LongTermPAM

The LongTermPAM package provides a means to automatically identify and filter spurious observations in F', Fm' and quantum yield of photosystem II (Y(II) or ΦP) caused by such as snow and condensation in long-term Monitoring PAM measurements, and calculate different chlorophyll fluorescence (ChlF) parameters. Detailed instruction of this package can be found in Zhang et al., (2025, in submission).

## The workflow consists of three steps:

1.  Read and prepare the data using `readPAM` and/or `correctF` function(s)

2.  Filter and remove spurious data using filtering functions:

    `filter1.lowF`, `filter2.night`, `filter3.day`, `filter4.FVFM`, `filter5.expand`, and `filter6.adjacent`

3.  Calculate ChlF parameters using functions:

    `ChlFRef`, `diurnalParams`, and `seasonalParams`.

## Data visualization

All data outputs from this package can be visualized using functions start with plot:

`plotRawData`, `plotCheckfilter`, `plotFVFM`, `plotDiurnQuench`, `plotDiurnYield`, `plotSeasQuench`, and `plotSeasYield`.

## Examples and instruction

Detailed instructions on how to use this R package can be found in the package documentation, along with two example datasets: <https://github.com/chaoxzhang/LongTermPAM/blob/main/vignettes/Intro_to_LongTermPAM.Rmd>

Users can download and use this introduction file directly to process their own data by replacing the example datasets.

**All output data and figures can be accessed here:**

<https://doi.org/10.5281/zenodo.14922468>

## Reference:

Chao Zhang, Erhard E. Pfündel, Jon Atherton, Juho Aalto, Jia Bai, Toivo Pohja, Paulina Rajewicz and Albert Porcar-Castell. A practical guide to long-term field PAM chlorophyll fluorescence measurements: setup, installation, data processing with R package ‘LongTermPAM’ and interpretation. (in submission)

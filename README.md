PRODES-Caatinga experiments and pipelines developed using the SITS package
================

<img src="./docs/figures/biomasbr_logo.jpg" align="right" height="100" width="100"/>

This repository brings together reproducible experiments and processing pipelines
from the PRODES-Caatinga project, developed using the SITS package. Its purpose
is to clearly and systematically document the adopted workflows,
providing references for experimentation, validation, and methodological
improvements, while supporting reproducibility and the continuous
evolution of project's analyses.

# Getting started

To use the scripts in this repository, clone the project to
your local machine using the command below:

``` sh
git clone https://github.com/francojra/sits-prodes-caatinga.git
```

After cloning, open the sits-prodes-caatinga directory in RStudio and install the
package using the command:

``` r
devtools::install(".")
```

# Repository structure

- `data/`: Datasets used and generated throughout the analyses
- `docs/`: Supplementary resources as word, pptx, figures (png, jpg, tif), pdfs, shp, gpkg and xlsx
- `results/`: Our results from processing with sits
- `scripts/`: Processing and experimentation routines
- `R/`: Package functions

# Folders containing .tif results from the classifications

### LTAE: Bands

### LTAE: Bands + NDII

https://drive.google.com/drive/folders/1-L1DtwxDkXNRLOserSdX_mMRYUm9DyW7

### LTAE: Bands + DBSI

https://drive.google.com/drive/folders/1NkxmZxk8wI5CtrdbFRstI_yfbze3nVq8

# Documentation

Visit the sitsbook to explore the package *sits* and learn more about:

``` sh
https://e-sensing.github.io/sitsbook/
```

# License

The data and results available in this repository are licensed under MIT License. Copyright (c) 2026 autores prodes.caatinga. Please consult the license file.

## Support

For questions, suggestions, or issues, please use the **Issues** section or
contact the repository maintainers.

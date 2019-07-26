# tealeaves-ms

[![DOI](https://zenodo.org/badge/167281492.svg)](https://zenodo.org/badge/latestdoi/167281492)

[{tealeaves}: an R package for modeling leaf temperature using energy budgets](https://doi.org/10.1101/529487). Accepted in *AoB PLANTS*.

This is a manuscript associated with the R package [{tealeaves}](https://github.com/cdmuir/tealeaves). This project was developed by [Chris Muir](https://www.chrisdmuir.com).

More information about the study is available on [github](https://github.com/cdmuir/tealeaves-ms/blob/master/ms/ms.pdf).

## Downloading data and code 

1. Download or clone this repository to your machine.

```
git clone git@github.com:cdmuir/tealeaves-ms.git
```

2. Open `tealeaves-ms.Rproj` in [RStudio](https://www.rstudio.com/)

## Generating manuscript

You can source all the code you need in the correct order using `r/run-all.R`. Even if you don't want to run all the code, you may need to install some packages (`r/install-packages.R`) and attach them (`r/header.R`).

- To use premade R output, simply open `ms/ms.Rnw` and compile using RStudio.
- Next, source `r/run-all.R` in the R Console:

```
# This will take several minutes to run
source("r/run-all.R")
```

[![CC BY](http://i.creativecommons.org/l/by/3.0/88x31.png)](http://creativecommons.org/licenses/by/3.0/)

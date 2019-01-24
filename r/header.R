rm(list = ls())
graphics.off()

# remove before final publication
file.copy("~/Google Drive/professional/resources/bibliography/refs.bib",
          "~/Google Drive/professional/research/active/tealeaves/tealeaves-ms/ms/refs.bib", 
          overwrite = TRUE)

# Libraries
library(magrittr)
library(tealeaves)
library(tidyverse)

source("r/functions.R")

palette(colorRampPalette(c("tomato", "steelblue"), alpha = TRUE)(5))


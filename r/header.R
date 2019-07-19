rm(list = ls())
graphics.off()

# remove before final publication
file.copy("/Volumes/GoogleDrive/My Drive/resources/bibliography/refs.bib",
          #"/Volumes/GoogleDrive/My Drive/research/02_review/tealeaves-ms/ms/refs.bib",
          "/Users/cdmuir/Desktop/tealeaves-ms/ms/refs.bib",
           overwrite = TRUE)

# Libraries
library(cowplot)
library(magrittr)
library(tealeaves)
library(tidyverse)

source("r/functions.R")

palette(colorRampPalette(c("tomato", "steelblue"), alpha = TRUE)(5))


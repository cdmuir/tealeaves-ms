# Uncomment if you need to install packages:
# source("r/install-packages.R")

source("r/header.R")

source("r/01_run-exe.R")
source("r/02_plot-exe.R")

# Render manuscript
setwd(path_ms)
rmarkdown::render("ms.Rmd", c("html_document"))
setwd(path)

#knit("ms.Rmd")
#knit("ms.md")
#knit("supplement.Rmd")

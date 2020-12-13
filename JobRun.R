library(rmarkdown)
Sys.setenv(RSTUDIO_PANDOC="C:/Users/frbrit/Documents/RStudio/bin/pandoc")
setwd("C:/Users/frbrit/Documents/R/Projects/Covid Analysis")
rmarkdown::render("Index2.Rmd", output_file = "index.html")

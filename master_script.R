# Master script

# check if packrat is set up correctly
print("Is packrat set up correctly?")
packrat::status()

source("rscripts/04a-Case-Study-Egypt.R")
source("rscripts/05a-Case-Study-Tunisia.R")
source("rscripts/06-Quantitative-Surveys.R")
source("rscripts/07-Quantitative-Surveys-Other.R")

rmarkdown::render("README.Rmd")
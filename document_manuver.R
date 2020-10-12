# Directory
getwd()
dir.create("Documents")
dir.create("Original_data")
dir.create("Analysis_data")
dir.create("Command_files")

# Packages
library(rmarkdown)

draft("Documents/manuscript.Rmd", template="pdf", package="pinp", edit=FALSE)
render("Documents/manuscript.Rmd")

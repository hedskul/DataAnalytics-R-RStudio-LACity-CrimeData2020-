# 20250506
# @author hedskul
# ------------------------------------------------------------------------------
# Final
# LA - Crime Date 2020-Present 
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Create an R Project for this lab and include the data file for question
# 5.9.1, as well as your solution, in the project.  Zip and upload the
# project folder.
# ------------------------------------------------------------------------------

# if (!require(readr))
#   install.packages(readr)
# library(readr)
# if (!require(purrr))
#   install.packages(purrr)
# library(purrr)
# if (!require(readxl))
#   install.packages(readxl)
# library(readxl)
# if (!require(tidyr))
#   install.packages(tidyr)
# library(tidyr)
# if (!require(dplyr))
#   install.packages(dplyr)
# library(dplyr)
# if (!require(ggplot2))
#   install.packages(ggplot2)
# library(ggplot2)
# if (!require(dslabs))
#   install.packages(dslabs)
# library(dslabs)
# 
# path <- system.file("extdata", package = "dslabs")
# files <- list.files(path)
# files
# 
# for (file in files) {
#   if (length(grep("csv", file, fixed=TRUE)!=0)) {
#     f <- read_csv(
#       file.path(path,file),
#       locale = locale(encoding = "ISO-8859-1"),
#       show_col_types = FALSE
#     )
#   }
# }
# # file with path vector
# absolute_path_and_filenames <- file.path(path, files)
# each2 <- read_csv(
#   absolute_path_and_filenames[2],
#   locale = locale(encoding = "ISO-8859-1"),
#   show_col_types = FALSE
# )
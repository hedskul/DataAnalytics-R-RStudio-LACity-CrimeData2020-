?read_csv
# read_delim {readr}
# Read a delimited file (including CSV and TSV) into a tibbles.
# # Read from multiple file paths at once
# continents <- c("africa", "americas", "asia", "europe", "oceania")
# filepaths <- vapply(
#   paste0("mini-gapminder-", continents, ".csv"),
#   FUN = readr_example,
#   FUN.VALUE = character(1)
# )
# read_csv(filepaths, id = "file")
# # Including remote paths
# remote_csv <- read_csv(
#   "https://github.com/tidyverse/readr/raw/main/inst/extdata/mtcars.csv"
# );

?setwd()
# # Get or Set Working Directory
# (WD <- getwd())
# if (!is.null(WD)) setwd(WD)

?file
# # Functions to Manipulate Connections (Files, URLs, ...)
# zzfil <- tempfile(fileext=".data")

?file.path
# Construct the path to a file from components in a platform-independent way.
# file.path(..., fsep = .Platform$file.sep)

?getwd();
# getwd();
# list.files(path = ".", pattern = NULL, all.files = FALSE,
# ..
# .

?select
# select {dplyr}
# Keep or drop columns using their names and types
# starwars %>% select(!(name:mass));
# # Error in starwars %>% select(!(name:mass)) : 
# #   could not find function "%>%"
# FIX: library(tidyr);

?where
# # where {tidyselect}
# # where(fn)
# iris %>% select(where(is.numeric))
# iris %>% select(where(is.factor))
# # For better printing
# iris <- as_tibble(iris)

?vroom
# # Read a delimited file into a tibble
# input_file <- vroom_example("mtcars.csv")
# input_file

?problems
# problems {readr}
# # Retrieve parsing problems
# stop_for_problems(x);
# x <- parse_integer(c("1X", "blah", "3"))
# problems(x)

?names
# # Functions to get or set the names of an object.
# names(x)

?head
# # Return the First or Last Parts of an Object
# head(x, ...)
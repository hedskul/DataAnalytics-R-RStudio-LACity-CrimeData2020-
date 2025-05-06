# 20250506
# @author hedskul
# @version 1.0.0

# ------------------------------------------------------------------------------
# Final
# LA - Crime Date 2020-20250503
# Downloaded 20250503
# ------------------------------------------------------------------------------

if (!require(readr)) {
  install.packages(readr);
}
library(readr);
# if (!require(purrr))
#   install.packages(purrr)
# library(purrr)
if (!require(tidyr)) {
  install.packages(tidyr);
}
library(tidyr);
if (!require(dplyr)) {
  install.packages(dplyr);
}
library(dplyr);
# if (!require(ggplot2))
#   install.packages(ggplot2)
# library(ggplot2)

# install.packages("RSocrata")
# earthquakesDataFrame <- read.socrata("https://soda.demo.socrata.com/resource/4334-bgaj.csv")
# nrow(earthquakesDataFrame) # 1007 (two "pages")
# class(earthquakesDataFrame$Datetime[1]) # POSIXlt

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

# # Default API limit exceeded
# # Our API has a default limit of providing 1,000 rows.
# # Learn more about how you can modify the default limit.
# la_crime_data <- read_csv("https://data.lacity.org/resource/2nrs-mtv8.json");

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

# getwd();
# ..
# .
project_wd <- setwd(
  "."
);
project_wd;
# [1] "D:/CURRENT_WORK/STUDY/Data Analytics/Final/DataAnalytics-R-RStudio-LACity-CrimeData2020-"

csv_location <- file.path(
  "data", "Crime_Data_from_2020_to_Present.csv"
);
csv_location;
# [1] "data/Crime_Data_from_2020_to_Present.csv"

la_crime_data <- read_csv(
  csv_location, 
  locale = locale(encoding = "ISO-8859-1"),
  # show_col_types = FALSE
);

?names
# # Functions to get or set the names of an object.
# names(x)

names(la_crime_data);

?slice
# slice {dplyr}
# Subset rows using their positions
# slice(.data, ..., .by = NULL, .preserve = FALSE)

clean_la_crime_data <- la_crime_data |>
  select("DATE OCC", "AREA NAME", "Crm Cd Desc", "Vict Age", "Vict Sex", "LAT", "LON");
head(clean_la_crime_data)

x <- unique(la_crime_data$`Crm Cd`);
unique(la_crime_data$`Crm Cd Desc`);

str(la_crime_data);

?head
# # Return the First or Last Parts of an Object
# head(x, ...)

head(la_crime_data);
# Warning message:                                                                                                                                 
#   One or more parsing issues, call `problems()` on your data frame for details, e.g.:
#   dat <- vroom(...)
#   problems(dat) 

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

# stop_for_problems(la_crime_data);
problem_tibble <- readr::problems(la_crime_data);

?select
# select {dplyr}
# Keep or drop columns using their names and types
# starwars %>% select(!(name:mass));
# # Error in starwars %>% select(!(name:mass)) : 
# #   could not find function "%>%"
# FIX: library(tidyr);

problem_tibble <- problem_tibble %>%
  select(-(file));
head(problem_tibble);

la_crime_data[248, 23];
# # A tibble: 1 × 1
# `Crm Cd 3`
# <lgl>     
#   1 NA  

# sd(na_example, na.rm = TRUE);
# ref <- NHANES |>
#   filter(AgeDecade == " 20-29", Gender == "female", !is.na(BPSysAve)) |>
#   summarize(blood_pressure_mean = mean(BPSysAve), blood_pressure_sd = sd(BPSysAve))

str(la_crime_data);

summary(la_crime_data, na.rm = true);



?read_csv




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
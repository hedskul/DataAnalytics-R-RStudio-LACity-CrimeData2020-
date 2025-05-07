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
# if (!require(devtools)) {
#   install.packages("devtools");
# } 

# # Set a default type
# read_csv(
#   file,
#   col_type = list(.default = col_double())
# )

# # install.packages("RSocrata");
# # # package ‘RSocrata’ is not available for this version of R
# # FIX: devtools::install_github("Chicago/RSocrata");
# # install.packages("devtools");
# devtools::install_github("Chicago/RSocrata");
# # * DONE (RSocrata)
# # Skipping install of 'RSocrata' from a github remote, the SHA1 (c25832cf) has not changed since last install.
# # Use `force = TRUE` to force installation
# library(RSocrata);
# # https://sandbox.demo.socrata.com/OData.svc/nimj-3ivp?$select=magnitude,depth
# # https://sandbox.demo.socrata.com/OData.svc/nimj-3ivp?$filter=magnitude%20gt%205
# print("note the ? and & to pass vars into URL");
# print("note the % encoded to parse later");
# print("note the %20 is space");
# print("Crm Cd = 110 murders");
# # https://support.socrata.com/hc/en-us/articles/115005364207-Access-Data-Insights-Data-using-OData
# query_str <- paste0(
#   "https://data.lacity.org/api/odata/v4/2nrs-mtv8"
#   # "?$select=date_occ,area_name,crm_cd,crm_cd_desc,lat,lon",
#   # "&$where=crm_cd=110",
#   # "&$order=date_occ"
# );
# # non-numeric argument to binary operator
# # FIX: use , not +
# query_str;
# la_public_safety_df <- read.socrata(
#   query_str
# );
# # 2025-05-06 13:27:25.407 getResponse: Error in httr GET: 400
# # https://data.lacity.org/resource/2nrs-mtv8.csv
# # ?%24select=date_occ%2Carea_name%2Ccrm_cd%2Ccrm_cd_desc%2Clat%2Clon
# # &%24where=crm_cd%3D110&$order=:id
# # FIX: order=date_occ
# # Error in getResponse(validUrl, email, password) : Bad Request (HTTP 400).
# 
# # ERROR: could not find function "read.socrata"
# # FIX: library(RSocrata);
# nrow(la_public_safety_df);
# head(la_public_safety_df);

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

names(la_crime_data);

?slice
# slice {dplyr}
# Subset rows using their positions
# slice(.data, ..., .by = NULL, .preserve = FALSE)

str(la_crime_data);


# stop_for_problems(la_crime_data);
problem_tibble <- readr::problems(la_crime_data);
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


head(la_crime_data);
# Warning message:                                                                                                                                 
#   One or more parsing issues, call `problems()` on your data frame for details, e.g.:
#   dat <- vroom(...)
#   problems(dat) 

# select(.data, ...)

str(la_crime_data);
clean_la_crime_data <- la_crime_data |>
  select("DATE OCC", "AREA NAME", "Crm Cd Desc", "Vict Age",
         "Vict Sex", "LAT", "LON", "LOCATION");
head(clean_la_crime_data);
# # A tibble: 6 × 8
# `DATE OCC`             `AREA NAME` `Crm Cd Desc`                            `Vict Age` `Vict Sex`   LAT   LON LOCATION                         
# <chr>                  <chr>       <chr>                                         <dbl> <chr>      <dbl> <dbl> <chr>                            
#   1 03/01/2020 12:00:00 AM Wilshire    VEHICLE - STOLEN                                  0 M           34.0 -118. 1900 S  LONGWOOD                …
# 2 02/08/2020 12:00:00 AM Central     BURGLARY FROM VEHICLE                            47 M           34.0 -118. 1000 S  FLOWER                  …
# 3 11/04/2020 12:00:00 AM Southwest   BIKE - STOLEN                                    19 X           34.0 -118. 1400 W  37TH                    …
# 4 03/10/2020 12:00:00 AM Van Nuys    SHOPLIFTING-GRAND THEFT ($950.01 & OVER)         19 M           34.2 -118. 14000    RIVERSIDE              …
# 5 09/09/2020 12:00:00 AM Hollenbeck  VEHICLE - STOLEN                                  0 NA          34.1 -118. 200 E  AVENUE 28                 
# 6 05/02/2020 12:00:00 AM Rampart     VEHICLE - STOLEN                                  0 NA          34.1 -118. 2500 W  4TH  

# x <- unique(la_crime_data$`Crm Cd`);

print("This dataset reflects incidents of crime in the City of Los Angeles
      dating back to 2020. This data is transcribed from original crime reports
      that are typed on paper and therefore there may be some inaccuracies
      within the data. Some location fields with missing data are
      noted as (0°, 0°). Address fields are only provided to the nearest
      hundred block in order to maintain privacy. This data is as accurate
      as the data in the database. Please note questions or
      concerns in the comments.");



# 
# unique(la_crime_data$"Crm Cd Desc");
# class(str(clean_la_crime_data));
# ?factor
# ?length
# ?levels
# how_many_regions = as.numeric(length(levels(murders$region)))
# print(paste("Factor stores num index instead of large char labels. Num of regions:", how_many_regions))
# print("used as.numeric do to the confusion of L returning with how_many_regions")
# 
# as_tibble(la_crime_data);
# la_crime_data %>% select(where(is.numeric))
# la_crime_data %>% select(where(is.factor))


# ?read_csv




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
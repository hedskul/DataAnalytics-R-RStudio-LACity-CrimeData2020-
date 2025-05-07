# 20250506
# @author hedskul


if (!require(readr))
  install.packages(readr)
library(readr)
if (!require(purrr))
  install.packages(purrr)
library(purrr)
if (!require(readxl))
  install.packages(readxl)
library(readxl)
if (!require(tidyr))
  install.packages(tidyr)
library(tidyr)
if (!require(dplyr))
  install.packages(dplyr)
library(dplyr)
if (!require(ggplot2))
  install.packages(ggplot2)
library(ggplot2)
if (!require(dslabs))
  install.packages(dslabs)
library(dslabs)

if (!require(ISLR)) install.packages("ISLR")
library(ISLR);

# ------------------------------------------------------------------------------
# Final
# LA - Crime Date 2020-Present 
# ------------------------------------------------------------------------------

library(dslabs);
data(murders)
str(murders)
?murders


+ 
  
  # Line  94:   geom_line() + ggtitle("CO2 Concentration") + coord_cartesian(ylim = c(260, 1800))
  # Line  96:   geom_line() + ggtitle("Methane Concentration")+ coord_cartesian(ylim = c(260, 1800))
  # Line  98:   geom_line() + ggtitle("Nitrous Oxide Concentration")+ coord_cartesian(ylim = c(260, 1800))
  
  
  # h + geom_density_2d()
  # x, y, alpha, color, group, linetype, size
  
  
  #   geom_density() +
  
  # 
  # minority_report |>
  #   ggplot(aes(x = height)) +
  #   geom_density();

data(College);
CCRI <- College %>% mutate(PercFull = F.Undergrad / (P.Undergrad + F.Undergrad) * 100,
                           PercPart = (P.Undergrad / (F.Undergrad + P.Undergrad)) * 100,
                           PercAccept = Accept / Apps * 100,
                           PercEnroll = Enroll / Apps * 100) %>%
  dplyr::select(Private, Apps, PercAccept, PercEnroll, Top10perc, Top25perc, PercFull, PercPart,
                Outstate, Room.Board, Books, Personal, PhD, Terminal, S.F.Ratio, perc.alumni, 
                Expend, Grad.Rate);
ggplot(heights, aes(x = height, fill = sex)) +
  geom_density() +
  scale_fill_manual(values = alpha(c("#BADA66", "#666666"), .2));
accept_me_CCRI_fit = lm(PercAccept ~ . -PercPart, data = CCRI);
summary(accept_me_CCRI_fit);
par(mfrow = c(2,2));
plot(accept_me_CCRI_fit);
par(mfrow = c(2,2));
accept_me_completely_CCRI_fit <- lm(PercAccept ~ - PercPart + Outstate  + poly(PercFull,2)
                                    + Private + poly(Top10perc,2) + S.F.Ratio + Books
                                    + Expend + Grad.Rate
                                    + poly(PercEnroll,2), data = CCRI);
summary(accept_me_completely_CCRI_fit);
plot(accept_me_completely_CCRI_fit);


murders_tib <- tibble(murders) |>
  mutate(color = c("purple"))

?case_match
# This function allows you to vectorise multiple switch() statements
# case_when(
#   x %in% c("a", "b") ~ 1,
#   x %in% "c" ~ 2,
#   x %in% c("d", "e") ~ 3
# )
murders_tib$region;
factor(murders_tib$region);

murders_tib <- murders_tib |>
  mutate(color_column = case_when(
    region == "Northeast" ~ "blue",
    region == "South" ~ "red",
    region == "Central" ~ "orange",
    region == "West" ~ "purple",
    region == "North" ~ "green",
    TRUE ~ "Other"
  ))
murders_tib;

murders_color_column_gg <- murders_tib |>
  ggplot(aes(population, total, label = abb, color = color_column)) +
  geom_label();
murders_color_column_gg;

murders %>%
  ggplot(aes(population, total, label = abb, color = region)) +
  geom_label();


# If not already loaded, let’s begin by loading the data:
  
  library(dslabs)
if (!exists("mnist")) mnist <- read_mnist()

murders_tib <- tibble(murders) |>
  mutate(color = c("purple"))

?case_match
# This function allows you to vectorise multiple switch() statements
# case_when(
#   x %in% c("a", "b") ~ 1,
#   x %in% "c" ~ 2,
#   x %in% c("d", "e") ~ 3
# )
murders_tib$region;
factor(murders_tib$region);

murders_tib <- murders_tib |>
  mutate(color_column = case_when(
    region == "Northeast" ~ "blue",
    region == "South" ~ "red",
    region == "Central" ~ "orange",
    region == "West" ~ "purple",
    region == "North" ~ "green",
  ))
murders_tib;

murders_color_column_gg <- murders_tib |>
  ggplot(aes(population, total, label = abb, color = color_column)) +
  geom_label();
murders_color_column_gg;

murders %>%
  ggplot(aes(population, total, label = abb, color = region)) +
  geom_label();


crime_codes <- define_code_group();

define_code_group <- function() {
  homicide_codes <- c(
    110, 113
  );
  rape_codes <- c(
    121, 122,
    815, 820, 821
  );
  robbery_codes <- c(
    210, 220
  );
  DV_codes <- c(236, 625);
  agg_assualt <- c(
    230, 231, 235, 236, 250,
    251, 781, 926
  );
  simple_assualt_codes <- c(
    435, 436, 437,
    622, 623, 624, 625, 626,
    627, 647, 763, 928, 930
  );
  property_codes <- c(
    310, 320,         
    330, 331, 410, 420, 421, 
    341, 343, 345, 440, 442, 443, 444, 445, 
    350, 351, 352, 353,
    450, 451, 452, 453,
    510, 520        
  );
  
  list(
    homicide = homicide_codes,
    rape = rape_codes,
    robbery = robbery_codes,
    DV = DV_codes,
    agg_assault = agg_assault_codes,
    simple_assault = simple_assault_codes,
    property = property_codes
  )
};



# # maps
# # Draw the appropriate geometric object depending on the
# # simple features present in the data. aes() arguments:
# #   map_id, alpha, color, fill, linetype, linewidth.
# # nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))
# ggplot(nc) +
#   geom_sf(aes(fill = AREA))
# # DONE: if exists
# if(!exists("la_crime_data")){
#   la_crime_data <- read_csv(
#     csv_location, 
#     # locale = locale(encoding = "ISO-8859-1"),
#     guess_max = 100000,
#     name_repair = "universal",
#     col_types = cols(
#       LAT = col_double(),
#       LON = col_double()
#       # LAT = col_character(),
#       # LON = col_character()
#     )
#     # show_col_types = FALSE
#   );
# };

#   aes(
#     x = LON, y = LAT, color = AREA.NAME
#   )
# ) +
#   geom_point(
#     alpha = 0.5
#   )

# # minority_report %>% 
#   ggplot(nghbrhd_data_shp) +
#   geom_sf(
#     aes(
#       x = LON, y = LAT, color = AREA.NAME, data = minority_report
#     )
#   );
# Error in `geom_sf()`:
#   ! Problem while computing aesthetics.
# ℹ Error occurred in the 1st layer.
# Caused by error:
#   ! object 'LON' not found
# Run `rlang::last_trace()` to see where the error occurred.
# Warning message:
#   In layer_sf(geom = GeomSf, data = data, mapping = mapping, stat = stat,  :
#                 Ignoring unknown aesthetics: x, y, and data

# murders_color_column_gg <- minority_report %>%
#   ggplot(
# murders_color_column_gg <- minority_report
# ggplot(nc) +
#   geom_sf(aes(fill = AREA))


# arrange()

# group_by_select_la_crime_data <- select_la_crime_data |>
#   group_by(Crm.Cd);
# 
# names(group_by_select_la_crime_data);
# head(group_by_select_la_crime_data);
# summary(group_by_select_la_crime_data);
# nrow(group_by_select_la_crime_data);
# str(group_by_select_la_crime_data);

# filter(AgeDecade == " 20-29", Gender == "Female", !is.na()) |>

# df3 <- read_csv(
#   readr_example("challenge.csv"), 
#   col_types = list(
#     x = col_double(),
#     y = col_date(format = "")
#   )
# )

# vignette("readr");

# col_types
# One of NULL, a cols() specification, or a string. See vignette("readr") for more details.
# 
# If NULL, all column types will be inferred from guess_max rows of the input, interspersed throughout the file. This is convenient (and fast), but not robust. If the guessed types are wrong, you'll need to increase guess_max or supply the correct types yourself.
# 
# Column specifications created by list() or cols() must contain one column specification for each column. If you only want to read a subset of the columns, use cols_only().
# 
# Alternatively, you can use a compact string representation where each character represents one column:



# Default API limit exceeded
# Our API has a default limit of providing 1,000 rows.
# Learn more about how you can modify the default limit.
la_crime_data <- read_csv("https://data.lacity.org/resource/2nrs-mtv8.json");

# install.packages("RSocrata");
# # package ‘RSocrata’ is not available for this version of R
# FIX: devtools::install_github("Chicago/RSocrata");
# install.packages("devtools");
devtools::install_github("Chicago/RSocrata");
# * DONE (RSocrata)
# Skipping install of 'RSocrata' from a github remote, the SHA1 (c25832cf) has not changed since last install.
# Use `force = TRUE` to force installation
library(RSocrata);
# https://sandbox.demo.socrata.com/OData.svc/nimj-3ivp?$select=magnitude,depth
# https://sandbox.demo.socrata.com/OData.svc/nimj-3ivp?$filter=magnitude%20gt%205
print("note the ? and & to pass vars into URL");
print("note the % encoded to parse later");
print("note the %20 is space");
print("Crm Cd = 110 murders");
# https://support.socrata.com/hc/en-us/articles/115005364207-Access-Data-Insights-Data-using-OData
query_str <- paste0(
  "https://data.lacity.org/api/odata/v4/2nrs-mtv8",
  "?$select=date_occ,area_name,crm_cd,crm_cd_desc,lat,lon",
  "&$where=crm_cd=110",
  "&$order=date_occ"
);
# non-numeric argument to binary operator
# FIX: use , not +
query_str;
la_public_safety_df <- read.socrata(
  query_str
);
# 2025-05-06 13:27:25.407 getResponse: Error in httr GET: 400
# https://data.lacity.org/resource/2nrs-mtv8.csv
# ?%24select=date_occ%2Carea_name%2Ccrm_cd%2Ccrm_cd_desc%2Clat%2Clon
# &%24where=crm_cd%3D110&$order=:id
# FIX: order=
# Error in getResponse(validUrl, email, password) : Bad Request (HTTP 400).


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

# islands_df <- data.frame(Area = as.numeric(islands));
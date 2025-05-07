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
if (!require(ggplot2)) {
  install.packages(ggplot2);
}
library(ggplot2);
if (!require(sf)) {
  install.packages(sf);
}
library(sf);
if (!require(ISLR)) {
  install.packages(ISLR);
}
library(ISLR);
# Enter an item from the menu, or 0 to exit
# Selection: 

print("250MB is too big.");
print("need read_csv to filter Crm.Cd == c(110, 133) and download.");
print("dont need population if use area and a heatmap");
print("census data is old and a different dataset");

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

# ?options
# Line 113: # Allow the user to set and examine a variety of global options
# which affect the way in which R computes and displays its results.
#   Line 114: op <- options(digits = 4, width = 90) # for nice formatting
# Line 119: options(op)
options(digits = 10);

# DONE: if exists
if(!exists("la_crime_data")){
  la_crime_data <- read_csv(
    csv_location, 
    # locale = locale(encoding = "ISO-8859-1"),
    guess_max = 100000,
    name_repair = "universal",
    col_types = cols(
      LAT = col_double(),
      LON = col_double()
      # LAT = col_character(),
      # LON = col_character()
    )
    # show_col_types = FALSE
  );
};

print("LAT and LON are coming in without decimal places?");

head(la_crime_data$LAT);
head(la_crime_data$LON);

# islands_df <- data.frame(Area = as.numeric(islands));
# ?as.double
# la_crime_data <- la_crime_data %>%
#   mutate(
#     LAT = as.numeric(LAT),
#     LON = as.numeric(LON)
#   );

# la_crime_data %>%
#   select(
#     LAT, LON
#   ) %>%
#   print();
# # # A tibble: 6 × 2
# # LAT   LON
# # <dbl> <dbl>
# #   1  34.0 -118.
# # 2  34.0 -118.
# # 3  34.0 -118.
# # 4  34.2 -118.
# # 5  34.1 -118.
# # 6  34.1 -118.

head(la_crime_data$LAT);

# ?read_csv
# name_repair 

# names(la_crime_data);
# head(la_crime_data);
# summary(la_crime_data);
# nrow(la_crime_data);
# class(str(la_crime_data));

# https://data.lacity.org/Public-Safety/Crime-Data-from-2020-to-Present/2nrs-mtv8/about_data

# DATE OCC
# MM/DD/YYYY

select_la_crime_data <- la_crime_data |>
  select(DATE.OCC, TIME.OCC, AREA, AREA.NAME,
         Crm.Cd, Crm.Cd.Desc,
         Vict.Age, Vict.Sex, Mocodes,
         Part.1.2,
         LAT, LON, LOCATION);

print("Mocodes = describe suspects");
# Modus Operandi: Activities associated with the suspect in commission
# of the crime.See attached PDF for list of MO Codes in numerical order.
print("Part.1.2 = so many other things possible not reported.");
print("UCR REPORTING – Return A (Based on date of reporting)");
print("COMPSTAT REPORTING - (Based on date of occurrence)");
print("SIMPLE ASSAULT** Not included in Compstat Reports.");

# UCR_Manual.pdf
# Column 1: Classification of Offenses
# The Part I criminal offenses to be scored on the Return A are listed in Column 1.
# (Definitions for Part I offenses can be found in Chapter II.) They include:
#   1. Criminal Homicide
# 2. Rape
# 3. Robbery
# 4. Aggravated Assault
# 5. Burglary
# 6. Larceny-theft
# 7. Motor Vehicle Theft
# NOTE: There is a separate report to submit human trafficking and arson offenses.




# names(select_la_crime_data);
# head(select_la_crime_data);
# summary(select_la_crime_data);
# nrow(select_la_crime_data);
# class(str(select_la_crime_data));

# https://data.lacity.org/Public-Safety/Crime-Data-from-2020-to-Present/2nrs-mtv8/about_data

# MO_CODES_Numerical_20191119.pdf
# UCR REPORTING – Return A
# (Based on date of reporting)
# Part I – Violent Crimes
# HOMICIDE 110 (Homicide)
# 113 (Manslaughter)

print("Do i include manslaughter?");
print("I should find by Crm.Cd and Gun");

print("Crm.Cd.1 and Crm.Cd.2 are charged offenses.");
print("Crm.Cd is crime report");

filter_select_la_crime_data <- select_la_crime_data |>
  filter(
    Crm.Cd %in% c(
      110, 113
    )
  );

# names(filter_select_la_crime_data);
# head(filter_select_la_crime_data);
# summary(filter_select_la_crime_data);
# nrow(filter_select_la_crime_data);
# str(filter_select_la_crime_data);

print("Whats the population?");
print("pin for later. populations and comparing murders to 2010 murders dataset.");
print("LA vs cali, LA vs national");
print("requires a larger data set 2010-2020");
print("which I have to filter before download.");
print("Its so large a browser cant download.");
print("i'll stick with the heatmap or spatial map.");

print("group all crimes: homicide, violent, felony, misdiamenor");


#                 29 KIDNAPPING                                                  910   812
#                 42 KIDNAPPING - GRAND ATTEMPT                                  920   232
#                 49 HUMAN TRAFFICKING - INVOLUNTARY SERVITUDE                   921   118
#                                                           66    910 KIDNAPPING                                                 812
#                                                           67    920 KIDNAPPING - GRAND ATTEMPT                                 232
#                                                           68    921 HUMAN TRAFFICKING - INVOLUNTARY SERVITUDE                  118
# 17 ARSON                                                       648  2568

define_code_group <- function() {
  homicide_code <- c(110);
  manslaughter_code <- c(113);
  rape_codes <- c(
    121, 122,
    815, 820, 821
  );
  robbery_codes <- c(
    210, 220
  );
  DV_codes <- c(236, 625);
  
  agg_assault_codes <- c(
    230, 231, 235, 236, 250,
    251, 781, 926
  );
  simple_assault_codes <- c(
    435, 436, 437,
    622, 623, 624, 625, 626,
    627, 647, 763, 928, 930
  );
  property_codes <- c(
    310, 320,     
    510, 520, 433,
    330, 331, 410, 420, 421, 
    350, 351, 352, 353,
    450, 451, 452, 453,
    341, 343, 345, 440, 441, 442, 443, 444, 445, 
    470, 471, 472, 473, 474, 475, 480, 485, 487, 491
  );
  human_trafficking_code <- c(118);
  arson_code <- c(648);
  
  list(
    homicide = homicide_code,
    manslaughter = manslaughter_code,
    rape = rape_codes,
    robbery = robbery_codes,
    DV = DV_codes,
    agg_assault = agg_assault_codes,
    simple_assault = simple_assault_codes,
    property = property_codes,
    human_trafficking = human_trafficking_code,
    arson = arson_code
  )
};

crime_codes <- define_code_group();

print("Is the database UCR or compstat?");
print("Could define the groups crime betters");

select_la_crime_data <- select_la_crime_data |>
  mutate(
    TypesOfCriminalOffenses = case_when(
      #   x %in% c("a", "b") ~ 1,
      
      Crm.Cd %in% crime_codes$homicide ~ "Homicide",
      Crm.Cd %in% crime_codes$manslaughter ~ "Manslaughter",
      
      Crm.Cd %in% c(
        crime_codes$rape,
        crime_codes$robbery,
        crime_codes$agg_assault
      ) ~ "Violent",
      
      Crm.Cd %in% crime_codes$simple_assault ~ "SimpleAssault",
      
      Crm.Cd %in% crime_codes$arson ~ "Arson",
      Crm.Cd %in% crime_codes$human_trafficking ~ "HumanTrafficking",
      Crm.Cd %in% crime_codes$property ~ "PropertyCrimes",
      
      TRUE ~ "Other"
    )
  );
# ! argument 5 is empty
# FIX: remove ,

print("Whats in Other?");

select_la_crime_data %>%
  filter(TypesOfCriminalOffenses == "Other") %>%
  nrow();
# [1] 889729 WOW
# 265380 now scares me in other ways
# FIX: double check variable names. remove _codes

select_la_crime_data %>%
  count(TypesOfCriminalOffenses);
# # A tibble: 6 × 2
# TypesOfCriminalOffenses      n
# <chr>                    <int>
#   1 Arson                     2568
# 2 Homicide                  1577
# 3 Other                   265380
# 4 PropertyCrimes          467303
# 5 SimpleAssault           154478
# 6 Violent                 113803


select_la_crime_data %>%
  filter(TypesOfCriminalOffenses == "Other") %>%
  count(Crm.Cd.Desc, Crm.Cd, sort = TRUE) %>%
  print(n = 100);
# # A tibble: 76 × 3
# Crm.Cd.Desc                                              Crm.Cd     n
# <chr>                                                     <dbl> <int>
#   1 THEFT OF IDENTITY                                           354 62539
# 2 VANDALISM - FELONY ($400 & OVER, ALL CHURCH VANDALISMS)     740 61094
# 3 VANDALISM - MISDEAMEANOR ($399 OR UNDER)                    745 25381
# 4 TRESPASSING                                                 888 18428
# 5 BRANDISH WEAPON                                             761 14533

print("Whats Part 1-2?");

select_la_crime_data %>%
  filter(Part.1.2 == 2) %>%
  select(Crm.Cd, Crm.Cd.Desc);
# # A tibble: 402,356 × 3
# # Groups:   Part.1.2 [1]
# Part.1.2 Crm.Cd Crm.Cd.Desc  

select_la_crime_data %>%
  filter(Part.1.2 == 2) %>%
  count(Crm.Cd, Crm.Cd.Desc) %>%
  print(n = 200);
# # A tibble: 86 × 3
# Crm.Cd Crm.Cd.Desc                                                  n
# <dbl> <chr>                                                    <int>
#   1    237 CHILD NEGLECT (SEE 300 W.I.C.)                            1205
# 2    347 GRAND THEFT / INSURANCE FRAUD                               12
# 3    349 GRAND THEFT / AUTO REPAIR                                    6
# 4    354 THEFT OF IDENTITY                                        62539
# 5    432 BLOCKING DOOR INDUCTION CENTER                               6
# 6    434 FALSE IMPRISONMENT                                         341
# 7    435 LYNCHING                                                    22
# 8    436 LYNCHING - ATTEMPTED                                        11
# 9    437 RESISTING ARREST                                          1038

print("kidnapping, arson are filtered out in Part.1.2. says should be included else where.");

filter_select_la_crime_data <- select_la_crime_data %>%
  filter(TypesOfCriminalOffenses != "Other");
nrow(filter_select_la_crime_data);
# [1] 739729

print("No human trafficking cases?");
select_la_crime_data %>% 
  count(Crm.Cd == 118);
# 1 FALSE           1005109

print("should i pivot wider with counts for each offense?");
print("stick to plan finding recent crimes by location");

print("homicide and manslaughter are very different");

minority_report <- select_la_crime_data %>%
  filter(TypesOfCriminalOffenses == "Homicide") %>%
  filter(!is.na(LAT) & !is.na(LON));

select_la_crime_data %>%
  filter(TypesOfCriminalOffenses == "Homicide") %>%
  nrow();

select_la_crime_data %>%
  filter(!is.na(LAT) & !is.na(LON)) %>%
  nrow();

print("no LAT and LON are NA?");

select_la_crime_data %>%
  filter(LAT > 1 & LON < -1) %>%
  nrow();
# [1] 1002869

nrow(select_la_crime_data);
# [1] 1005109


select_la_crime_data <- select_la_crime_data %>%
  filter(LAT > 1 & LON < -1);

minority_report <- select_la_crime_data %>%
  filter(TypesOfCriminalOffenses == "Homicide") %>%
  filter(LAT > 1 & LON < -1);


names(minority_report);
head(minority_report);
summary(minority_report);
nrow(minority_report);
class(str(minority_report));

murders_color_column_gg <- minority_report %>%
  ggplot(
    aes(
      x = LON, y = LAT, color = AREA.NAME
    )
  ) +
  geom_point(
    alpha = 0.4
    # color = "red"
  ) +
  labs(
    title = "Homicides X and Y",
    x = "LON",
    y = "LAT"
  );
murders_color_column_gg;
# Error in UseMethod("depth") : 
#   no applicable method for 'depth' applied to an object of class "NULL"
# FIX: Don't use a million rows


# ?geom_density
# # p + scale_fill_manual(values = alpha(c("blue", "red"), .3))
# ggplot(heights, aes(x = height, fill = sex)) +
#   geom_density() +
#   scale_fill_manual(values = alpha(c("#BADA66", "#666666"), .2));
murders_color_column_gg <- minority_report %>%
  ggplot(
    aes(
      x = LON, y = LAT, color = AREA.NAME
    )
  ) +
  geom_point(
    alpha = 0.5,
    # color = "red"
  ) +
  geom_density_2d(color = "black")
murders_color_column_gg;

minority_report <-minority_report %>%
  group_by(AREA.NAME);


murders_color_column_gg <- murders_color_column_gg +
  geom_density_2d();
murders_color_column_gg;

# warnings();
# # Warning messages:
# #   1: `stat_contour()`: Zero contours were generated
# # 2: In min(x) : no non-missing arguments to min; returning Inf
# # 3: In max(x) : no non-missing arguments to max; returning -Inf

print("need to set the range.")

summary(select_la_crime_data$LAT);
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 33.70590 34.01540 34.05920 34.07415 34.16490 34.33430 
summary(select_la_crime_data$LON);
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# -118.6676 -118.4309 -118.3230 -118.3547 -118.2740 -118.1554 

murders_color_column_gg <- murders_color_column_gg +
  geom_density_2d(color = "black")
murders_color_column_gg;
# 
# warnings();
# # 28: `stat_contour()`: Zero contours were generated
# # 29: In min(x) : no non-missing arguments to min; returning Inf
# # 30: In max(x) : no non-missing arguments to max; returning -Inf
# FIX: coord_cartesian(ylim = c(33.70590, 34.33430), xlim = c(-118.6676, -118.1554))




minority_report <- minority_report %>%
  group_by(AREA.NAME);
murders_color_column_gg <- minority_report %>%
  ggplot(
    aes(
      x = LON, y = LAT, color = AREA.NAME
    )
  ) +
  geom_point(
    alpha = 0.5
  ) +
  geom_density_2d(color = "black") +
  # geom_hex(color = "red") +
  # LAT = y
  # LON = x
  coord_cartesian(ylim = c(33.70590, 34.33430), xlim = c(-118.6676, -118.1554))
murders_color_column_gg;




print("intergrate map.");

shape_location <- file.path(
  "shape", "nghbrhd_data.shp"
);
shape_location;
# [1] "shape/nghbrhd_data.shp"
nghbrhd_data_shp <- st_read(shape_location);
# Error: `dsn` must point to a source, not an empty string.
# FIX: removed extra package info


# 
# ?geom_sf
# # data = NULL,
# ?sf
# vignette("sf");
# 
# ??sf
# st_shift_longitude(sfc)
## crs        : +proj=longlat +datum=NAD83 +no_defs 

minority_report %>%
  ggplot(
    aes(
      x = LON, y = LAT, color = AREA.NAME
    )
  ) +
  # ggplot() +
  # geom_sf(data = nghbrhd_data_shp, data = NULL, color = "gray50") +
  # geom_sf(data = nghbrhd_data_shp, color = "gray50") +
  # geom_sf(data = nghbrhd_data_shp, fill = NA, color = "gray50") +
  geom_point(
    # data = minority_report,
    # aes(
    #   x = LON, y = LAT, color = AREA.NAME
    # ),
    alpha = 0.5
  ) +
  geom_density_2d(
    # data = minority_report,
    # aes(x = LON, y = LAT, fill = AREA.NAME),
    color = "black"
  ) +
  coord_cartesian(ylim = c(33.70590, 34.33430), xlim = c(-118.6676, -118.1554)) +
  # coord_sf(ylim = c(33.70590, 34.33430), xlim = c(-118.6676, -118.1554)) +
  labs(
    title = "Homicides X and Y",
    x = "LON",
    y = "LAT"
  );
# Error in `geom_sf()`:
#   ! Problem while converting geom to grob.
# ℹ Error occurred in the 1st layer.
# Caused by error in `draw_panel()`:
#   ! `geom_sf()` can only be used with `coord_sf()`.
# Run `rlang::last_trace()` to see where the error occurred.


print("find recent location. pin point locations.");
print("predict")
# 
# data(College);
# CCRI <- College %>% mutate(PercFull = F.Undergrad / (P.Undergrad + F.Undergrad) * 100,
#                            PercPart = (P.Undergrad / (F.Undergrad + P.Undergrad)) * 100,
#                            PercAccept = Accept / Apps * 100,
#                            PercEnroll = Enroll / Apps * 100) %>%
#   dplyr::select(Private, Apps, PercAccept, PercEnroll, Top10perc, Top25perc, PercFull, PercPart,
#                 Outstate, Room.Board, Books, Personal, PhD, Terminal, S.F.Ratio, perc.alumni, 
#                 Expend, Grad.Rate);
# ggplot(heights, aes(x = height, fill = sex)) +
#   geom_density() +
#   scale_fill_manual(values = alpha(c("#BADA66", "#666666"), .2));
# attach(College);

# select_la_crime_data <- la_crime_data |>
#   select(DATE.OCC, TIME.OCC, AREA, AREA.NAME,
#          Crm.Cd, Crm.Cd.Desc,
#          Vict.Age, Vict.Sex, Mocodes,
#          Part.1.2,
#          LAT, LON, LOCATION);

LAT_fit = lm(LAT ~ LON + AREA, data = minority_report);
LON_fit = lm(LON ~ LAT + AREA, data = minority_report);
# Error in `contrasts<-`(`*tmp*`, value = contr.funs[1 + isOF[nn]]) : 
  # contrasts can be applied only to factors with 2 or more levels
# FIX: removed Crm.Cd
summary(LAT_fit);
summary(LON_fit);

print("cannot allocate vector of size 717.4 Gb");

# data(College);
# CCRI <- College %>% mutate(PercFull = F.Undergrad / (P.Undergrad + F.Undergrad) * 100,
#                            PercPart = (P.Undergrad / (F.Undergrad + P.Undergrad)) * 100,
#                            PercAccept = Accept / Apps * 100,
#                            PercEnroll = Enroll / Apps * 100) %>%
#   dplyr::select(Private, Apps, PercAccept, PercEnroll, Top10perc, Top25perc, PercFull, PercPart,
#                 Outstate, Room.Board, Books, Personal, PhD, Terminal, S.F.Ratio, perc.alumni, 
#                 Expend, Grad.Rate);
# ggplot(heights, aes(x = height, fill = sex)) +
#   geom_density() +
#   scale_fill_manual(values = alpha(c("#BADA66", "#666666"), .2));
# accept_me_CCRI_fit = lm(PercAccept ~ . -PercPart, data = CCRI);
# summary(accept_me_CCRI_fit);
# par(mfrow = c(2,2));
# plot(accept_me_CCRI_fit);
# par(mfrow = c(2,2));
# accept_me_completely_CCRI_fit <- lm(PercAccept ~ - PercPart + Outstate  + poly(PercFull,2)
#                                     + Private + poly(Top10perc,2) + S.F.Ratio + Books
#                                     + Expend + Grad.Rate
#                                     + poly(PercEnroll,2), data = CCRI);
# summary(accept_me_completely_CCRI_fit);
# plot(accept_me_completely_CCRI_fit);

# Error: cannot allocate vector of size 717.4 Gb
# Error in `contrasts<-`(`*tmp*`, value = contr.funs[1 + isOF[nn]]) : 
#   contrasts can be applied only to factors with 2 or more levels


# summary(accept_me_CCRI_fit);
# par(mfrow = c(2,2));
# plot(accept_me_CCRI_fit);
# par(mfrow = c(2,2));
# accept_me_completely_CCRI_fit <- lm(PercAccept ~ - PercPart + Outstate  + poly(PercFull,2)
#                                     + Private + poly(Top10perc,2) + S.F.Ratio + Books
#                                     + Expend + Grad.Rate
#                                     + poly(PercEnroll,2), data = CCRI);
# summary(accept_me_completely_CCRI_fit);
# plot(accept_me_completely_CCRI_fit);


  
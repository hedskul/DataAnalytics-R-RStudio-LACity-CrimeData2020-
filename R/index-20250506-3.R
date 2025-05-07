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

# # maps
# # Draw the appropriate geometric object depending on the
# # simple features present in the data. aes() arguments:
# #   map_id, alpha, color, fill, linetype, linewidth.
# # nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))
# ggplot(nc) +
#   geom_sf(aes(fill = AREA))



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


print("find recent location. pin point locations.");
print("intergrate map.");


# ?geom_density
# # p + scale_fill_manual(values = alpha(c("blue", "red"), .3))
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

# murders_tib <- tibble(murders) |>
#   mutate(color = c("purple"))
# 
# ?case_match
# # This function allows you to vectorise multiple switch() statements
# # case_when(
# #   x %in% c("a", "b") ~ 1,
# #   x %in% "c" ~ 2,
# #   x %in% c("d", "e") ~ 3
# # )
# murders_tib$region;
# factor(murders_tib$region);
# 
# murders_tib <- murders_tib |>
#   mutate(color_column = case_when(
#     region == "Northeast" ~ "blue",
#     region == "South" ~ "red",
#     region == "Central" ~ "orange",
#     region == "West" ~ "purple",
#     region == "North" ~ "green",
#     TRUE ~ "Other"
#   ))
# murders_tib;
# 
# murders_color_column_gg <- murders_tib |>
#   ggplot(aes(population, total, label = abb, color = color_column)) +
#   geom_label();
# murders_color_column_gg;
# 
# murders %>%
#   ggplot(aes(population, total, label = abb, color = region)) +
#   geom_label();
  

#   HOMICIDE 110 (Homicide)
# 113 (Manslaughter)
# RAPE 121 (Rape)
# 122 (Attempted Rape)
# 815 (Sexual Penetration w/ Foreign Object)
# 820 (Oral Copulation)
# 821 (Sodomy)
# ROBBERY 210 (Robbery)
# 220 (Robbery - attempted)
# AGG. ASSAULTS 230 (ADW),
# 231 (ADW against LAPD Police Officer)
# 235 (Child beating)
# DV* 236 (Spousal beating)
# 250 (Shots Fired)
# 251 (Shots fired inhabited dwelling)
# 761 (Brandishing)
# 926 (Train Wrecking)
# AGG. ASSAULTS 230 (ADW)
# 231 (ADW against LAPD Police Officer)
# 235 (Child beating)
# DV* 236 (Spousal beating)
# 250 (Shots Fired)
# 251 (Shots fired inhabited dwelling)
# 761 (Brandishing)
# 926 (Train Wrecking)
# SIMPLE ASSAULT** 435 (Lynching)
# (Not Incl. in Part I 436 (Lynching - attempted)
#   Violent Crime) 437 (Resisting Arrest)
# 622 (Battery on Firefighter)
# 623 (Battery on Police Officer)
# 624 (Battery - misdemeanor)
# 625 (Other Misd. Assault)
# DV* 626 (Spousal/Cohab Abuse - Simple Assault)
# 627 (Child Abuse - Simple Assault)
# 647 (Throwing substance at vehicle)
# 763 (Stalking)
# 928 (Threatening Phone Calls / Letters)
# 930 (Criminal Threats)
# ASSAULTS = Agg. Assault (Incl. Domestic Viol) + Other Assault
# Part I – Property Crimes
# BURGLARY 310 (Burglary)
# 320 (Burglary - attempted)
# MVT (GTA)*** 510 (Stolen Vehicle)
# 520 (Stolen Vehicle - attempted)
# 433 DWOC
# BTFV 330 (Burg from Vehicle)
# 331 (Theft from vehicle - $950.01 & over)
# 410 (Burg from Vehicle - attempted)
# 420 (Theft from vehicle - $950 & under)
# 421 (Theft from vehicle attempted)
# PERSONAL THFT 350 (Theft from person)
# 351 (Pursesnatch)
# 352 (Pickpocket)
# 353 (Drunkroll)
# 450 (Theft from person - attempted)
# 451 (Pursesnatch - attempted)
# 452 (Pickpocket - attempted)
# 453 (Drunkroll - attempted)
# OTHER THEFT 341 (Theft - $950.01 & over)
# 343 (Shoplifting - $950.01 & over)
# 345 (Dishonest employee - grand theft)
# 440 (Theft - $950 & under)
# 441 (Theft - attempted)
# 442 (Shoplifting - $950 & under)
# 443 (Shoplifting - attempted)
# 444 (Dishonest employee - petty theft)
# 445 (Dishonest Employee - attempted)
# 470 (Till Tap - $950.01 & over)
# 471 (Till Tap - $950 & under)
# 472 (Till Tap - attempted)
# 473 (Theft from coin m/c - $950.01 & over)
# 474 (Theft from coin m/c - $950 & under)
# 475 (Theft from coin m/c - attempted)
# 480 (Bicycle - stolen)
# 485 (Bicycle - attempted stolen)
# 487 (Boat - stolen)
# 491 (Boat - attempted stolen)

# Crm.Cd.Desc                                              Crm.Cd     n
# <chr>                                                     <dbl> <int>
#   1 THEFT OF IDENTITY                                           354 62539
# 2 VANDALISM - FELONY ($400 & OVER, ALL CHURCH VANDALISMS)     740 61094
# 3 VANDALISM - MISDEAMEANOR ($399 OR UNDER)                    745 25381
# 4 TRESPASSING                                                 888 18428
# 5 BRANDISH WEAPON                                             761 14533
# 6 VIOLATION OF RESTRAINING ORDER                              901 11748
# 7 LETTERS, LEWD  -  TELEPHONE CALLS, LEWD                     956  8712
# 8 OTHER MISCELLANEOUS CRIME                                   946  6967
# 9 VIOLATION OF COURT ORDER                                    900  6381
# 10 BUNCO, GRAND THEFT                                          662  6185
# 11 EMBEZZLEMENT, GRAND THEFT ($950.01 & OVER)                  668  4178
# 12 BATTERY WITH SEXUAL CONTACT                                 860  4164
# 13 DOCUMENT FORGERY / STOLEN FELONY                            649  3373
# 14 VEHICLE, STOLEN - OTHER (MOTORIZED SCOOTERS, BIKES, ETC)    522  2969
# 15 DISCHARGE FIREARMS/SHOTS FIRED                              753  2725
# 16 CONTEMPT OF COURT                                           903  2722
# 17 ARSON                                                       648  2568
# 18 EXTORTION                                                   940  2033
# 19 BUNCO, PETTY THEFT                                          664  2027
# 20 CRM AGNST CHLD (13 OR UNDER) (14-15 & SUSP 10 YRS OLDER)    812  1771
# 21 FAILURE TO YIELD                                            890  1714
# 22 DISTURBING THE PEACE                                        886  1514
# 23 CHILD NEGLECT (SEE 300 W.I.C.)                              237  1205
# 24 INDECENT EXPOSURE                                           850  1202
# 25 SEX,UNLAWFUL(INC MUTUAL CONSENT, PENETRATION W/ FRGN OBJ    810  1081
#                 26 CHILD ANNOYING (17YRS & UNDER)                              813  1049
#                 27 SEX OFFENDER REGISTRANT OUT OF COMPLIANCE                   845   966
#                 28 VIOLATION OF TEMPORARY RESTRAINING ORDER                    902   921
#                 29 KIDNAPPING                                                  910   812
#                 30 LEWD CONDUCT                                                762   714
#                 31 UNAUTHORIZED COMPUTER ACCESS                                661   523
#                 32 HUMAN TRAFFICKING - COMMERCIAL SEX ACTS                     822   479
#                 33 BOMB SCARE                                                  755   471
#                 34 CHILD STEALING                                              922   459
#                 35 PEEPING TOM                                                 932   381
#                 36 FALSE IMPRISONMENT                                          434   341
#                 37 DEFRAUDING INNKEEPER/THEFT OF SERVICES, $950 & UNDER        951   337
#                 38 BUNCO, ATTEMPT                                              666   324
#                 39 RECKLESS DRIVING                                            438   284
#                 40 CHILD PORNOGRAPHY                                           814   276
#                 41 CRUELTY TO ANIMALS                                          943   269
#                 42 KIDNAPPING - GRAND ATTEMPT                                  920   232
#                 43 PROWLER                                                     933   202
#                 44 FALSE POLICE REPORT                                         439   182
#                 45 EMBEZZLEMENT, PETTY THEFT ($950 & UNDER)                    670   150
#                 46 PANDERING                                                   806   141
#                 47 PIMPING                                                     805   141
#                 48 COUNTERFEIT                                                 660   119
#                 49 HUMAN TRAFFICKING - INVOLUNTARY SERVITUDE                   921   118
#                 50 CREDIT CARDS, FRAUD USE ($950.01 & OVER)                    653   111
#                 51 ILLEGAL DUMPING                                             949    95
#                 52 CREDIT CARDS, FRAUD USE ($950 & UNDER                       654    92
#                                             53 LEWD/LASCIVIOUS ACTS WITH CHILD                             760    87
#                                             54 DEFRAUDING INNKEEPER/THEFT OF SERVICES, OVER $950.01        950    78
#                                             55 DOCUMENT WORTHLESS ($200.01 & OVER)                         651    73
#                                             56 CONTRIBUTING                                                954    45
#                                             57 WEAPONS POSSESSION/BOMBING                                  756    41
#                                             58 DOCUMENT WORTHLESS ($200 & UNDER)                           652    27
#                                             59 CHILD ABANDONMENT                                           870    26
#                                             60 CONSPIRACY                                                  944    24
#                                             61 INCITING A RIOT                                             882    22
#                                             62 DISRUPT SCHOOL                                              880    13
#                                             63 REPLICA FIREARMS(SALE,DISPLAY,MANUFACTURE OR DISTRIBUTE)    931    13
#                                             64 DRUGS, TO A MINOR                                           865    12
#                                             65 GRAND THEFT / INSURANCE FRAUD                               347    12
#                                             66 BEASTIALITY, CRIME AGAINST NATURE SEXUAL ASSLT WITH ANIM    840     9
#                                             67 BRIBERY                                                     942     8
#                                             68 PETTY THEFT - AUTO REPAIR                                   446     8
#                                             69 BIGAMY                                                      948     7
#                                             70 TELEPHONE PROPERTY - DAMAGE                                 924     7
#                                             71 BLOCKING DOOR INDUCTION CENTER                              432     6
#                                             72 FAILURE TO DISPERSE                                         884     6
#                                             73 GRAND THEFT / AUTO REPAIR                                   349     6
#                                             74 INCEST (SEXUAL ACTS BETWEEN BLOOD RELATIVES)                830     6
#                                             75 FIREARMS EMERGENCY PROTECTIVE ORDER (FIREARMS EPO)          904     5
#                                             76 FIREARMS RESTRAINING ORDER (FIREARMS RO)                    906     4


# Part.1.2 - 

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
# 10    438 RECKLESS DRIVING                                           284
# 11    439 FALSE POLICE REPORT                                        182
# 12    446 PETTY THEFT - AUTO REPAIR                                    8
# 13    622 BATTERY ON A FIREFIGHTER                                   257
# 14    623 BATTERY POLICE (SIMPLE)                                   2565
# 15    624 BATTERY - SIMPLE ASSAULT                                 74842
# 16    625 OTHER ASSAULT                                             4246
# 17    626 INTIMATE PARTNER - SIMPLE ASSAULT                        46712
# 18    627 CHILD ABUSE (PHYSICAL) - SIMPLE ASSAULT                   3558
# 19    647 THROWING OBJECT AT MOVING VEHICLE                          791
# 20    649 DOCUMENT FORGERY / STOLEN FELONY                          3373
# 21    651 DOCUMENT WORTHLESS ($200.01 & OVER)                         73
# 22    652 DOCUMENT WORTHLESS ($200 & UNDER)                           27
# 23    653 CREDIT CARDS, FRAUD USE ($950.01 & OVER)                   111
# 24    654 CREDIT CARDS, FRAUD USE ($950 & UNDER                       92
#                                    25    660 COUNTERFEIT                                                119
#                                    26    661 UNAUTHORIZED COMPUTER ACCESS                               523
#                                    27    662 BUNCO, GRAND THEFT                                        6185
#                                    28    664 BUNCO, PETTY THEFT                                        2027
#                                    29    666 BUNCO, ATTEMPT                                             324
#                                    30    668 EMBEZZLEMENT, GRAND THEFT ($950.01 & OVER)                4178
#                                    31    670 EMBEZZLEMENT, PETTY THEFT ($950 & UNDER)                   150
#                                    32    740 VANDALISM - FELONY ($400 & OVER, ALL CHURCH VANDALISMS)  61094
#                                    33    745 VANDALISM - MISDEAMEANOR ($399 OR UNDER)                 25381
#                                    34    753 DISCHARGE FIREARMS/SHOTS FIRED                            2725
#                                    35    755 BOMB SCARE                                                 471
#                                    36    756 WEAPONS POSSESSION/BOMBING                                  41
#                                    37    760 LEWD/LASCIVIOUS ACTS WITH CHILD                             87
#                                    38    762 LEWD CONDUCT                                               714
#                                    39    763 STALKING                                                   666
#                                    40    805 PIMPING                                                    141
#                                    41    806 PANDERING                                                  141
#                                    42    810 SEX,UNLAWFUL(INC MUTUAL CONSENT, PENETRATION W/ FRGN OBJ  1081
#                                                           43    812 CRM AGNST CHLD (13 OR UNDER) (14-15 & SUSP 10 YRS OLDER)  1771
#                                                           44    813 CHILD ANNOYING (17YRS & UNDER)                            1049
#                                                           45    814 CHILD PORNOGRAPHY                                          276
#                                                           46    822 HUMAN TRAFFICKING - COMMERCIAL SEX ACTS                    479
#                                                           47    830 INCEST (SEXUAL ACTS BETWEEN BLOOD RELATIVES)                 6
#                                                           48    840 BEASTIALITY, CRIME AGAINST NATURE SEXUAL ASSLT WITH ANIM     9
#                                                           49    845 SEX OFFENDER REGISTRANT OUT OF COMPLIANCE                  966
#                                                           50    850 INDECENT EXPOSURE                                         1202
#                                                           51    860 BATTERY WITH SEXUAL CONTACT                               4164
#                                                           52    865 DRUGS, TO A MINOR                                           12
#                                                           53    870 CHILD ABANDONMENT                                           26
#                                                           54    880 DISRUPT SCHOOL                                              13
#                                                           55    882 INCITING A RIOT                                             22
#                                                           56    884 FAILURE TO DISPERSE                                          6
#                                                           57    886 DISTURBING THE PEACE                                      1514
#                                                           58    888 TRESPASSING                                              18428
#                                                           59    890 FAILURE TO YIELD                                          1714
#                                                           60    900 VIOLATION OF COURT ORDER                                  6381
#                                                           61    901 VIOLATION OF RESTRAINING ORDER                           11748
#                                                           62    902 VIOLATION OF TEMPORARY RESTRAINING ORDER                   921
#                                                           63    903 CONTEMPT OF COURT                                         2722
#                                                           64    904 FIREARMS EMERGENCY PROTECTIVE ORDER (FIREARMS EPO)           5
#                                                           65    906 FIREARMS RESTRAINING ORDER (FIREARMS RO)                     4

#                                                           66    910 KIDNAPPING                                                 812
#                                                           67    920 KIDNAPPING - GRAND ATTEMPT                                 232
#                                                           68    921 HUMAN TRAFFICKING - INVOLUNTARY SERVITUDE                  118
#                                                           69    922 CHILD STEALING                                             459
#                                                           70    924 TELEPHONE PROPERTY - DAMAGE                                  7
#                                                           71    928 THREATENING PHONE CALLS/LETTERS                            483
#                                                           72    930 CRIMINAL THREATS - NO WEAPON DISPLAYED                   19287
#                                                           73    931 REPLICA FIREARMS(SALE,DISPLAY,MANUFACTURE OR DISTRIBUTE)    13
#                                                           74    932 PEEPING TOM                                                381
#                                                           75    933 PROWLER                                                    202
#                                                           76    940 EXTORTION                                                 2033
#                                                           77    942 BRIBERY                                                      8
#                                                           78    943 CRUELTY TO ANIMALS                                         269
#                                                           79    944 CONSPIRACY                                                  24
#                                                           80    946 OTHER MISCELLANEOUS CRIME                                 6967
#                                                           81    948 BIGAMY                                                       7
#                                                           82    949 ILLEGAL DUMPING                                             95
#                                                           83    950 DEFRAUDING INNKEEPER/THEFT OF SERVICES, OVER $950.01        78
#                                                           84    951 DEFRAUDING INNKEEPER/THEFT OF SERVICES, $950 & UNDER       337
#                                                           85    954 CONTRIBUTING                                                45
#                                                           86    956 LETTERS, LEWD  -  TELEPHONE CALLS, LEWD                   8712

  
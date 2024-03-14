
#--Setup--

library(readxl)
library(tidyverse)
library(tidycensus)
library(stringr)
library(tidyr)
library(leaflet)
library(lubridate)
library(sf)
# library(rethnicity)
library(shiny)
library(shinythemes)
library(shinyjs)
library(janitor) 
library(tidygeocoder)

# 
# #----NEW - Read Data----
# 
# raw_docket <- read_xlsx('raw/Act-135-Cases_2024.02.07.xlsx')
# raw_opa <- read.csv('raw2/opa_properties_public.csv', colClasses = c("parcel_number" = "character"))
# 
# docket_2 <- raw_docket %>%
#   filter(!is.na(opanum))
# 
# # Merge with OPA Data
# 
# docket_2 <- docket_2 %>%
#   left_join(raw_opa, by = c("opanum" = "parcel_number"))
# 
# docket_3 <- docket_2
# 
# # Separate out coordinates
# 
# # Remove "SRID=2272;POINT (" from the beginning of each string
# docket_3$shape <- gsub("^SRID=\\d+;POINT  \\(\\ s*", "", docket_3$shape)
# 
# # Remove trailing ")"
# docket_3$shape <- gsub("\\)", "", docket_3$shape)
# 
# # Split the remaining string into latitude and longitude
# docket_3 <- cbind(docket_3, do.call(rbind, strsplit(docket_3$shape, " ")))
# 
# # Rename the columns
# colnames(docket_3)[91:92] <- c("Latitude", "Longitude")
# 
# # Convert to numeric
# docket_3[, c("Latitude", "Longitude")] <- sapply(docket_3[, c("Latitude", "Longitude")], as.double)
# 
# # Remove the original shape column
# docket_3 <- docket_3 %>%
#   select(-c("shape"))
# 
# # Deal with non-matches
# 
# docket_missing_coords <- docket_3 %>%
#   filter(is.na(Latitude)) %>%
#   select(-c("Latitude", "Longitude"))
# 
# docket_missing_coords$City <- "Philadelphia"
# docket_missing_coords$State <- "PA"
# 
# docket_missing_coords[49,10] = "323 W GIRARD AVE"
# docket_missing_coords[27,10] = "1641 N 2ND STREET"
# docket_missing_coords[28,10] = "830 N 4TH STREET"
# docket_missing_coords[36,10] = "930 N 2ND STREET"
# docket_missing_coords[1,10] = "753 S 8TH STREET"
# docket_missing_coords[39,10] = "930 N 2ND STREET"
# 
# docket_geocode <- docket_missing_coords %>%
#   geocode(method = "census", street = address, city= City, state = State, lat = latitude, long = longitude) %>%
#   rename(c("Latitude" = "latitude", "Longitude" = "longitude")) %>%
#   select(-c("City", "State"))
# 
# docket_geocode2 <- docket_geocode %>%
#   st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
# 
# # Work with matched data
# docket_with_coords <- docket_3 %>%
#   filter(!is.na(Latitude))
# 
# docket_4 <- docket_with_coords %>%
#   st_as_sf(coords = c("Latitude", "Longitude"), crs = 2272)
# 
# docket_4 <- docket_4 %>%
#   st_transform(4326)
# 
# # FINALLY!
# 
# geocoded_docket_opa <- docket_4 %>%
#   rbind(docket_geocode2) %>%
#   separate(SING_RESP, ",", into = c("LAST", "FIRST"), remove = FALSE) %>%
#   mutate(FIRST = trimws(FIRST),
#          LAST = trimws(LAST))
# 
# geocoded_docket_opa_withnames <- geocoded_docket_opa %>%
#   filter(!is.na(FIRST)) %>%
#   distinct(SING_RESP, .keep_all = TRUE) %>%
#   distinct(opanum, .keep_all = TRUE)
# 
# ##-----NAME ANALYSIS -----
# 
# geocoded_docket_opa_withnames[48,7] <- "O'MALLEY"
# geocoded_docket_opa_withnames[177,7] <- "O'CONNELL"
# geocoded_docket_opa_withnames[368,7] <- "KELLAM"
# geocoded_docket_opa_withnames[100,7] <- "MICHAEL"
# geocoded_docket_opa_withnames[117,7] <- "HOWARD"
# 
# # get rethnicity predictions:
# lastnames <- geocoded_docket_opa_withnames$LAST
# firstnames <- geocoded_docket_opa_withnames$FIRST
# predicted_eths <<- predict_ethnicity(firstnames = firstnames, lastnames = lastnames,
#                                      method = "fullname")
# 
# predicted_eths <- predicted_eths %>%
#   rename(c("FIRST" = "firstname", "LAST" = "lastname"))
# 
# # join back together!
# all_properties_geocoded <- geocoded_docket_opa %>%
#   left_join(predicted_eths, by = c("FIRST", "LAST"))
# 
# # get a count of how many names are paired with an LLC, have multiple resp, etc.
# 
# #-----Clean up Act 135 Property Data and Add Flags-----
# 
# #Human vs non-human
# all_properties_geocoded_flags <- all_properties_geocoded %>%
#   mutate(resp_entity = case_when(
#     grepl("CITY|LAND\040BANK|REDEVELOPMENT|AUTHORITY", respondent) ~ "city",
#     grepl("HEIR|ESTATE\040OF|EXECUTRIX|ESTATES", respondent) ~ "heir",
#     grepl("LLC|HOLDINGS|REALTY|LP$|PROPERTIES|CORPORATION|SCIOLI-TURCO|DEVELOPERS|COMPANY|METAL|INTEGRAL|GROUP|INVESTMENT|AVDC"
#                                        , respondent) ~ "corporate",
#                            grepl("CEMETARY|CHURCH|MUSLIM|PRESBY|MINISTRY|MINISTRIES", respondent) ~ "religious",
#                            grepl("INC$|FOUNDATION|CHARITIES|GUILD", respondent) ~ "non-profit",
#                            TRUE ~ "person"),
#     city = ifelse(grepl("CITY|LAND\040BANK|REDEVELOPMENT|AUTHORITY", respondent),1,0),
#     heir = ifelse(grepl("HEIR|ESTATE\040OF|EXECUTRIX|ESTATES", respondent),1,0),
#     corporate = ifelse(grepl("LLC|HOLDINGS|REALTY|LP$|PROPERTIES|CORPORATION|SCIOLI-TURCO|DEVELOPERS|COMPANY|METAL|INTEGRAL|GROUP|INVESTMENT|AVDC"
#                              , respondent), 1, 0),
#     religious = ifelse(grepl("CEMETARY|CHURCH|MUSLIM|PRESBY|MINISTRY|MINISTRIES", respondent),1,0),
#     nonprof = ifelse(grepl("INC$|FOUNDATION|CHARITIES|GUILD", respondent),1,0),
#     resp_human = ifelse(!is.na(FIRST),TRUE,FALSE),
#     person_and_other = ifelse(resp_human == 1 & (city == 1 | corporate ==1 | religious ==1 | nonprof ==1),
#                               1, 0))
# 
# st_write(all_properties_geocoded_flags, "processed/all_properties_geocoded_flags.geojson")

#----START HERE!----

all_properties_geocoded_flags <- st_read("processed/all_properties_geocoded_flags.geojson")

props_with_names <- all_properties_geocoded_flags %>%
  filter(resp_human)

#-----Prep for human vs nonhuman-----

human_properties_distinct_ppl <- all_properties_geocoded_flags %>%
  filter(resp_human & !is.na(race))

#-----Read data-----

#Neighborhoods 

# neighborhoods <- geojson_read('https://raw.githubusercontent.com/azavea/geo-data/master/Neighborhoods_Philadelphia/Neighborhoods_Philadelphia.geojson', what = "sp")
# neighborhoods_fort <- st_as_sf(neighborhoods)

cds <- st_read('raw/Council_Districts_2024.geojson')

bgs21 <- st_read("raw/bgs.geojson")

#Displacement Risk Ratio
drr_raw <- read.csv("raw/drr_bg_apr23.csv", colClasses=c("geoid"='character')) %>%
  rename("GEOID"="geoid") %>%
  select("GEOID", 
         "DRR1516", "DRR1516C",
         "DRR2122", "DRR2122C")

#-----create html labels for mapping------

content <- paste("<p> Address:", all_properties_geocoded_flags$address, "<br>",
                 "OPA Number:", all_properties_geocoded_flags$opanum, "<br>",
                 "Petitioner:", all_properties_geocoded_flags$petitioner, "<br>",
                 "Respondent:", all_properties_geocoded_flags$respondent, "<br>",
                 "Respondent Entity Type:", all_properties_geocoded_flags$resp_entity, "<br>",
                 "# of Properties:", all_properties_geocoded_flags$number_props, "<br>") %>%
  lapply(htmltools::HTML)

content2 <- paste("<p> Address:", all_properties_geocoded_flags$address, "<br>",
                 "OPA Number:", all_properties_geocoded_flags$opanum, "<br>",
                 "Petitioner:", all_properties_geocoded_flags$petitioner, "<br>",
                 "Respondent:", all_properties_geocoded_flags$respondent, "<br>",
                 "Respondent Entity Type:", all_properties_geocoded_flags$resp_entity, "<br>",
                 "# of Properties:", all_properties_geocoded_flags$number_props, "<br>",
                 "Predicted Ethnicity:", all_properties_geocoded_flags$race) %>%
  lapply(htmltools::HTML)


bgs21$DRR2122C <- factor(bgs21$DRR2122C, ordered = TRUE, levels = c("Insufficient Data",
                                                                    "Below Area Average",
                                                                    "0.0 - 0.5",
                                                                    "0.5 - 1.0",
                                                                    "1.0 - 1.5",
                                                                    "1.5 - 2.0",
                                                                    "2.0 - 2.5",
                                                                    "2.5 - 3.0",
                                                                    "3.0 or Above"
                                                                    ))

bgs21$DRR1516C <- factor(bgs21$DRR1516C, ordered = TRUE, levels = c("Insufficient Data",
                                                                    "Below Area Average",
                                                                    "0.0 - 0.5",
                                                                    "0.5 - 1.0",
                                                                    "1.0 - 1.5",
                                                                    "1.5 - 2.0",
                                                                    "2.0 - 2.5",
                                                                    "2.5 - 3.0",
                                                                    "3.0 or Above"
))

all_properties_geocoded_flags <- all_properties_geocoded_flags %>%
  mutate(resp_human = ifelse(resp_human, "Human", "Non-human"))


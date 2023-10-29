
#--Setup--

library(readxl)
library(tidyverse)
library(tidycensus)
library(stringr)
library(tidyr)
library(leaflet)
library(lubridate)
library(sf)
library(geojsonio)
library(rethnicity)
library(shiny)
library(shinythemes)
library(shinyjs)
library(janitor) 

#-----Read data-----

#Geocoded properties
all_properties_geocoded3 <- read_xlsx('raw/Act135Properties_geocodio.xlsx') %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) 

#Cases
#Distinct OPA number may keep out a few...
act135_cases <- read_xlsx('raw/Cases With Final Disposition.xlsx') %>%
  group_by(opanum) %>%
  mutate(opanum_count = n()) %>%
  ungroup() 

#Neighborhoods 
neighborhoods <- geojson_read('https://raw.githubusercontent.com/azavea/geo-data/master/Neighborhoods_Philadelphia/Neighborhoods_Philadelphia.geojson', what = "sp")
neighborhoods_fort <- st_as_sf(neighborhoods)

bgs21 <- st_read("raw/bgs.geojson")

#Displacement Risk Ratio
drr_raw <- read.csv("raw/drr_bg_apr23.csv", colClasses=c("geoid"='character')) %>%
  rename("GEOID"="geoid") %>%
  select("GEOID", 
         "DRR1516", "DRR1516C",
         "DRR2122", "DRR2122C")

##-----NAME ANALYSIS -----

#Names - Noah's initial list
names <- read_xlsx('raw/noah_names.xlsx') %>%
  select(c(docketnum,respondent_first,respondent_last)) %>%
  left_join(act135_cases, by = "docketnum") %>%
  select(c(docketnum,opanum,respondent_first,respondent_last,respondent))

#get rethnicity predictions:
lastnames <- names$respondent_last
firstnames <- names$respondent_first
predicted_eths <<- predict_ethnicity(firstnames = firstnames, lastnames = lastnames,
                                     method = "fullname")

#join back together
names <- names %>%
  left_join(predicted_eths, by = c("respondent_first" = "firstname", "respondent_last" = "lastname"),
            relationship = "many-to-many") %>%
  distinct(respondent_first, respondent_last, .keep_all = TRUE) %>%
  select(docketnum, race, respondent_first, respondent_last, respondent, prob_asian,
         prob_black, prob_white, prob_hispanic)

#Names that were missing from Noah's initial dataset
discrepancies <- read_xlsx('raw/discrepancies.xlsx') %>%
  filter(!is.na(respondent_first)) %>%
  distinct(respondent_first, respondent_last, .keep_all = TRUE)

lastnames2 <- discrepancies$respondent_last
firstnames2 <- discrepancies$respondent_first
predicted_eths2 <<- predict_ethnicity(firstnames = firstnames2, lastnames = lastnames2,
                                     method = "fullname")

#Merge predictions back together with name data
discrepancies <- discrepancies %>%
  left_join(predicted_eths2, by = c("respondent_first" = "firstname", "respondent_last" = "lastname")) %>%
  mutate(opanum = as.character(opanum)) %>%
  filter(!is.na(race)) %>%
  distinct(respondent_first, respondent_last, .keep_all = TRUE) %>%
  select(respondent, opanum, race, respondent_first, respondent_last,`Nature of error`, prob_asian,
         prob_black, prob_white, prob_hispanic)

#put together final dataset
all_properties_geocoded <- all_properties_geocoded3 %>%
  #apg3 = 524 records 
  #(more than act135 because I separated out properties with multiple addresses)
  
  #remove ACS variables
  select(-contains("ACS")) %>%
  
  #there are 577 act135 cases
  #joining geocoded addresses with the act135 filings based on address match
  left_join(act135_cases, by = c("RecordMatch"="address"), relationship = "many-to-many") %>%
 
  #when we join names, it takes us to 585 (some duplicates where there are diff respondent names)
  #names has 381
  left_join(names, by = "docketnum", relationship = "many-to-many") %>%
  #and 382 join on a docketnum
  
  #discrepancies has 19
  left_join(discrepancies, by = "opanum", relationship = "many-to-many") %>%
    #and 18 join on an opanum
  
  #fix some variable issues that come up in the join
  mutate(respondent = coalesce(respondent, respondent.x, respondent.y)) %>%
  select(-respondent.x, -respondent.y)%>%
  rename(c("race" = "race.x")) %>%
  mutate(race = coalesce(race, race.y),
         race = str_to_title(race),
         respondent_first = coalesce(respondent_first.x, respondent_first.y),
         respondent_last = coalesce(respondent_last.x,respondent_last.y),
         prob_asian = coalesce(prob_asian.x, prob_asian.y),
         prob_black = coalesce(prob_black.x, prob_black.y), 
         prob_white = coalesce(prob_white.x, prob_white.y), 
         prob_hispanic = coalesce(prob_hispanic.x, prob_hispanic.y)
         ) %>%
  select(-respondent_last.x, -respondent_first.x, -respondent_last.y, -respondent_first.y,
         -race.y, -prob_asian.x, -prob_asian.y, -prob_black.x, -prob_black.y, -prob_white.x, -prob_white.y,
         -prob_hispanic.x, -prob_hispanic.y
         ) %>%
  arrange(race) %>%
  
  #keep distinct opanums (should be no duplicate properties)
  distinct(opanum, substr(RecordMatch, 0, 3), .keep_all = TRUE)

##-----Prediction thresholds----

##looking at analyzing based on the threshold probability of race predictions
all_properties_race_threshold <- all_properties_geocoded %>%
  mutate(probability = case_when(race == "White" ~ prob_white,
                                 race == "Black" ~ prob_black,
                                 race == "Asian" ~ prob_asian,
                                 race == "Hispanic" ~ prob_hispanic,
                               is.na(race) ~ NA),
         meets_threshold = ifelse(probability>.75,TRUE,FALSE)) %>%
  select(-prob_white, -prob_black, -prob_asian, -prob_hispanic)

all_properties_race_threshold %>%
  filter(!is.na(race) & meets_threshold) %>%
  tabyl(race) %>%
  adorn_totals() %>%
  adorn_pct_formatting()

mean(all_properties_race_threshold$probability, na.rm = TRUE)


#-----Clean up Act 135 Property Data and Add Flags-----

#Human vs non-human
all_properties_geocoded_flags <- all_properties_geocoded %>%
  mutate(resp_entity = case_when(
    grepl("CITY|LAND\040BANK|REDEVELOPMENT|AUTHORITY", respondent) ~ "city",
    grepl("HEIR|ESTATE\040OF|EXECUTRIX|ESTATES", respondent) ~ "heir",
    grepl("LLC|HOLDINGS|REALTY|LP$|PROPERTIES|CORPORATION|SCIOLI-TURCO|DEVELOPERS|COMPANY|METAL|INTEGRAL|GROUP|INVESTMENT|AVDC"
                                       , respondent) ~ "corporate",
                           grepl("CEMETARY|CHURCH|MUSLIM|PRESBY|MINISTRY|MINISTRIES", respondent) ~ "religious",
                           grepl("INC$|FOUNDATION|CHARITIES|GUILD", respondent) ~ "non-profit",
                           TRUE ~ "person"),
         resp_human = case_when(resp_entity == "person" | resp_entity == "heir" ~ TRUE,
                                 TRUE ~ FALSE))

props_with_names <- all_properties_geocoded_flags %>%
  filter(!is.na(race) | resp_human)

write.csv(props_with_names, row.names = FALSE, "processed/properties_with_names.csv")

#-----Properties spanning multiple addresses-----

#did not do anything with this but it is one way of assigning more weight to properties that span multiple addresses
all_properties_geocoded_flags <- all_properties_geocoded_flags %>%
  mutate(multi = case_when(grepl("6703-6703R", RecordMatch) ~ FALSE,
    grepl("-", RecordMatch) ~ TRUE,
                           TRUE ~ FALSE))

all_properties_geocoded_flags <- all_properties_geocoded_flags %>%
  mutate(numbers = str_extract(RecordMatch, "\\d+-\\d+"),
                 numero1 = str_extract(numbers, "\\d+-"),
                 numero2 = str_extract(numbers, "-\\d+"),
                 numero1 = str_remove(numero1, "-"),
                 numero2 = as.numeric(str_remove(numero2, "-")),
                 numero1 = as.numeric(case_when(nchar(numero2)<=2 ~ str_sub(numero1,-2,-1),
                                     nchar(numero2)>2 ~ numero1)),
                 number_props = (numero2 - numero1)/2,
         number_props = replace_na(number_props,1),
         number_props = if_else(number_props == 0, 1, number_props)) %>%
          select(-numbers, -numero1, -numero2)

#-----Prep for human vs nonhuman-----

#Both are 364 now
human_properties_distinct_ppl <- all_properties_geocoded_flags %>%
  filter(resp_human & !is.na(race))

# #See where my human properties differ from Noah's
# human_properties2 <- all_properties_geocoded_flags %>%
#   filter(resp_human)
# #This is complete now
# discrepancy_from_noah <- human_properties2 %>% #needs predicted race/ethnicity
#   filter(is.na(race))

#Calculate counts multiple times if someone has multiple properties
human_properties_distinct_ppl %>%
  tabyl(race) %>%
  adorn_totals() %>%
  adorn_pct_formatting() 

predicted_eths %>%
  tabyl(race) %>%
  adorn_totals() %>%
  adorn_pct_formatting() 

#-----create html labels for mapping------

content <- paste("<p> Address:", all_properties_geocoded_flags$RecordMatch, "<br>",
                 "OPA Number:", all_properties_geocoded_flags$opanum, "<br>",
                 "Petitioner:", all_properties_geocoded_flags$petitioner, "<br>",
                 "Respondent:", all_properties_geocoded_flags$respondent, "<br>",
                 "Respondent Entity Type:", all_properties_geocoded_flags$resp_entity, "<br>",
                 "# of Properties:", all_properties_geocoded_flags$number_props, "<br>") %>%
  lapply(htmltools::HTML)

content2 <- paste("<p> Address:", all_properties_geocoded_flags$RecordMatch, "<br>",
                 "OPA Number:", all_properties_geocoded_flags$opanum, "<br>",
                 "Petitioner:", all_properties_geocoded_flags$petitioner, "<br>",
                 "Respondent:", all_properties_geocoded_flags$respondent, "<br>",
                 "Respondent Entity Type:", all_properties_geocoded_flags$resp_entity, "<br>",
                 "# of Properties:", all_properties_geocoded_flags$number_props, "<br>",
                 "Predicted Ethnicity:", all_properties_geocoded_flags$race) %>%
  lapply(htmltools::HTML)




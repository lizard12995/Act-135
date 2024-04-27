
#--Setup--

library(readxl)
library(tidyverse)
library(tidycensus)
library(stringr)
library(tidyr)
library(leaflet)
library(lubridate)
library(sf)
library(rethnicity)
library(shiny)
library(shinythemes)
library(shinyjs)
library(janitor) 
library(tidygeocoder)
library(geojsonio)

#----NEW - Read Data----

raw_docket <- read_xlsx('raw/Act-135-Cases_2024.02.07.xlsx')
raw_opa <- read.csv('raw2/opa_properties_public.csv', colClasses = c("parcel_number" = "character"))

# Merge with OPA Data

docket_3 <- raw_docket %>%
  filter(!is.na(opanum)) %>%
  left_join(raw_opa, by = c("opanum" = "parcel_number"))

#### Separate out coordinates

# Remove "SRID=2272;POINT (" from the beginning of each string
docket_3$shape <- gsub("^SRID=\\d+;POINT  \\(\\ s*", "", docket_3$shape)

# Remove trailing ")"
docket_3$shape <- gsub("\\)", "", docket_3$shape)

# Split the remaining string into latitude and longitude
docket_3 <- cbind(docket_3, do.call(rbind, strsplit(docket_3$shape, " ")))

# Rename the columns
colnames(docket_3)[91:92] <- c("Latitude", "Longitude")

# Convert to numeric
docket_3[, c("Latitude", "Longitude")] <- sapply(docket_3[, c("Latitude", "Longitude")], as.double)

# Remove the original shape column
docket_3 <- docket_3 %>%
  select(-c("shape"))

# Deal with non-matches

docket_missing_coords <- docket_3 %>%
  filter(is.na(Latitude)) %>%
  select(-c("Latitude", "Longitude"))

docket_missing_coords$City <- "Philadelphia"
docket_missing_coords$State <- "PA"

docket_missing_coords[49,10] = "323 W GIRARD AVE"
docket_missing_coords[27,10] = "1641 N 2ND STREET"
docket_missing_coords[28,10] = "830 N 4TH STREET"
docket_missing_coords[36,10] = "930 N 2ND STREET"
docket_missing_coords[1,10] = "753 S 8TH STREET"
docket_missing_coords[39,10] = "930 N 2ND STREET"

docket_geocode <- docket_missing_coords %>%
  geocode(method = "census", street = address, city= City, state = State, lat = latitude, long = longitude) %>%
  rename(c("Latitude" = "latitude", "Longitude" = "longitude")) %>%
  select(-c("City", "State"))

docket_geocode2 <- docket_geocode %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

# Work with matched data
docket_with_coords <- docket_3 %>%
  filter(!is.na(Latitude))

docket_4 <- docket_with_coords %>%
  st_as_sf(coords = c("Latitude", "Longitude"), crs = 2272)

docket_4 <- docket_4 %>%
  st_transform(4326)

# Separate first/last name of single respondent and trim white space 

geocoded_docket_opa <- docket_4 %>%
  rbind(docket_geocode2) %>%
  separate(SING_RESP, ",", into = c("LAST", "FIRST"), remove = FALSE) %>%
  mutate(FIRST = trimws(FIRST),
         LAST = trimws(LAST))

geocoded_docket_opa_withnames <- geocoded_docket_opa %>%
  filter(!is.na(FIRST)) %>%
  distinct(SING_RESP, .keep_all = TRUE) %>%
  distinct(opanum, substr(address, 0, 3), .keep_all = TRUE)

##-----NAME ANALYSIS -----

geocoded_docket_opa_withnames[48,8] <- "O'MALLEY"
geocoded_docket_opa_withnames[177,8] <- "O'CONNELL"
geocoded_docket_opa_withnames[368,8] <- "KELLAM"

geocoded_docket_opa_withnames[20,9] <-"ERDIS"
geocoded_docket_opa_withnames[23,9] <- "JAMES"
geocoded_docket_opa_withnames[34,9] <-"EMERY"
geocoded_docket_opa_withnames[86,9] <-"LOUIS"
geocoded_docket_opa_withnames[98,9] <- "MICHAEL"
geocoded_docket_opa_withnames[100,9] <- "MICHAEL"
geocoded_docket_opa_withnames[115,9] <-"HOWARD"
geocoded_docket_opa_withnames[275,9] <-"HERBERT"
geocoded_docket_opa_withnames[395,9] <-"ANNA"

# get rethnicity predictions:
lastnames <- trimws(geocoded_docket_opa_withnames$LAST)
firstnames <- trimws(geocoded_docket_opa_withnames$FIRST)
predicted_eths <<- predict_ethnicity(firstnames = firstnames, lastnames = lastnames,
                                     method = "fullname")

predicted_eths <- predicted_eths %>%
  rename(c("FIRST" = "firstname", "LAST" = "lastname"))

predicted_eths %>%
  filter(!is.na(race)) %>%
  tabyl(race) %>%
  adorn_totals() %>%
  adorn_pct_formatting()

# join back together!
all_properties_geocoded <- geocoded_docket_opa %>%
  left_join(predicted_eths, by = c("FIRST", "LAST"))

#-----Read other data-----

#Neighborhoods 
neighborhoods <- geojson_read('https://raw.githubusercontent.com/azavea/geo-data/master/Neighborhoods_Philadelphia/Neighborhoods_Philadelphia.geojson', what = "sp")
neighborhoods_fort <- st_as_sf(neighborhoods) %>%
  st_transform(4326)

cds <- st_read('raw/Council_Districts_2024.geojson')

bgs21 <- st_read("raw/bgs.geojson") %>%
  st_transform(4326)

#Displacement Risk Ratio
drr_raw <- read.csv("raw/drr_bg_apr23.csv", colClasses=c("geoid"='character')) %>%
  rename("GEOID"="geoid") %>%
  select("GEOID", 
         "DRR1516", "DRR1516C",
         "DRR2122", "DRR2122C", "dDRR1121WPPNC")


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
    city = ifelse(grepl("CITY|LAND\040BANK|REDEVELOPMENT|AUTHORITY", respondent),1,0),
    heir = ifelse(grepl("HEIR|ESTATE\040OF|EXECUTRIX|ESTATES", respondent),1,0),
    corporate = ifelse(grepl("LLC|HOLDINGS|REALTY|LP$|PROPERTIES|CORPORATION|SCIOLI-TURCO|DEVELOPERS|COMPANY|METAL|INTEGRAL|GROUP|INVESTMENT|AVDC"
                             , respondent), 1, 0),
    religious = ifelse(grepl("CEMETARY|CHURCH|MUSLIM|PRESBY|MINISTRY|MINISTRIES", respondent),1,0),
    nonprof = ifelse(grepl("INC$|FOUNDATION|CHARITIES|GUILD", respondent),1,0),
    resp_human = ifelse(!is.na(FIRST),TRUE,FALSE),
    person_and_other = ifelse(resp_human & (city == 1 | corporate ==1 | religious ==1 | nonprof ==1), 
                              1, 0),
    person_no_other = ifelse(resp_human & (city == 0 & corporate ==0 & religious ==0 & nonprof ==0), 
                             1, 0))

props_with_names <- all_properties_geocoded_flags %>%
  filter(resp_human)

single_opa <- all_properties_geocoded_flags %>%
  distinct(opanum, .keep_all = TRUE)

#post-amendments
all_properties_geocoded_flags %>%
  filter(as.Date(dfiled) <= '2015-01-01') %>%
  st_drop_geometry() %>%
  summarise(count = n())

#QUICK STATS
sum(single_opa$heir)
sum(single_opa$person_and_other)
sum(single_opa$person_no_other)
sum(all_properties_geocoded_flags$heir)

all_properties_geog <- single_opa %>%
  st_join(cds) %>%
  st_join(bgs21) %>%
  left_join(drr_raw, by = "GEOID")

#District grouping
all_properties_geog %>%
  group_by(DISTRICT) %>%
  st_drop_geometry() %>%
  summarise(count = n())

all_properties_geog2 <- all_properties_geog %>%
  mutate(majority_white = ifelse(pct_non_white < 50, 1,0),
         majority_non_white = ifelse(pct_non_white >= 50,1,0),
         majority_black = ifelse(pct_black >= 50,1,0))

#Block group composition analysis
sum(all_properties_geog2$majority_white, na.rm = TRUE) / nrow(all_properties_geog2) * 100
sum(all_properties_geog2$majority_non_white, na.rm = TRUE) / nrow(all_properties_geog2) * 100
sum(all_properties_geog2$majority_black, na.rm = TRUE) / nrow(all_properties_geog2) * 100

all_properties_geog$dfiled <- as.Date(all_properties_geog$dfiled)

#DRR composition analysis
all_properties_geog %>%
  distinct(opanum, .keep_all = TRUE) %>%
  #filter(dfiled < "2023-01-01") %>%
  tabyl(dDRR1121WPPNC) %>%
  adorn_totals() %>%
  adorn_pct_formatting()

#petitioner analysis
all_properties_petitioner <- all_properties_geog2 %>%
  select(dfiled, petitioner, DISTRICT,resp_human) %>%
  mutate(turco = str_detect(petitioner, "SCIOLI"),
         pcdc = str_detect(petitioner, "PHILADELPHIA COMMUNITY DEVELOPMENT COALITION"),
         big_2 = ifelse(turco == TRUE | pcdc == TRUE, "ST or PCDC", "Other"))


all_properties_petitioner2 <- all_properties_geocoded_flags %>%
  mutate(turco = str_detect(petitioner, "SCIOLI"),
         pcdc = str_detect(petitioner, "PHILADELPHIA COMMUNITY DEVELOPMENT COALITION"),
         barr = str_detect(petitioner, "GLOBAL GIVINGS |TULPEHOCKEN|710 N 42ND"),
         commgrowth = str_detect(petitioner, "PHILADELPHIA COMMUNITY GROWTH"),
         nd = str_detect(petitioner, "NEIGHBORHOOD DEVELOPMENT PARTNERSHIP"),
         airy = str_detect(petitioner, "MT. AIRY"),
         williams = str_detect(petitioner, "JAMAR"),
         big_2 = ifelse(turco == TRUE | pcdc == TRUE, "ST or PCDC", "Other"))

sum(all_properties_petitioner2$turco)
sum(all_properties_petitioner2$pcdc)
sum(all_properties_petitioner2$barr)
sum(all_properties_petitioner2$commgrowth)
sum(all_properties_petitioner2$nd)
sum(all_properties_petitioner2$airy)
sum(all_properties_petitioner2$williams)

count <- all_properties_petitioner2 %>%
  st_drop_geometry() %>%
  group_by(petitioner) %>%
  summarise(count = n()) %>%
  #filter(count >=5) %>%
  arrange(count, descending = TRUE)

#phl community growth = keith regan
#global givings = Ephraim Barr - also owns 1940 N Hollywood LLC, 710 N 42nd LLC, 1537 Tulpehocken LLC
#extensive criminal record related to fraud
#Comm Preservation Alliance Inc - same phone as Axcel Capital

#-----Prep for human vs nonhuman-----

human_properties_distinct_ppl <- all_properties_geocoded_flags %>%
  filter(resp_human & !is.na(race))

human_properties_distinct_ppl_ONLY <- all_properties_geocoded_flags %>%
  distinct(opanum, .keep_all = TRUE) %>%
  distinct(SING_RESP, .keep_all=TRUE) %>%
  filter(resp_human & !is.na(race) & person_no_other == 1)

#Calculate counts multiple times if someone has multiple properties

# Counts by TOTAL PETITIONS FILED
human_properties_distinct_ppl %>%
  tabyl(race) %>%
  adorn_totals() %>%
  adorn_pct_formatting() 

# Counts by UNIQUE PPL, PROPERTIES, ONLY HUMANS
human_properties_distinct_ppl_ONLY %>%
  tabyl(race) %>%
  adorn_totals() %>%
  adorn_pct_formatting() 

# Counts by UNIQUE PEOPLE & PROPERTIES
predicted_eths %>%
  tabyl(race) %>%
  adorn_totals() %>%
  adorn_pct_formatting() 

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
  mutate(resp_human = ifelse(resp_human == 1, "Human", "Non-human"))


# -----Static Maps------

# All points and neighborhoods
# 
# all_pts_nhd <- ggplot(neighborhoods_fort) +
#   geom_sf(alpha = .5) +
#   geom_sf(data = all_properties_geocoded_flags,
#           size = .2) +
#   ggtitle("Philadelphia Act 135 Properties",
#           subtitle = "Including Philadelphia neighborhood boundaries") +
#   theme_void() +
#   theme(panel.grid.major = element_line(colour = 'transparent'),
#         legend.margin = margin(0.3, 0.3, 0.3, 0.3, "cm"),
#         plot.margin = margin(0.4, 0.4, 0.4, 0.4, "cm"),
#         plot.title =element_text(family="AppleGothic", size=15, hjust = .5),
#         plot.subtitle = element_text(family="AppleGothic", size=10, hjust = .5),
#         legend.text = element_text(family="AppleGothic", size=8, hjust = .5))

# all_pts_nhd

# All Points and CDs

all_pts_cds <- ggplot(cds) +
  geom_sf(alpha = .5) +
  geom_sf(data = all_properties_geocoded_flags,
          size = .2) +
  ggtitle("Philadelphia Act 135 Properties",
          subtitle = "Including Philadelphia Council Districts") +
  theme_void() +
  theme(panel.grid.major = element_line(colour = 'transparent'),
        legend.margin = margin(0.3, 0.3, 0.3, 0.3, "cm"),
        plot.margin = margin(0.4, 0.4, 0.4, 0.4, "cm"),
        plot.title =element_text(family="AppleGothic", size=15, hjust = .5),
        plot.subtitle = element_text(family="AppleGothic", size=10, hjust = .5),
        legend.text = element_text(family="AppleGothic", size=8, hjust = .5))


all_pts_petitioner$big_2 <- as.factor(all_pts_petitioner$big_2)

all_pts_petitioner <- ggplot(cds) +
  geom_sf(alpha = .5) +
  geom_sf(data = all_properties_petitioner,
          aes(color = big_2),
          size = .7) +
  ggtitle("Philadelphia Act 135 Properties",
          subtitle = "Including Philadelphia Council Districts") +
  theme_void() +
  theme(panel.grid.major = element_line(colour = 'transparent'),
        legend.margin = margin(0.3, 0.3, 0.3, 0.3, "cm"),
        plot.margin = margin(0.4, 0.4, 0.4, 0.4, "cm"),
        plot.title =element_text(family="AppleGothic", size=15, hjust = .5),
        plot.subtitle = element_text(family="AppleGothic", size=10, hjust = .5),
        legend.text = element_text(family="AppleGothic", size=8, hjust = .5))

all_pts_petitioner


all_pts_nhd <- ggplot(neighborhoods_fort) +
  geom_sf(alpha = .5) +
  geom_sf(data = all_properties_geocoded_flags,
          size = .2) +
  ggtitle("Philadelphia Act 135 Properties",
          subtitle = "Including Philadelphia Neighborhoods") +
  theme_void() +
  theme(panel.grid.major = element_line(colour = 'transparent'),
        legend.margin = margin(0.3, 0.3, 0.3, 0.3, "cm"),
        plot.margin = margin(0.4, 0.4, 0.4, 0.4, "cm"),
        plot.title =element_text(family="AppleGothic", size=15, hjust = .5),
        plot.subtitle = element_text(family="AppleGothic", size=10, hjust = .5),
        legend.text = element_text(family="AppleGothic", size=8, hjust = .5))

# all_pts_cds

# All points and % nonwhite

all_pts_nw <- ggplot(bgs21) +
  geom_sf(aes(fill=pct_non_white),  alpha = .7, stroke = .3) +
  geom_sf(data = all_properties_geocoded_flags,
          size = .5,
          color = "red") +
  scale_fill_viridis_c(name = "Percent Non-White", alpha = .7, direction = -1) +
  labs(title = "Philadelphia Act 135 Properties and\nPercent of Population Identifying as Non-White",
          caption = "American Community Survey, 2021") +
  theme_void() +
  theme(panel.grid.major = element_line(colour = 'transparent'),
        legend.margin = margin(0.3, 0.3, 0.3, 0.3, "cm"),
        plot.margin = margin(0.4, 0.4, 0.4, 0.4, "cm"),
        plot.title =element_text(family="AppleGothic", size=15, hjust = .5),
        plot.subtitle = element_text(family="AppleGothic", size=10, hjust = .5),
        plot.caption = element_text(family="AppleGothic", size=8, hjust = .5),
        legend.text = element_text(family="AppleGothic", size=8, hjust = .5),
        legend.title = element_text(family="AppleGothic", size=8, hjust = .5))

# all_pts_nw

# All points and DRR

all_pts_drr <- ggplot(bgs21) +
  geom_sf(aes(fill=DRR2122C),
          alpha = .7, stroke = .3) +
  geom_sf(data = all_properties_geocoded_flags,
          size = .6,
          color = "red") +
  ggtitle("Philadelphia Act 135 Properties and\nBlock Group Displacement Risk Ratio, 2021") +
  scale_fill_brewer(name = "Displacement Risk Ratio", palette = 3, direction = 1) +
  theme_void() +
  theme(panel.grid.major = element_line(colour = 'transparent'),
        legend.margin = margin(0.3, 0.3, 0.3, 0.3, "cm"),
        plot.margin = margin(0.4, 0.4, 0.4, 0.4, "cm"),
        plot.title =element_text(family="AppleGothic", size=15, hjust = .5),
        plot.subtitle = element_text(family="AppleGothic", size=10, hjust = .5),
        legend.text = element_text(family="AppleGothic", size=12),
        legend.title = element_text(family="AppleGothic", size=14))

all_pts_drr

# Predicted Rethnicity and neighorhoods

predicted_rethnicity <- ggplot(all_properties_geocoded_flags %>%
                                        filter(!is.na(race))) +
  geom_sf(data = cds, alpha = .5) +
  geom_sf(aes(color = race), size = 1.5, stroke = .5) +
  labs(title = "Predicted Race or Ethnicity of Property Owner",
       subtitle = "For all Act 135 properties with individual, non-corporate respondents",
       color = "Predicted Race or Ethnicity") +
  theme_void() +
  theme(panel.grid.major = element_line(colour = 'transparent'),
        legend.margin = margin(0.3, 0.3, 0.3, 0.3, "cm"),
        plot.margin = margin(0.4, 0.4, 0.4, 0.4, "cm"),
        plot.title =element_text(family="AppleGothic", size=15, hjust = .5),
        plot.subtitle = element_text(family="AppleGothic", size=10, hjust = .5),
        legend.text = element_text(family="AppleGothic", size=10),
        legend.title = element_text(family="AppleGothic", size=12))

predicted_rethnicity

ggsave('plots/predicted_rethnicity.jpeg', predicted_rethnicity, scale = 1, width = 10, units = "in", dpi = 500)

ggsave('plots/all_pts_drr.jpeg', all_pts_drr, scale = 1, width = 10, units = "in", dpi = 500)

ggsave('plots/all_pts_nw.jpeg', all_pts_nw, scale = 1, width = 10, units = "in", dpi = 500)

ggsave('plots/all_pts_cds.jpeg', all_pts_cds, scale = 1, width = 10, units = "in", dpi = 500)

ggsave('plots/all_pts_nhd.jpeg', all_pts_nhd, scale = 1, width = 10, units = "in", dpi = 500)


#----Graphs and additional maps for presentation----

table_1<-read_excel("~/Documents/GitHub/Act-135/Act_135_Philly/raw/table_1.xlsx")
table_2<-read_excel("~/Documents/GitHub/Act-135/Act_135_Philly/raw/table_2.xlsx")

graph_table_1 <- table_1 %>%
  pivot_longer(cols = c("Act 135 Respondents", "Total Respondents", "Phila.", "Phila. Homeowners")) %>%
  filter(name != "Total Respondents" & name != "Phila.") 

table_1 <- ggplot(graph_table_1, aes(fill = name, y = value, x = Race)) +
  geom_bar(position = position_dodge(), stat = "identity") +
  scale_fill_manual(name = "", values = c("#ef476f","#2c4263")) +
  ylab("Percent") +
  scale_y_continuous(breaks = seq(0,45,by = 5)) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 16),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 12),
        panel.grid.minor = element_line(colour = 'transparent'),
        panel.grid.major.x  = element_blank(),
        legend.margin = margin(0.3, 0.3, 0.3, 0.3, "cm"),
        plot.margin = margin(0.4, 0.4, 0.4, 0.4, "cm"),
        legend.text = element_text(size=11),
        legend.position = "bottom")

graph_table_2 <- table_2 %>%
  pivot_longer(cols = c("Act 135 Petitions", "Philadelphia"))

table_2 <- ggplot(graph_table_2, aes(fill = name, y = value, x = Category)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_manual(name = "", values = c("#ef476f","#2c4263")) +
  theme_minimal() +
  scale_y_continuous(breaks = seq(0,75,by = 5)) +
  ylab("Percent") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 16),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 12),
        panel.grid.minor = element_line(colour = 'transparent'),
              legend.margin = margin(0.3, 0.3, 0.3, 0.3, "cm"),
              plot.margin = margin(0.4, 0.4, 0.4, 0.4, "cm"),
              legend.text = element_text(size=11),
        panel.grid.major.x  = element_blank(),
        legend.position = "bottom")

ggsave('plots/table_1.jpeg', table_1, scale = 1, width = 10, units = "in", dpi = 500, bg = "transparent")
ggsave('plots/table_2.jpeg', table_2, scale = 1, width = 10, units = "in", dpi = 500, bg = "transparent")


bgs21.2 <- bgs21 %>%
  mutate(majority_nonwhite = ifelse(pct_non_white > 50, "Non-White","White"))


bgs21.2$majority_nonwhite <- factor(bgs21.2$majority_nonwhite, ordered = TRUE, levels = c("Non-White", "White"
))

all_points_category <- ggplot(bgs21.2) +
  geom_sf(aes(fill=majority_nonwhite), alpha = .7, stroke = .3) +
  geom_sf(data = all_properties_geocoded_flags,
          size = .5,
          color = "#ffd166") +
  scale_fill_manual(name = "Majority", values = c("#2c4263", "#ef476f")) +
  labs(title = "Philadelphia Act 135 Properties and\nWhether Block Group is Majority Non-White",
       caption = "American Community Survey, 2021") +
  theme_void() +
  theme(panel.grid.major = element_line(colour = 'transparent'),
        legend.margin = margin(0.3, 0.3, 0.3, 0.3, "cm"),
        plot.margin = margin(0.4, 0.4, 0.4, 0.4, "cm"),
        plot.title =element_text(family="AppleGothic", size=15, hjust = .5),
        plot.subtitle = element_text(family="AppleGothic", size=10, hjust = .5),
        plot.caption = element_text(family="AppleGothic", size=8, hjust = .5),
        legend.text = element_text(family="AppleGothic", size=8, hjust = .5),
        legend.title = element_text(family="AppleGothic", size=8, hjust = .5),
        )

all_points_category

ggsave('plots/all_points_category.jpeg', all_points_category, scale = 1, width = 10, units = "in", dpi = 500, bg = "transparent")




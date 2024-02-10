


POLYGON DATA

DRR22 <- colorFactor(
  palette = "magma",
  domain = bgs21$DRR2122C)

DRR16 <- colorFactor(
  palette = "magma",
  domain = bgs21$DRR1516C)

non_white <- colorNumeric(
  palette = "viridis",
  domain = bgs21$pct_non_white)

asian <- colorNumeric(
  palette = "viridis",
  domain = bgs21$pct_asian)

#POINT DATA

resp_entity <- colorFactor(
  palette = c("#d7191c","#fc8d59","#fee08b","#e6f598","#99d594","#3288bd"),
  domain = all_properties_geocoded_flags$resp_entity)

human_v_non <- colorFactor(
  palette = c("#d7191c","#3288bd"),
  domain = all_properties_geocoded_flags$resp_human)

human_eth <- colorFactor(
  palette = c("#d7191c","#fc8d59","#99d594","#3288bd"),
  domain = human_properties$race)


# MAP

points_map <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%

  addMapPane("polygons", zIndex = 410) %>%
  addMapPane("point", zIndex = 420) %>%

  #POLYGONS

  addPolygons(data = neighborhoods,
             stroke = TRUE,
             weight = 1,
             group = "Neighborhoods",
             label = neighborhoods@data$mapname,
             options = pathOptions(pane="polygons")) %>%
  addPolygons(data = bgs21,
              stroke = TRUE,
              weight = .5,
              fillOpacity = .5,
              color = ~DRR16(DRR1516C),
              group = "DRR 2016",
              options = pathOptions(pane="polygons")) %>%
  addPolygons(data = bgs21,
              stroke = TRUE,
              weight = .5,
              fillOpacity = .5,
              color = ~DRR22(DRR2122C),
              group = "DRR 2022",
              options = pathOptions(pane="polygons")) %>%
  addPolygons(data = bgs21,
              stroke = TRUE,
              weight = .5,
              fillOpacity = .5,
              color = ~non_white(pct_non_white),
              group = "Percent Non-White",
              options = pathOptions(pane="polygons")) %>%
  addPolygons(data = bgs21,
              stroke = TRUE,
              weight = .5,
              fillOpacity = .5,
              color = ~asian(pct_asian),
              group = "Percent Asian",
              options = pathOptions(pane="polygons")) %>%

  #POINTS

  addCircleMarkers(data = all_properties_geocoded_flags,
                   options = pathOptions(pane="point"),
                   fillColor= ~resp_entity(resp_entity),
                   fillOpacity = .7,
                   radius = 4,
                   stroke = TRUE,
                   weight = .5,
                   color = "black",
                   label = content,
                   labelOptions = c(direction='right'),
                   group = "All Entities and Types") %>%
  addCircleMarkers(data = all_properties_geocoded_flags,
                   options = pathOptions(pane="point"),
                   fillColor= ~human_v_non(resp_human),
                   fillOpacity = .7,
                   radius = 4,
                   stroke = TRUE,
                   weight = .5,
                   color = "black",
                   label = content2,
                   labelOptions = c(direction='right'),
                   group = "Human vs. Non-Human") %>%
  addCircleMarkers(data = human_properties,
                   options = pathOptions(pane="point"),
                   fillColor= ~human_eth(race),
                   fillOpacity = .7,
                   radius = 4,
                   stroke = TRUE,
                   weight = .5,
                   color = "black",
                   label = content2,
                   labelOptions = c(direction='right'),
                   group = "Predicted Race/Ethnicity") %>%

  # addLayersControl(
  #   baseGroups = c("Neighborhoods",
  #                  "DRR 2016", "DRR 2022",
  #                  "Percent Non-White",
  #                  "Percent Asian"),
  #   options = layersControlOptions(collapsed = FALSE)) %>%
  addLayersControl(
    baseGroups = c("All Entities and Types",
                   "Human vs. Non-Human", "Predicted Race/Ethnicity"),
    options = layersControlOptions(collapsed = FALSE),) %>%
  setView(lat = 39.94960, lng = -75.20723, zoom = 10) %>%
  addLegend(data = bgs21,
            "bottomleft",
            pal = DRR22,
            values = ~DRR2122C,
            title = "DRR",
            group = "DRR 2016") %>%
  addLegend(data = all_properties_geocoded_flags,
          "bottomright",
          pal = resp_entity,
          values = ~resp_entity,
          title = "Respondent Entity Type",
          group = "All Entities and Types") %>%
  addLegend(data = all_properties_geocoded_flags,
            "bottomleft",
            pal = human_v_non,
            values = ~resp_human,
            title = "Human?",
            group = "Human vs. Non-Human") %>%
  addLegend(data = human_properties,
            "bottomright",
            pal = human_eth,
            values = ~race,
            title = "Predicted Race/Ethnicity",
            group = "Predicted Race/Ethnicity")

points_map

## OLD

# %>%
# distinct(opanum, substr(address,1,4), .keep_all = TRUE)

# act135_cases %>%
#   distinct(docketnum) %>%
#   nrow()
#
# #577 unique docket numbers
#
# act135_cases %>%
#   distinct(opanum) %>%
#   nrow()
#
# #515 unique opanumbers
#
# act135_cases %>%
#   distinct(address) %>%
#   nrow()

#521 unique addresses

# #Cases with multiple OPA numbers and different respondents
# act135_cases_multopa <- act135_cases %>%
#   filter(opanum_count>1) %>%
#   select(respondent, opanum, opanum_count, status, address, `Final Disposition`, docketnum)


#-----Get ACS data-----

# bg.geo21 <- get_acs(geography = "block group",
#                        year = 2021,
#                        variables = "B03002_001",
#                        state = "PA",
#                        county = "Philadelphia",
#                        geometry = TRUE,
#                        cache = TRUE)
#
# bgs.acs21 <- get_acs(geography = "block group",
#                         variables = c("B03002_001", # Total population
#                                       "B03002_003", # Total not hisp: white
#                                       "B03002_004", # Total not hisp: Black
#                                       "B03002_006", # Total not hisp: Asian
#
#                                       "B19013_001", # Median household income
#                                       "B19013A_001", # Median household income: white householder
#                                       "B19013B_001", # Median household income: Black householder
#
#                                       "B25003_001", # Tenure
#                                       "B25003_002", # Tenure: owner-occupied
#                                       "B25003_003", # Tenure: renter-occupied
#
#                                       "B25003A_003", #Tenure - renter-occupied - white non-H
#
#
#                                       "B25003B_001", # Tenure - total Black non-H
#                                       "B25003B_002", # Tenure: owner-occupied - Black non-H
#                                       "B25003B_003"), # Tenure: renter-occupied - Black non-H
#
#                         year = 2021, state = "PA", county = "Philadelphia", geometry = FALSE,
#                         moe_level = "90", survey = "acs5") %>%
#
#   pivot_wider(names_from = variable, values_from = c(estimate, moe)) %>%
#
#   rename(totpop = estimate_B03002_001, # Total population
#          totnhisp_wh = estimate_B03002_003, # Total not hisp: white
#          totnhisp_bk = estimate_B03002_004, # Total not hisp: Black
#          totnhisp_as = estimate_B03002_006, # Total not hisp: Asian
#
#          medhhinc = estimate_B19013_001, # Median household income
#          medhhinc_wh = estimate_B19013A_001, # Median household income: white householder
#          medhhinc_bk = estimate_B19013B_001, # Median household income: Black householder
#
#          tenure = estimate_B25003_001, # tenure
#          ten_ownocc = estimate_B25003_002, # owner-occ
#          ten_renter = estimate_B25003_003, #renter-occ
#          w_ten_renter = estimate_B25003A_003, #renter-occ - white non-H
#
#          b_tenure = estimate_B25003B_001, # tenure - Black non-H
#          b_ten_ownocc = estimate_B25003B_002, # owner-occ - Black non-H
#          b_ten_renter = estimate_B25003B_003, #renter-occ - Black non-H
#
#          #MOE labeling
#
#          moe_totpop = moe_B03002_001, # Total population
#          moe_totnhisp_wh = moe_B03002_003, # Total not hisp: white
#          moe_totnhisp_bk = moe_B03002_004, # Total not hisp: Black
#
#          moe_medhhinc = moe_B19013_001, # Median household income
#          moe_medhhinc_wh = moe_B19013A_001, # Median household income: white householder
#          moe_medhhinc_bk = moe_B19013B_001, # Median household income: Black householder
#
#          moe_tenure = moe_B25003_001, # tenure
#          moe_ten_ownocc = moe_B25003_002, # owner-occ
#          moe_ten_renter = moe_B25003_003,  # renter-occ
#          moe_w_ten_renter = moe_B25003A_003, #renter-occ - white non-H
#
#          moe_b_tenure = moe_B25003B_001, # tenure - Black non-H
#          moe_b_ten_ownocc = moe_B25003B_002, # owner-occ - Black non-H
#          moe_b_ten_renter = moe_B25003B_003) %>% #renter-occ - Black non-H
#   mutate(pct_black = round(totnhisp_bk/totpop*100,2),
#          moe_pct_black = round(moe_prop(totnhisp_bk,totpop, moe_totnhisp_bk, moe_totpop)*100,2),
#          pct_white_renter = round(w_ten_renter/ten_renter*100,2),
#          moe_white_renter = round(moe_prop(w_ten_renter,ten_renter, moe_w_ten_renter, moe_ten_renter)*100,2),
#          pct_bnh_renter = round(b_ten_renter/ten_renter*100,2),
#          pct_bnh_renter2 = round(b_ten_renter/tenure*100,2),
#          pct_renter = round(ten_renter/tenure*100,2),
#          moe_bnh_renter = round(moe_prop(b_ten_renter,ten_renter, moe_b_ten_renter, moe_ten_renter)*100,2),
#          pct_non_white = round((totpop - totnhisp_wh)/totpop * 100, 2),
#          pct_asian = round(totnhisp_as/totpop*100,2))
#
# bg.geo16 <- get_acs(geography = "block group",
#                        year = 2016,
#                        variables = "B03002_001",
#                        state = "PA",
#                        county = "Philadelphia",
#                        geometry = TRUE,
#                        cache = TRUE)
#
# bgs.acs16 <- get_acs(geography = "block group",
#                         variables = c("B03002_001", # Total population
#                                       "B03002_003", # Total not hisp: white
#                                       "B03002_004", # Total not hisp: Black
#
#                                       "B19013_001", # Median household income
#                                       "B19013A_001", # Median household income: white householder
#                                       "B19013B_001", # Median household income: Black householder
#
#                                       "B25003_001", # Tenure
#                                       "B25003_002", # Tenure: owner-occupied
#                                       "B25003_003", # Tenure: renter-occupied
#
#                                       "B25003A_003", #Tenure - renter-occupied - white non-H
#
#
#                                       "B25003B_001", # Tenure - total Black non-H
#                                       "B25003B_002", # Tenure: owner-occupied - Black non-H
#                                       "B25003B_003"), # Tenure: renter-occupied - Black non-H
#
#                         year = 2016, state = "PA", county = "Philadelphia", geometry = FALSE,
#                         moe_level = "90", survey = "acs5") %>%
#
#   pivot_wider(names_from = variable, values_from = c(estimate, moe)) %>%
#
#   rename(totpop = estimate_B03002_001, # Total population
#          totnhisp_wh = estimate_B03002_003, # Total not hisp: white
#          totnhisp_bk = estimate_B03002_004, # Total not hisp: Black
#
#          medhhinc = estimate_B19013_001, # Median household income
#          medhhinc_wh = estimate_B19013A_001, # Median household income: white householder
#          medhhinc_bk = estimate_B19013B_001, # Median household income: Black householder
#
#          tenure = estimate_B25003_001, # tenure
#          ten_ownocc = estimate_B25003_002, # owner-occ
#          ten_renter = estimate_B25003_003, #renter-occ
#          w_ten_renter = estimate_B25003A_003, #renter-occ - white non-H
#
#          b_tenure = estimate_B25003B_001, # tenure - Black non-H
#          b_ten_ownocc = estimate_B25003B_002, # owner-occ - Black non-H
#          b_ten_renter = estimate_B25003B_003, #renter-occ - Black non-H
#
#          #MOE labeling
#
#          moe_totpop = moe_B03002_001, # Total population
#          moe_totnhisp_wh = moe_B03002_003, # Total not hisp: white
#          moe_totnhisp_bk = moe_B03002_004, # Total not hisp: Black
#
#          moe_medhhinc = moe_B19013_001, # Median household income
#          moe_medhhinc_wh = moe_B19013A_001, # Median household income: white householder
#          moe_medhhinc_bk = moe_B19013B_001, # Median household income: Black householder
#
#          moe_tenure = moe_B25003_001, # tenure
#          moe_ten_ownocc = moe_B25003_002, # owner-occ
#          moe_ten_renter = moe_B25003_003,  # renter-occ
#          moe_w_ten_renter = moe_B25003A_003, #renter-occ - white non-H
#
#          moe_b_tenure = moe_B25003B_001, # tenure - Black non-H
#          moe_b_ten_ownocc = moe_B25003B_002, # owner-occ - Black non-H
#          moe_b_ten_renter = moe_B25003B_003) %>% #renter-occ - Black non-H
#
#   mutate(pct_black = round(totnhisp_bk/totpop*100,2),
#          pct_non_white = round((totpop - totnhisp_wh)/totpop * 100, 2),
#          moe_pct_black = round(moe_prop(totnhisp_bk,totpop, moe_totnhisp_bk, moe_totpop)*100,2),
#          pct_white_renter = round(w_ten_renter/ten_renter*100,2),
#          moe_white_renter = round(moe_prop(w_ten_renter,ten_renter, moe_w_ten_renter, moe_ten_renter)*100,2),
#          pct_bnh_renter = round(b_ten_renter/ten_renter*100,2),
#          moe_bnh_renter = round(moe_prop(b_ten_renter,ten_renter, moe_b_ten_renter, moe_ten_renter)*100,2))
#
#
# #-----Prep spatial data-----
#
# bgs16 <- bg.geo16 %>%
#   left_join(bgs.acs16) %>%
#   left_join(drr_raw) %>%
#   mutate(DRR1516C = replace_na(DRR1516C,'Insufficient Data'),
#          DRR2122C = replace_na(DRR2122C,'Insufficient Data'))
#
# bgs21 <- bg.geo21 %>%
#   left_join(bgs.acs21) %>%
#   left_join(drr_raw) %>%
#   mutate(DRR1516C = replace_na(DRR1516C,'Insufficient Data'),
#          DRR2122C = replace_na(DRR2122C,'Insufficient Data'))
#
# st_write(bgs21, "/Users/ejs/Desktop/R Projects/Act135/Act_135_Philly/raw/bgs.geojson",
#          )



# test<-all_properties_geocoded %>%
#   st_drop_geometry() %>%
#   filter(opanum_count>1) %>%
#   select(opanum, race, respondent, respondent_first, respondent_last)
#
#What is on Names that did not make it to geocoded properties?
# x <- names %>%
#   mutate(id = paste(respondent_first, respondent_last))
#
# y <- all_properties_geocoded %>%
#   mutate(id = paste(respondent_first,respondent_last))
#
# z <- setdiff(x$id,y$id)
#
# x <- x %>%
#   filter(id %in% z)

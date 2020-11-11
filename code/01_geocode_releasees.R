## THIS FILE GEOCODES THE DOC RELEASE RECORDS AND MAPS THEM TO THEIR HOME
## VOTER PRECINCTS AND CENSUS BLOCK GROUPS

released <- fread("./raw_data/doc/INMATE_RELEASE_ROOT_0419.csv")

parole <- fread("./raw_data/doc/OFFENDER_ROOT_0419.csv")

incarcerated <- fread("./raw_data/doc/INMATE_ACTIVE_ROOT_0419.csv")

released <- filter(released, !(DCNumber %in% parole$DCNumber),
                   !(DCNumber %in% incarcerated$DCNumber))

addresses <- fread("./raw_data/doc/INMATE_RELEASE_RESIDENCE_0419.csv")

released_with_addresses <- inner_join(released, addresses) %>% 
  filter(releasedateflag_descr == "valid release date")

full_n <- nrow(released_with_addresses)

saveRDS(full_n, "./temp/number_released.rds")

released_with_addresses <- released_with_addresses %>% 
  filter(grepl("[0-9]", substring(AddressLine1, 1, 1))) %>% 
  mutate(address = gsub("[#]", "", paste(AddressLine1, City)))

drop_start_chara <- full_n - nrow(released_with_addresses)
n_to_geo <- nrow(released_with_addresses)

saveRDS(nrow(released_with_addresses), "./temp/number_released_goodnumber.rds")


released_with_addresses$group <- ceiling((c(1:nrow(released_with_addresses)) /
                                           nrow(released_with_addresses))*4)

# these take a long time and are expensive to run so commenting out
lats_longs1 <- geocode(filter(released_with_addresses, group == 1)$address, override_limit = T, output = "more")
saveRDS(lats_longs1, "./temp/geocode_output1.rds")
lats_longs2 <- geocode(filter(released_with_addresses, group == 2)$address, override_limit = T, output = "more")
saveRDS(lats_longs2, "./temp/geocode_output2.rds")
lats_longs3 <- geocode(filter(released_with_addresses, group == 3)$address, override_limit = T, output = "more")
saveRDS(lats_longs3, "./temp/geocode_output3.rds")
lats_longs4 <- geocode(filter(released_with_addresses, group == 4)$address, override_limit = T, output = "more")
saveRDS(lats_longs4, "./temp/geocode_output4.rds")

lats_longs <- bind_rows(
  readRDS("./temp/geocode_output1.rds"),
  readRDS("./temp/geocode_output2.rds"),
  readRDS("./temp/geocode_output3.rds"),
  readRDS("./temp/geocode_output4.rds")
)

### now keep only those in florida with good addresses, map to precincts
released_with_addresses <- bind_cols(released_with_addresses, lats_longs) %>% 
  filter(grepl(", fl ", address1))

drop_geocode <- n_to_geo - nrow(released_with_addresses)
ngood_geo <- nrow(released_with_addresses)

released_with_addresses <- released_with_addresses %>% 
  filter(!is.na(lat), !is.na(lon),
         loctype %in% c("range_interpolated", "rooftop"))

ndrop_out_fl <- ngood_geo - nrow(released_with_addresses)

saveRDS(nrow(released_with_addresses), "./temp/good_geocoded.rds")

precincts <- readOGR("./raw_data/shapefiles/fl_2016_FEST",
                     "fl_2016")
precincts@data$full_id <- paste0(precincts@data$county, precincts@data$pct)

precincts <- spTransform(precincts, "+init=epsg:4326")

pings  <- SpatialPoints(released_with_addresses[c('lon','lat')], proj4string = precincts@proj4string)
released_with_addresses$precinct <- over(pings, precincts)$full_id

#### now map to census block groups

bgs <- readOGR("./raw_data/shapefiles/tl_2018_12_bg",
                     "tl_2018_12_bg")

bgs <- spTransform(bgs, "+init=epsg:4326")


pings  <- SpatialPoints(released_with_addresses[c('lon','lat')], proj4string = bgs@proj4string)
released_with_addresses$block_group <- over(pings, bgs)$GEOID

saveRDS(released_with_addresses, "./temp/released_with_addresses.rds")

saveRDS(sum(released_with_addresses$loctype %in% c("range_interpolated", "rooftop")), "./temp/good_geocoded.rds")


############ COLLAPSE DOWN TO PRECINCTS / BLOCK GROUPS
doc <- readRDS("./temp/released_with_addresses.rds") %>% 
  mutate(county = substring(precinct, 1, 3),
         precinct = str_pad(substring(precinct, 4), width = 10, side = "left", pad = "0"),
         cp = paste0(county, precinct),
         PrisonReleaseDate = as.Date(substring(PrisonReleaseDate, 1, 10), "%m/%d/%Y"),
         years_since = 2018 - year(PrisonReleaseDate)) %>% 
  filter(!is.na(cp),
         PrisonReleaseDate <= "2018-11-06") %>% 
  group_by(address1) %>% 
  mutate(big_release = n() >= 5,
         years_since = ifelse(big_release, NA, years_since)) %>% 
  group_by(cp) %>% 
  summarize(years_since = mean(years_since, na.rm = T),
            all_doc = n(),
            small_res_doc = sum(1 - big_release),
            big_release = sum(big_release))
saveRDS(doc, "temp/full_doc_precinct.rds")

doc_recent <- readRDS("./temp/released_with_addresses.rds") %>% 
  mutate(county = substring(precinct, 1, 3),
         precinct = str_pad(substring(precinct, 4), width = 10, side = "left", pad = "0"),
         cp = paste0(county, precinct),
         PrisonReleaseDate = as.Date(substring(PrisonReleaseDate, 1, 10), "%m/%d/%Y"),
         years_since = 2018 - year(PrisonReleaseDate)) %>% 
  filter(!is.na(cp),
         PrisonReleaseDate >= "2015-01-01",
         PrisonReleaseDate <= "2018-11-06") %>% 
  group_by(address1) %>% 
  mutate(big_release_recent = n() >= 5) %>% 
  group_by(cp) %>% 
  summarize(all_doc_recent = n(),
            small_res_doc_recent = sum(1 - big_release_recent),
            big_release_recent = sum(big_release_recent))
saveRDS(doc_recent, "temp/recent_doc_precinct.rds")

## block groups

doc_bg <- readRDS("./temp/released_with_addresses.rds") %>% 
  mutate(county = substring(precinct, 1, 3),
         PrisonReleaseDate = as.Date(substring(PrisonReleaseDate, 1, 10), "%m/%d/%Y")) %>% 
  filter(!is.na(block_group),
         PrisonReleaseDate <= "2018-11-06") %>% 
  group_by(address1) %>% 
  mutate(big_release = n() >= 5,
         years_since = ifelse(big_release, NA, 2018 - year(PrisonReleaseDate))) %>% 
  group_by(GEOID = block_group) %>% 
  summarize(years_since = mean(years_since, na.rm = T),
            all_doc = n(),
            small_res_doc = sum(1 - big_release))

saveRDS(doc_bg, "temp/all_doc_bg.rds")

doc_bg_recent <- readRDS("./temp/released_with_addresses.rds") %>% 
  mutate(county = substring(precinct, 1, 3),
         PrisonReleaseDate = as.Date(substring(PrisonReleaseDate, 1, 10), "%m/%d/%Y")) %>% 
  filter(!is.na(block_group),
         PrisonReleaseDate >= "2015-01-01",
         PrisonReleaseDate <= "2018-11-06") %>% 
  group_by(address1) %>% 
  mutate(big_release = n() >= 5) %>% 
  group_by(GEOID = block_group) %>% 
  summarize(all_doc_recent = n(),
            small_res_doc_recent = sum(1 - big_release))

saveRDS(doc_bg_recent, "temp/recent_doc_bg.rds")
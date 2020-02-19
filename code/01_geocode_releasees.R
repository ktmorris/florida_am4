

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
# lats_longs1 <- geocode(filter(released_with_addresses, group == 1)$address, override_limit = T, output = "more")
# saveRDS(lats_longs1, "./temp/geocode_output1.rds")
# lats_longs2 <- geocode(filter(released_with_addresses, group == 2)$address, override_limit = T, output = "more")
# saveRDS(lats_longs2, "./temp/geocode_output2.rds")
# lats_longs3 <- geocode(filter(released_with_addresses, group == 3)$address, override_limit = T, output = "more")
# saveRDS(lats_longs3, "./temp/geocode_output3.rds")
# lats_longs4 <- geocode(filter(released_with_addresses, group == 4)$address, override_limit = T, output = "more")
# saveRDS(lats_longs4, "./temp/geocode_output4.rds")

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

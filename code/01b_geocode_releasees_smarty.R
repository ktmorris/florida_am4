

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

# released_with_addresses <- kevostools::geocode(released_with_addresses %>% 
#                     rename(city = City,
#                            street = AddressLine1,
#                            state = State,
#                            zip = ZipCode))
# 
# saveRDS(released_with_addresses, "temp/released_with_addresses_post_geo_smarty.rds")

released_with_addresses <- readRDS("temp/released_with_addresses_post_geo_smarty.rds")
### now keep only those in florida with good addresses, map to precincts
released_with_addresses <- released_with_addresses %>% 
  filter(state == "FL")

drop_geocode <- n_to_geo - nrow(released_with_addresses)
ngood_geo <- nrow(released_with_addresses)

released_with_addresses <- released_with_addresses %>% 
  filter(!is.na(latitude), !is.na(longitude),
         match %in% c("Zip8", "Zip9"))

ndrop_out_fl <- ngood_geo - nrow(released_with_addresses)

saveRDS(nrow(released_with_addresses), "./temp/good_geocoded_ss.rds")

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

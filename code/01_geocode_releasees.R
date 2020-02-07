

released <- fread("./raw_data/doc/INMATE_RELEASE_ROOT_0419.csv")

parole <- fread("./raw_data/doc/OFFENDER_ROOT_0419.csv")

incarcerated <- fread("./raw_data/doc/INMATE_ACTIVE_ROOT_0419.csv")

released <- filter(released, !(DCNumber %in% parole$DCNumber),
                   !(DCNumber %in% incarcerated$DCNumber))

addresses <- fread("./raw_data/doc/INMATE_RELEASE_RESIDENCE_0419.csv")

released_with_addresses <- inner_join(released, addresses) %>% 
  filter(releasedateflag_descr == "valid release date",
         grepl("[0-9]", substring(address, 1, 1))) %>% 
  mutate(address = gsub("[#]", "", paste(AddressLine1, City)))

lats_longs <- geocode(released_with_addresses$address, override_limit = T, output = "more")

### now keep only those in florida with good addresses, map to precincts
released_with_addresses <- bind_cols(released_with_addresses, lats_longs) %>% 
  filter(!is.na(lat), !is.na(lon)) %>% 
  filter(grepl(", fl ", address1))

precincts <- readOGR("./raw_data/shapefiles/fl_2016_FEST",
                     "fl_2016")
precincts@data$full_id <- paste0(precincts@data$county, precincts@data$pct)

precincts <- spTransform(precincts, "+init=epsg:4326")

pings  <- SpatialPoints(released_with_addresses[c('lon','lat')], proj4string = precincts@proj4string)
released_with_addresses$precinct <- over(pings, precincts)$full_id

#### now map to census block groups

released_with_addresses <- readRDS("./temp/released_with_addresses.rds")

bgs <- readOGR("./raw_data/shapefiles/tl_2018_12_bg",
                     "tl_2018_12_bg")

bgs <- spTransform(bgs, "+init=epsg:4326")


pings  <- SpatialPoints(released_with_addresses[c('lon','lat')], proj4string = bgs@proj4string)
released_with_addresses$block_group <- over(pings, bgs)$GEOID

saveRDS(released_with_addresses, "./temp/released_with_addresses.rds")

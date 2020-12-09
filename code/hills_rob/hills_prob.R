# 
# 
# hills_prob <- rbindlist(lapply(LETTERS, function(l){
#   print(l)
#   if(l != "J"){
#   if(!(file.exists(paste0("raw_data/hills_prob/CircuitCriminalNameIndex_", l, ".txt")))){
#     download.file(paste0("https://publicrec.hillsclerk.com/Criminal/name_index/Circuit/CircuitCriminalNameIndex_", l, ".txt"),
#                   paste0("raw_data/hills_prob/CircuitCriminalNameIndex_", l, ".txt"))
#   }
# 
#   ja <- fread(paste0("raw_data/hills_prob/CircuitCriminalNameIndex_", l, ".txt"),
#         fill = T, sep = "|")
#   colnames(ja) <- clean_names(ja)
#   ja <- ja %>%
#     select(court_type, last_name, first_name, middle_name,
#            current_status, current_status_date,
#            street = party_address_line_1,
#            city = party_address_city,
#            state = party_address_state,
#            zip = party_address_zip_code,
#            disposition_date,
#            disposition_description,
#            date_of_birth,
#            disposition_date,
#            business_name,
#            current_status,
#            party_connection_type)
# 
#   return(ja)
#   }
# }))
# 
# j <- read_delim("raw_data/hills_prob/CircuitCriminalNameIndex_J.txt",
#                 delim = "|")
# colnames(j) <- clean_names(j)
# j <- j %>%
#   select(court_type, last_name, first_name, middle_name,
#          current_status, current_status_date,
#          street = party_address_line_1,
#          city = party_address_city,
#          state = party_address_state,
#          zip = party_address_zip_code,
#          disposition_date,
#          disposition_description,
#          date_of_birth,
#          disposition_date,
#          business_name,
#          current_status,
#          party_connection_type)
# 
# hills_prob <- bind_rows(hills_prob, j) %>%
#   filter(business_name == "",
#          current_status %in% c("Closed", "CLOSED"),
#          party_connection_type == "Defendant") %>%
#   select(-business_name, -current_status, -party_connection_type)
# 
# saveRDS(hills_prob, "temp/hills_prob_raw.rds")

hills_prob <- readRDS("temp/hills_prob_raw.rds")

codes <- fread("raw_data/hills_prob/codes.csv")

hills_prob <- left_join(hills_prob, codes) %>% 
  filter(guilty == "y")

one_per <- hills_prob %>% 
  mutate(disposition_date = as.Date(disposition_date, "%m/%d/%Y")) %>% 
  filter(disposition_date <= "2018-11-06") %>% 
  group_by(first_name, middle_name, last_name, date_of_birth) %>% 
  arrange(desc(disposition_date)) %>% 
  filter(row_number() == 1) %>% 
  ungroup()

#######################

one_per <- one_per %>% 
  filter(disposition_date >= "1997-10-01")

one_per$ad <- with(one_per, paste(street, city, state, zip))

# #####################
# addresses2 <- one_per %>%
#   group_by(ad) %>%
#   filter(row_number() == 1,
#          !(grepl("UNKNOWN|AT LARGE|XXXX", ad)),
#          grepl("[0-9]", substring(ad, 1, 1))) %>%
#   select(ad) %>%
#   ungroup()
# 
# addresses <- mutate(addresses, group = floor(row_number() / 1000))
# 
# 
# for(i in unique(addresses$group)){
#   print(i)
#   if(!file.exists(paste0("temp/lats_longs_hills_", i, ".rds"))){
#     run <- filter(addresses, group == i)
#     
#     run <- mutate_geocode(run, ad, output = "more")
#     
#     saveRDS(run, paste0("temp/lats_longs_hills_", i, ".rds")) 
#   }
# }
# 
# 
# ads <- rbindlist(lapply(unique(addresses$group), function(i){
#   readRDS(paste0("temp/lats_longs_hills_", i, ".rds"))
# }))
# 
# j <- filter(ads, is.na(lon)) %>% 
#   mutate(ad = gsub("#", "no", ad))
# 
# j <- mutate_geocode(select(j, ad, group), ad, output = "more")
# 
# k <- filter(ads, !is.na(lon))
# 
# ads <- bind_rows(j, k)


# saveRDS(ads, "temp/probation_addresses.rds")

addresses <- readRDS("temp/probation_addresses.rds")
one_per <- left_join(one_per, select(addresses, ad, lat, lon, loctype, address))

one_per <- filter(one_per, !is.na(lat), !is.na(lon),
                  loctype %in% c("range_interpolated", "rooftop"))


############################## formerly inc
inc <- readRDS("./temp/released_with_addresses.rds") %>% 
  rename(last_name = LastName, first_name = FirstName, middle_name = MiddleName) %>% 
  mutate_at(vars(last_name, first_name, middle_name),
            ~ gsub("[[:punct:]]| ", "", ifelse(. == "", NA, toupper(.)))) %>% 
  mutate(date_of_birth = substring(BirthDate, 1, 10)) %>% 
  filter(substring(block_group, 1, nchar("12057")) == "12057") %>% 
  group_by(date_of_birth, last_name, first_name, middle_name) %>% 
  filter(row_number() == 1) %>% 
  select(date_of_birth, address = address1, last_name, first_name, middle_name)

one_per <- anti_join(one_per, inc)

##############################


precincts <- readOGR("./raw_data/shapefiles/fl_2016_FEST",
                     "fl_2016")
precincts@data$full_id <- paste0(precincts@data$county, precincts@data$pct)

precincts <- spTransform(precincts, "+init=epsg:4326")

pings  <- SpatialPoints(one_per[c('lon','lat')], proj4string = precincts@proj4string)
one_per$precinct <- over(pings, precincts)$full_id

#### now map to census block groups

bgs <- readOGR("./raw_data/shapefiles/tl_2018_12_bg",
               "tl_2018_12_bg")

bgs <- spTransform(bgs, "+init=epsg:4326")


pings  <- SpatialPoints(one_per[c('lon','lat')], proj4string = bgs@proj4string)
one_per$block_group <- over(pings, bgs)$GEOID

saveRDS(one_per, "./temp/hills_with_ads.rds")

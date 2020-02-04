

released <- fread("./raw_data/doc/INMATE_RELEASE_ROOT_0419.csv")

parole <- fread("./raw_data/doc/OFFENDER_ROOT_0419.csv")

incarcerated <- fread("./raw_data/doc/INMATE_ACTIVE_ROOT_0419.csv")

released <- filter(released, !(DCNumber %in% parole$DCNumber),
                   !(DCNumber %in% incarcerated$DCNumber))

addresses <- fread("./raw_data/doc/INMATE_RELEASE_RESIDENCE_0419.csv")

released_with_addresses <- inner_join(released, addresses) %>% 
  filter(releasedateflag_descr == "valid release date") %>% 
  mutate(address = gsub("[#]", "", paste(AddressLine1, City)))

lats_longs <- geocode(released_with_addresses$address, override_limit = T, output = "more")

released_with_addresses <- bind_cols(released_with_addresses, lats_longs)
saveRDS(released_with_addresses, "./temp/released_with_addresses.rds")
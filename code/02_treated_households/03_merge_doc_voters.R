
address_cleaner <- fread("./raw_data/misc/address_cleaning.csv") %>%
  mutate(search = paste0(" ", search, " "),
         replace = paste0(" ", replace, " "))

######

released_with_addresses <- readRDS("./temp/released_with_addresses.rds") %>%
  mutate(address = gsub(" apt  ,", ",", ggl_address),
         address = gsub("\\s+", " ", address),
         address = gsub(" ,", ",", address),
         address = tolower(gsub("[[:punct:]]|", "", address)))

for(i in 1:nrow(address_cleaner)){
  released_with_addresses$address <- gsub(address_cleaner$search[i],
                                          address_cleaner$replace[i],
                                          released_with_addresses$address)
}

saveRDS(released_with_addresses, "./temp/released_with_addresses_clean.rds")

released_with_addresses <- readRDS("./temp/released_with_addresses_clean.rds")

latest_release <- released_with_addresses %>% 
  mutate(release_date = as.Date(substring(PrisonReleaseDate, 1, 10), "%m/%d/%Y")) %>% 
  group_by(address) %>% 
  summarize(max_release = max(release_date))

####

fl_file <- dbGetQuery(db, "select LALVOTERID, 
                           Voters_StateVoterID,
                           Residence_Addresses_AddressLine,
                           Residence_Addresses_HouseNumber,
                           Residence_Addresses_PrefixDirection,
                           Residence_Addresses_StreetName,
                           Residence_Addresses_Designator,
                           Residence_Addresses_SuffixDirection,
                           Residence_Addresses_ApartmentNum,
                           Residence_Addresses_City,
                           Residence_Addresses_State,
                           Residence_Addresses_Zip,
                           Residence_Addresses_CensusTract,
                           Residence_Addresses_CensusBlockGroup,
                           Residence_Addresses_Latitude,
                           Residence_Addresses_Longitude,
                           Voters_Gender,
                           Voters_Age,
                           Voters_BirthDate,
                           Voters_OfficialRegDate,
                           Parties_Description,
                           EthnicGroups_EthnicGroup1Desc,
                           US_Congressional_District,
                           Voters_FIPS
                           from fl") %>% 
  mutate(address = paste(Residence_Addresses_HouseNumber,
                         Residence_Addresses_PrefixDirection,
                         Residence_Addresses_StreetName,
                         Residence_Addresses_Designator,
                         Residence_Addresses_SuffixDirection,
                         "apt",
                         Residence_Addresses_ApartmentNum,
                         ",",
                         Residence_Addresses_City,
                         ",",
                         Residence_Addresses_State,
                         Residence_Addresses_Zip,
                         ", usa"),
         address = gsub(" apt  ,", ",", address),
         address = gsub("\\s+", " ", address),
         address = gsub(" ,", ",", address),
         address = tolower(gsub("[[:punct:]]|", "", address)),
         reg_date = lubridate::make_date(year = substring(Voters_OfficialRegDate, 7),
                                         month = substring(Voters_OfficialRegDate, 1, 2),
                                         day = substring(Voters_OfficialRegDate, 4, 5))) %>% 
  select(-Residence_Addresses_AddressLine,
         -Residence_Addresses_HouseNumber,
         -Residence_Addresses_PrefixDirection,
         -Residence_Addresses_StreetName,
         -Residence_Addresses_Designator,
         -Residence_Addresses_SuffixDirection,
         -Residence_Addresses_ApartmentNum,
         -Residence_Addresses_City,
         -Residence_Addresses_State,
         -Residence_Addresses_Zip)

for(i in 1:nrow(address_cleaner)){
  fl_file$address <- gsub(address_cleaner$search[i], address_cleaner$replace[i], fl_file$address)
}

fl_file$treated <- fl_file$address %in% released_with_addresses$address

fl_file <- left_join(fl_file, latest_release, by = "address")

fl_file <- fl_file %>% 
  filter(!(LALVOTERID %in% readRDS("./temp/matched_doc_file.rds")$LALVOTERID))

#### MERGE WITH OTHER VF DATA

db2 <- dbConnect(SQLite(), "D:/rolls.db")

fl_race <- dbGetQuery(db2, "select Race, Voter_ID, Gender from fl_roll_201902")
dbDisconnect(db2)
rm(db2)

fl_file <- left_join(fl_file, fl_race, by = c("Voters_StateVoterID" = "Voter_ID"))

## ANONYMIZE

scrambled_ids <- data.table(LALVOTERID = unique(fl_file$LALVOTERID),
                            voter_id_anon = paste0("V", c(1:length(unique(fl_file$LALVOTERID)))))

saveRDS(scrambled_ids, "temp/id_lookup_anon.rds")

fl_file <- left_join(fl_file, scrambled_ids) %>% 
  select(-LALVOTERID, -address,
         -Residence_Addresses_Latitude,
         -Residence_Addresses_Longitude,
         -Voters_StateVoterID)

saveRDS(fl_file, "./temp/fl_file_cleaned_addresses.rds")

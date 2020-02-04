
address_cleaner <- fread("./raw_data/misc/address_cleaning.csv") %>% 
  mutate(search = paste0(" ", search, " "),
         replace = paste0(" ", replace, " "))

######

released_with_addresses <- readRDS("./temp/released_with_addresses.rds") %>% 
  mutate(address = gsub(" apt  ,", ",", address1),
         address = gsub("\\s+", " ", address),
         address = gsub(" ,", ",", address),
         address = tolower(gsub("[[:punct:]]|", "", address)))

for(i in 1:nrow(address_cleaner)){
  released_with_addresses$address <- gsub(address_cleaner$search[i],
                                          address_cleaner$replace[i],
                                          released_with_addresses$address)
}

released_with_addresses <- filter(released_with_addresses,
                                  grepl("[0-9]", substring(address, 1, 1)))

####
tt <- dbGetQuery(db, "select Voters_OfficialRegDate from fl")

fl_file <- dbGetQuery(db, "select LALVOTERID, Voters_LastName,
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
                           Voters_OfficialRegDate,
                           Parties_Description,
                           EthnicGroups_EthnicGroup1Desc
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
         address = tolower(gsub("[[:punct:]]|", "", address))) %>% 
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

saveRDS(fl_file, "./temp/fl_file_cleaned_addresses.rds")

###############

tt <- fl_file %>% 
  mutate(reg_date = lubridate::make_date(year = substring(Voters_OfficialRegDate, 7),
                                         month = substring(Voters_OfficialRegDate, 1, 2),
                                         day = substring(Voters_OfficialRegDate, 4, 5)),
         reg_month = lubridate::make_date(year = substring(Voters_OfficialRegDate, 7),
                                          month = substring(Voters_OfficialRegDate, 1, 2),
                                          day = 1)) %>% 
  group_by(treated, reg_month) %>% 
  tally() %>% 
  group_by(treated) %>% 
  mutate(s = n / sum(n))

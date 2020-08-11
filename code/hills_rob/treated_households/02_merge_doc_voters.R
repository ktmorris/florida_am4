# 
# address_cleaner <- fread("./raw_data/misc/address_cleaning.csv") %>%
#   mutate(search = paste0(" ", search, " "),
#          replace = paste0(" ", replace, " "))
# 
# ######
# probationers <- readRDS("./temp/hills_with_ads.rds") %>%
#   mutate(address = gsub(" apt  ,", ",", address),
#          address = gsub("\\s+", " ", address),
#          address = gsub(" ,", ",", address),
#          address = tolower(gsub("[[:punct:]]|", "", address)))
# 
# released_with_addresses <- readRDS("./temp/released_with_addresses.rds") %>%
#   mutate(address = gsub(" apt  ,", ",", address1),
#          address = gsub("\\s+", " ", address),
#          address = gsub(" ,", ",", address),
#          address = tolower(gsub("[[:punct:]]|", "", address)))
# 
# for(i in 1:nrow(address_cleaner)){
#   released_with_addresses$address <- gsub(address_cleaner$search[i],
#                                           address_cleaner$replace[i],
#                                           released_with_addresses$address)
# 
#   probationers$address <- gsub(address_cleaner$search[i],
#                                address_cleaner$replace[i],
#                                probationers$address)
# }
# 
# saveRDS(probationers, "./temp/probation_with_addresses_clean.rds")
# saveRDS(released_with_addresses, "./temp/released_with_addresses_clean.rds")

probationers <- readRDS("./temp/probation_with_addresses_clean.rds") %>% 
  select(address, release_date = disposition_date)

released_with_addresses <- readRDS("./temp/released_with_addresses_clean.rds") %>% 
  mutate(release_date = as.Date(substring(PrisonReleaseDate, 1, 10), "%m/%d/%Y")) %>% 
  select(release_date, address)

prob_only <- filter(probationers, !(address %in% released_with_addresses$address))$address

saveRDS(prob_only, "temp/probation_only_ads_hills.rds")

latest_release <- rbind(released_with_addresses, probationers) %>% 
  group_by(address) %>% 
  summarize(max_release = max(release_date))

####

fl_file <- dbGetQuery(db, "select LALVOTERID, 
                           Voters_StateVoterID,
                           Voters_LastName,
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
                           from fl where Voters_FIPS == 57") %>% 
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

fl_file$treated <- fl_file$address %in% latest_release$address

fl_file <- left_join(fl_file, latest_release, by = "address")

fl_file$prob <- fl_file$address %in% prob_only

saveRDS(fl_file, "./temp/hills_file_cleaned_addresses.rds")

fl_file <- readRDS("./temp/fl_file_cleaned_addresses.rds") %>% 
  mutate(Gender = ifelse(is.na(Gender), Voters_Gender, Gender),
         Race = ifelse(is.na(Race) & EthnicGroups_EthnicGroup1Desc == "East and South Asian",
                       2, Race),
         Race = ifelse(is.na(Race) & EthnicGroups_EthnicGroup1Desc == "European",
                       5, Race),
         Race = ifelse(is.na(Race) & EthnicGroups_EthnicGroup1Desc == "Hispanic and Portuguese",
                       4, Race),
         Race = ifelse(is.na(Race) & EthnicGroups_EthnicGroup1Desc == "Likely African-American",
                       3, Race),
         Race = ifelse(is.na(Race), 6, Race),
         GEOID = paste0("12", str_pad(Voters_FIPS, width = 3, side = "left", pad = "0"),
                        str_pad(Residence_Addresses_CensusTract, width = 6, side = "left", pad = "0"),
                        Residence_Addresses_CensusBlockGroup)) %>% 
  select(-EthnicGroups_EthnicGroup1Desc, -Voters_Gender)


## downloading census data works better county-by-county
## commenting out because it takes so long
# census_data <- rbindlist(lapply(filter(fips_codes, state == "FL")$county_code, function(c){
#   get_basic_census_stats(geo = "block group", year = 2018, state = "FL", county = c)
# }))
# saveRDS(census_data, "./temp/block_group_census_data.RDS")

census_data <- readRDS("./temp/block_group_census_data.RDS")

fl_file <- left_join(fl_file, census_data)

fl_file_pre_match <- fl_file %>% 
  mutate(white = Race == 5,
         black = Race == 3,
         latino = Race == 4,
         asian = Race == 2,
         female = Gender == "F",
         male = Gender == "M",
         dem = Parties_Description == "Democratic",
         rep = Parties_Description == "Republican",
         reg_date = as.Date(Voters_OfficialRegDate, "%m/%d/%Y")) %>% 
  select(voter_id_anon, treated, white, black, latino, asian, female, male,
         reg_date, age = Voters_Age, dem, rep, median_income,
         some_college)


fl_file_pre_match <- fl_file_pre_match[complete.cases(fl_file_pre_match), ]

fl_file_pre_match <- fl_file_pre_match %>% 
  mutate_at(vars(white, black, latino, asian, female, male, dem, rep, treated),
            ~ . * 1) %>% 
  mutate(reg_date = as.integer(reg_date - as.Date("2000-01-01")))

fl_file_pre_match <- left_join(fl_file_pre_match, select(fl_file, voter_id_anon, max_release,
                                                         US_Congressional_District))

saveRDS(fl_file_pre_match, "./temp/fl_file_pre_match.rds")


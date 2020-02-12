## get real race gender from file
db2 <- dbConnect(SQLite(), "D:/rolls.db")
fl_race <- dbGetQuery(db2, "select Race, Voter_ID, Gender, Precinct, County_Code from fl_roll_201902")
dbDisconnect(db2)
rm(db2)
### find precinct demos

fl_file <- dbGetQuery(db, "select LALVOTERID,
                           Voters_StateVoterID,
                           Voters_Gender,
                           Voters_Age,
                           Parties_Description,
                           Residence_Addresses_CensusTract,
                           Residence_Addresses_CensusBlockGroup,
                           Voters_FIPS,
                           US_Congressional_District,
                           Voters_OfficialRegDate
                           from fl where Voters_Active == 'A'")

fl_file <- inner_join(fl_file, fl_race, by = c("Voters_StateVoterID" = "Voter_ID")) %>% 
  mutate(GEOID = paste0("12", str_pad(Voters_FIPS, width = 3, side = "left", pad = "0"),
                        str_pad(Residence_Addresses_CensusTract, width = 6, side = "left", pad = "0"),
                        Residence_Addresses_CensusBlockGroup),
         white = Race == 5,
         black = Race == 3,
         latino = Race == 4,
         asian = Race == 2,
         female = Gender == "F",
         male = Gender == "M",
         dem = Parties_Description == "Democratic",
         rep = Parties_Description == "Republican") %>% 
  rename(age = Voters_Age,
         county = County_Code)

rm(fl_race)
## downloading census data works better county-by-county
## commenting out because it takes so long
# census_data <- rbindlist(lapply(filter(fips_codes, state == "FL")$county_code, function(c){
#   get_basic_census_stats(geo = "block group", year = 2018, state = "FL", county = c)
# }))
# saveRDS(census_data, "./temp/block_group_census_data.RDS")

census_data <- readRDS("./temp/block_group_census_data.RDS") %>% 
  select(GEOID, median_income, some_college, unem)

fl_file <- inner_join(fl_file, census_data)

monthly <- fl_file %>% 
  mutate(reg_month = make_date(year = year(as.Date(Voters_OfficialRegDate, "%m/%d/%Y")),
                               month = month(as.Date(Voters_OfficialRegDate, "%m/%d/%Y")),
                               day = 1),
         precinct = str_pad(Precinct, width = 10, side = "left", pad = "0"),
         cp = paste0(county, precinct)) %>% 
  filter(reg_month >= "2010-01-01",
         reg_month <= "2018-10-01") %>% 
  group_by(cp, reg_month) %>% 
  summarize(registrations = n()) %>% 
  ungroup() %>% 
  complete(cp, reg_month) %>% 
  mutate(registrations = ifelse(is.na(registrations), 0, registrations))

precinct_level <- fl_file %>% 
  mutate(precinct = str_pad(Precinct, width = 10, side = "left", pad = "0"),
         cp = paste0(county, precinct)) %>% 
  group_by(county, cp) %>% 
  summarize_at(vars(white, black, latino, asian,
                    female, male, dem, rep, age,
                    median_income, some_college, unem),
               mean, na.rm = T)

cd <- fl_file %>% 
  mutate(precinct = str_pad(Precinct, width = 10, side = "left", pad = "0"),
         cp = paste0(county, precinct)) %>% 
  group_by(cp, US_Congressional_District) %>% 
  summarize(voter_count = n()) %>% 
  group_by(cp) %>% 
  filter(voter_count == max(voter_count)) %>% 
  group_by(cp) %>% 
  filter(row_number() == 1) %>% 
  select(-voter_count)

monthly <- left_join(monthly, inner_join(precinct_level, cd))

#### doc data

doc <- readRDS("./temp/released_with_addresses.rds") %>% 
  mutate(county = substring(precinct, 1, 3),
         precinct = str_pad(substring(precinct, 4), width = 10, side = "left", pad = "0"),
         cp = paste0(county, precinct),
         PrisonReleaseDate = as.Date(substring(PrisonReleaseDate, 1, 10), "%m/%d/%Y")) %>% 
  filter(!is.na(cp)) %>% 
  group_by(address1) %>% 
  mutate(big_release = n() >= 5) %>% 
  group_by(cp) %>% 
  summarize(all_doc = n(),
            small_res_doc = sum(1 - big_release))
##############

monthly <- left_join(monthly, doc) %>% 
  mutate_at(vars(all_doc, small_res_doc), ~ ifelse(is.na(.), 0, .))

months <- data.frame(reg_month = unique(monthly$reg_month)) %>% 
  arrange(reg_month) %>% 
  mutate(trend = row_number())

monthly <- left_join(monthly, months)

monthly$median_income <- monthly$median_income / 10000

m1 <- glm(registrations ~ I(reg_month > "2018-01-01") * small_res_doc + reg_month +
              white + black + latino + asian +
              female + male + dem + rep + age +
              median_income + some_college + unem +
            as.factor(US_Congressional_District) + reg_month, family = "poisson",
            data = monthly)

m2 <- glm.nb(registrations ~ I(reg_month > "2018-01-01") * small_res_doc + trend +
                     white + black + latino + asian +
                     female + male + dem + rep + age +
                     median_income + some_college + unem +
                     as.factor(US_Congressional_District),
                   data = monthly)
saveRDS(m2, "./temp/neg_bin.rds")


#####

monthly_ll <- monthly %>% 
  group_by(reg_month) %>% 
  summarize(regs = mean(registrations))
ggplot(monthly_ll, aes(x = reg_month, y = regs)) + geom_line()

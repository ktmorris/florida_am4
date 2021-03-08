## THIS FILE MERGES THE VOTER FILE DATA WITH 

## get real race gender precinct county from file
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
                           US_Congressional_District
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

### historical turnout from vf
history <- dbConnect(SQLite(), "D:/national_file_history.db")
fl_history <- dbGetQuery(history, "select LALVOTERID,
                                   General_2018_11_06,
                                   General_2016_11_08,
                                   General_2014_11_04,
                                   General_2012_11_06,
                                   General_2010_11_02
                                   from fl_history_18")
dbDisconnect(history)
rm(history)
fl_file <- inner_join(fl_file, fl_history, by = "LALVOTERID") %>% 
  select(-LALVOTERID, -Voters_StateVoterID)

saveRDS(fl_file, "temp/anon_for_precincts.rds")
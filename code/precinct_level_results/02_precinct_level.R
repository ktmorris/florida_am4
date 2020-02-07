## get real race gender from file
db2 <- dbConnect(SQLite(), "D:/rolls.db")

fl_race <- dbGetQuery(db2, "select Race, Voter_ID, Gender from fl_roll_201902")
dbDisconnect(db2)
rm(db2)
### find precinct demos

fl_file <- dbGetQuery(db, "select LALVOTERID,
                           Voters_StateVoterID,
                           Voters_Gender,
                           Voters_Age,
                           Parties_Description,
                           Precinct,
                           County,
                           Residence_Addresses_CensusTract,
                           Residence_Addresses_CensusBlockGroup,
                           Voters_FIPS,
                           US_Congressional_District
                           from fl where Voters_Active == 'A'")

fl_file <- inner_join(fl_file, fl_race, by = c("Voters_StateVoterID" = "Voter_ID"))

rm(fl_race)

counties <- fread("D:/rolls/florida/counties.csv") %>% 
  mutate(name = toupper(name))

fl_file <- inner_join(fl_file, counties, c("County" = "name")) %>% 
  select(-County) %>% 
  rename(county = code) %>% 
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
  rename(age = Voters_Age)

## downloading census data works better county-by-county
## commenting out because it takes so long
# census_data <- rbindlist(lapply(filter(fips_codes, state == "FL")$county_code, function(c){
#   get_basic_census_stats(geo = "block group", year = 2018, state = "FL", county = c)
# }))
# saveRDS(census_data, "./temp/block_group_census_data.RDS")

census_data <- readRDS("./temp/block_group_census_data.RDS") %>% 
  select(GEOID, median_income, some_college, unem)

fl_file <- inner_join(fl_file, census_data)

precinct_level <- fl_file %>% 
  group_by(county, Precinct,US_Congressional_District) %>% 
  summarize_at(vars(white, black, latino, asian,
                    female, male, dem, rep, age,
                    median_income, some_college, unem),
               mean, na.rm = T) %>% 
  mutate(precinct = str_pad(Precinct, width = 10, side = "left", pad = "0"),
         cp = paste0(county, precinct)) %>% 
  select(-Precinct)

pc <- fl_file %>% 
  group_by(county, Precinct,US_Congressional_District) %>% 
  summarize(voter_count = n())

precinct_level <- inner_join(precinct_level, pc)
rm(pc)
### read in results for am 4

results <- rbindlist(lapply(
  list.files("./raw_data/election_results/precinctlevelelectionresults2018gen/", full.names = T),
  fread)) %>% 
  filter(V12 == "Amendment No. 4: Voting Restoration Amendment",
         V18 %in% c(10, 20)) %>% 
  select(county = V1, precinct = V6, yn = V18, votes = V19) %>% 
  group_by(county, precinct) %>% 
  summarize(share_yes = sum(votes * (yn == 10)) / sum(votes),
            votes = sum(votes)) %>% 
  filter(votes != 0) %>% 
  mutate(precinct = str_pad(precinct, width = 10, side = "left", pad = "0"),
         cp = paste0(county, precinct))

### find highest turnout

results_to <- rbindlist(lapply(
  list.files("./raw_data/election_results/precinctlevelelectionresults2018gen/", full.names = T),
  fread)) %>% 
  filter(!(V18 %in% c(901, 902))) %>% 
  group_by(contest = V14, county = V1, precinct = V6) %>% 
  summarize(highest_votes = sum(V19)) %>% 
  group_by(county, precinct) %>% 
  filter(highest_votes == max(highest_votes)) %>% 
  group_by(county, precinct) %>% 
  filter(row_number() == 1) %>% 
  select(-contest) %>% 
  ungroup() %>% 
  filter(highest_votes != 0) %>% 
  mutate(precinct = str_pad(precinct, width = 10, side = "left", pad = "0"),
         cp = paste0(county, precinct))
  

### combine

results_demos <- inner_join(inner_join(results, results_to), precinct_level) %>% 
  filter(!is.na(cp))

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


results_demos <- left_join(results_demos, doc) %>% 
  mutate_at(vars(all_doc, small_res_doc), ~ ifelse(is.na(.), 0, .))

results_demos$US_Congressional_District <- as.factor(results_demos$US_Congressional_District)
results_demos$to <- results_demos$votes / results_demos$voter_count
results_demos$highest_to <- results_demos$highest_votes / results_demos$voter_count
results_demos$roll_off <- results_demos$votes / results_demos$highest_votes
###########

m0 <- lm_robust(share_yes ~ small_res_doc, data = results_demos)

marg <- ggeffect(model = m0, "small_res_doc [all]")

ggplot() + 
  geom_histogram(aes(x = small_res_doc, y = ..count../2500), position="identity", linetype=1,
                 fill="gray60", data = results_demos, alpha=0.5, bins = 30) + 
  geom_line(aes(x = x, y = predicted), data = marg) +
  geom_line(aes(x = x, y = conf.low), linetype = 0, data = marg) +
  geom_line(aes(x = x, y = conf.high), linetype = 0, data = marg) +
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), fill= "blue", alpha=0.25, data = marg) +
  xlab("Number of Formerly Incarcerated Residents") +
  ylab("Turnout") + scale_x_continuous(labels = comma, limits = c(0, 300)) +
  scale_y_continuous(labels = percent) +
  ggtitle("Marginal Effect of Disenfranchised Voters on Turnout for Amendment 4") +
  labs(caption = "Notes: Distribution of number of formerly incarcerated residents shown at bottom.") +
  geom_hline(yintercept = mean(results_demos$share_yes)) + theme_bw() +
  theme(plot.caption = element_text(hjust = 0))
###########
m1 <- lm_robust(share_yes ~ small_res_doc + 
                  white + black + latino + asian +
                  female + male + dem + rep + age +
                  median_income + some_college + unem +
                  US_Congressional_District, data = results_demos,
                clusters = US_Congressional_District)

marg <- ggeffect(model = m1, c("small_res_doc [all]"))

ggplot() + 
  geom_histogram(aes(x = small_res_doc, y = ..count../2500), position="identity", linetype=1,
                 fill="gray60", data = results_demos, alpha=0.5, bins = 30) + 
  geom_line(aes(x = x, y = predicted), data = marg) +
  geom_line(aes(x = x, y = conf.low), linetype = 0, data = marg) +
  geom_line(aes(x = x, y = conf.high), linetype = 0, data = marg) +
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), fill= "blue", alpha=0.25, data = marg) +
  xlab("Number of Formerly Incarcerated Residents") +
  ylab("Support for Amendment 4") + scale_x_continuous(labels = comma, limits = c(0, 300)) +
  scale_y_continuous(labels = percent) +
  ggtitle("Marginal Effect of Disenfranchised Voters on Turnout for Amendment 4") +
  labs(caption = "Notes: Distribution of number of formerly incarcerated residents shown at bottom.") +
  theme(plot.caption = element_text(hjust = 0))

#############
m2 <- lmer(to ~ small_res_doc + white + black + latino + asian +
                  female + male + dem + rep + age +
                  median_income + some_college + unem +
                  (1 | US_Congressional_District), data = results_demos)

marg <- ggeffect(model = m2, "small_res_doc [all]")

ggplot() + 
  geom_histogram(aes(x = small_res_doc, y = ..count../2500), position="identity", linetype=1,
                 fill="gray60", data = results_demos, alpha=0.5, bins = 30) + 
  geom_line(aes(x = x, y = predicted), data = marg) +
  geom_line(aes(x = x, y = conf.low), linetype = 0, data = marg) +
  geom_line(aes(x = x, y = conf.high), linetype = 0, data = marg) +
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), fill= "blue", alpha=0.25, data = marg) +
  xlab("Number of Formerly Incarcerated Residents") +
  ylab("Turnout") + scale_x_continuous(labels = comma, limits = c(0, 300)) +
  scale_y_continuous(labels = percent) +
  ggtitle("Marginal Effect of Disenfranchised Voters on Support for Amendment 4") +
  labs(caption = "Notes: Distribution of number of formerly incarcerated residents shown at bottom.") +
  geom_hline(yintercept = mean(results_demos$share_yes)) + theme_bw() +
  theme(plot.caption = element_text(hjust = 0))


#############
m3 <- lmer(roll_off ~ small_res_doc + 
                  white + black + latino + asian +
                  female + male + dem + rep + age +
                  median_income + some_college + unem +
                  (1 | US_Congressional_District), data = results_demos)

marg <- ggeffect(model = m3, "small_res_doc")

ggplot() + 
  geom_histogram(aes(x = small_res_doc, y = ..count../5000), position="identity", linetype=1,
                 fill="gray60", data = results_demos, alpha=0.5, bins = 30) + 
  geom_line(aes(x = x, y = predicted), data = marg) +
  geom_line(aes(x = x, y = conf.low), linetype = 0, data = marg) +
  geom_line(aes(x = x, y = conf.high), linetype = 0, data = marg) +
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), fill= "blue", alpha=0.25, data = marg) +
  xlab("Number of Formerly Incarcerated Residents") +
  ylab("Support for Amendment 4") + scale_x_continuous(labels = comma, limits = c(0, 300)) +
  scale_y_continuous(labels = percent) +
  ggtitle("Marginal Effect of Disenfranchised Voters on Support for Amendment 4") +
  labs(caption = "Notes: Distribution of number of formerly incarcerated residents shown at bottom.") +
  theme(plot.caption = element_text(hjust = 0))

#############

history <- dbConnect(SQLite(), "D:/national_file_history.db")
fl_history <- dbGetQuery(history, "select LALVOTERID,
                                   General_2018_11_06,
                                   General_2016_11_08,
                                   General_2014_11_04,
                                   General_2012_11_06,
                                   General_2010_11_02
                                   from fl_history_18")

fl_file <- left_join(fl_file, fl_history, by = "LALVOTERID")

bg_level <- fl_file %>% 
  group_by(county, GEOID) %>% 
  summarize_at(vars(white, black, latino, asian,
                    female, male, dem, rep, age),
               mean, na.rm = T)

bg2 <- fl_file %>% 
  group_by(county, GEOID) %>% 
  summarize_at(vars(General_2018_11_06,
                    General_2016_11_08,
                    General_2014_11_04,
                    General_2012_11_06,
                    General_2010_11_02),
               ~ sum(. == "Y"))

bg_level <- inner_join(bg_level, bg2)
rm(bg2)

doc_bg <- readRDS("./temp/released_with_addresses.rds") %>% 
  mutate(county = substring(precinct, 1, 3),
         PrisonReleaseDate = as.Date(substring(PrisonReleaseDate, 1, 10), "%m/%d/%Y")) %>% 
  filter(!is.na(block_group)) %>% 
  group_by(address1) %>% 
  mutate(big_release = n() >= 5) %>% 
  group_by(GEOID = block_group) %>% 
  summarize(all_doc = n(),
            small_res_doc = sum(1 - big_release))

bg_level <- left_join(bg_level, doc_bg) %>% 
  mutate_at(vars(all_doc, small_res_doc), ~ ifelse(is.na(.), 0, .))

cvap <- fread("./raw_data/misc/CVAP_2014-2018_ACS_csv_files/BlockGr.csv") %>% 
  mutate(GEOID = substring(geoid, 8)) %>% 
  filter(substring(GEOID, 1, 2) == "12",
         lntitle == "Total") %>% 
  select(GEOID, cvap = cvap_est)

bg_level <- inner_join(bg_level, cvap)

bg_level$to_18 <- bg_level$General_2018_11_06 / bg_level$cvap
bg_level$to_16 <- bg_level$General_2016_11_08 / bg_level$cvap
bg_level$to_14 <- bg_level$General_2014_11_04 / bg_level$cvap
bg_level$to_12 <- bg_level$General_2012_11_06 / bg_level$cvap
bg_level$to_10 <- bg_level$General_2010_11_02 / bg_level$cvap


bg_level <- filter(bg_level, !is.infinite(to))

bg_level <- inner_join(bg_level, census_data)

summary(lm(to_18 ~ small_res_doc + 
             white + black + latino + asian +
             female + male + dem + rep + age +
             median_income + some_college + unem +
             to_16 + to_14 + to_12 + to_10, bg_level))

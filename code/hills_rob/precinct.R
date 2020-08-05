## get real race gender from file
db2 <- dbConnect(SQLite(), "D:/rolls.db")
fl_race <- dbGetQuery(db2, "select Race, Voter_ID, Gender, Precinct, County_Code
                      from fl_roll_201902 where County_Code == 'HIL'")
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
                           from fl where Voters_Active == 'A' and Voters_FIPS == 57")

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
                                   General_2016_11_08,
                                   General_2014_11_04,
                                   General_2012_11_06,
                                   General_2010_11_02
                                   from fl_history_18") %>% 
  filter(LALVOTERID %in% fl_file$LALVOTERID)
dbDisconnect(history)
rm(history)
## downloading census data works better county-by-county
## commenting out because it takes so long
# census_data <- rbindlist(lapply(filter(fips_codes, state == "FL")$county_code, function(c){
#   get_basic_census_stats(geo = "block group", year = 2018, state = "FL", county = c)
# }))
# saveRDS(census_data, "./temp/block_group_census_data.RDS")

census_data <- readRDS("./temp/block_group_census_data.RDS") %>% 
  select(GEOID, median_income, some_college, unem)

fl_file <- inner_join(fl_file, census_data)
fl_file <- inner_join(fl_file, fl_history, by = "LALVOTERID")

precinct_level <- fl_file %>% 
  mutate(precinct = str_pad(Precinct, width = 10, side = "left", pad = "0"),
         cp = paste0(county, precinct)) %>% 
  group_by(county, cp) %>% 
  mutate_at(vars(starts_with("General")), ~ ifelse(. == "Y", 1, 0)) %>% 
  summarize_at(vars(white, black, latino, asian,
                    female, male, dem, rep, age,
                    median_income, some_college, unem,
                    starts_with("General")),
               mean, na.rm = T)

pc <- fl_file %>% 
  mutate(precinct = str_pad(Precinct, width = 10, side = "left", pad = "0"),
         cp = paste0(county, precinct)) %>% 
  group_by(county, cp) %>% 
  summarize(voter_count = n())

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

precinct_level <- inner_join(precinct_level, pc)
precinct_level <- inner_join(precinct_level, cd)
rm(pc, cd, fl_history, census_data)

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
ads <- readRDS("./temp/released_with_addresses.rds") %>% 
  mutate(county = substring(precinct, 1, 3),
         precinct = str_pad(substring(precinct, 4), width = 10, side = "left", pad = "0"),
         cp = paste0(county, precinct),
         PrisonReleaseDate = as.Date(substring(PrisonReleaseDate, 1, 10), "%m/%d/%Y")) %>% 
  group_by(address1) %>% 
  tally()

doc <- readRDS("./temp/released_with_addresses.rds") %>% 
  mutate(county = substring(precinct, 1, 3),
         precinct = str_pad(substring(precinct, 4), width = 10, side = "left", pad = "0"),
         cp = paste0(county, precinct),
         PrisonReleaseDate = as.Date(substring(PrisonReleaseDate, 1, 10), "%m/%d/%Y")) %>% 
  filter(!is.na(cp),
         PrisonReleaseDate <= "2018-11-06") %>% 
  group_by(address1) %>% 
  mutate(big_release = n() >= 5) %>% 
  group_by(cp) %>% 
  summarize(all_doc = n(),
            small_res_doc = sum(1 - big_release),
            big_release = sum(big_release))

doc_recent <- readRDS("./temp/released_with_addresses.rds") %>% 
  mutate(county = substring(precinct, 1, 3),
         precinct = str_pad(substring(precinct, 4), width = 10, side = "left", pad = "0"),
         cp = paste0(county, precinct),
         PrisonReleaseDate = as.Date(substring(PrisonReleaseDate, 1, 10), "%m/%d/%Y")) %>% 
  filter(!is.na(cp),
         PrisonReleaseDate >= "2015-01-01",
         PrisonReleaseDate <= "2018-11-06") %>% 
  group_by(address1) %>% 
  mutate(big_release_recent = n() >= 5) %>% 
  group_by(cp) %>% 
  summarize(all_doc_recent = n(),
            small_res_doc_recent = sum(1 - big_release_recent),
            big_release_recent = sum(big_release_recent))


results_demos <- left_join(results_demos, full_join(doc, doc_recent)) %>% 
  mutate_at(vars(all_doc, small_res_doc), ~ ifelse(is.na(.), 0, .))
###########################################
probation <- readRDS("./temp/hills_with_ads.rds") %>% 
  mutate(county = substring(precinct, 1, 3),
         precinct = str_pad(substring(precinct, 4), width = 10, side = "left", pad = "0"),
         cp = paste0(county, precinct)) %>% 
  group_by(cp) %>% 
  summarize(probationers = n())


results_demos <- left_join(results_demos, probation) %>% 
  mutate(probationers = ifelse(is.na(probationers), 0, probationers),
         tot_disenf = probationers + small_res_doc)
############################################

results_demos$to <- results_demos$votes / results_demos$voter_count
results_demos$highest_to <- results_demos$highest_votes / results_demos$voter_count
results_demos$roll_off <- 1- (results_demos$votes / results_demos$highest_votes)
results_demos$US_Congressional_District <- as.factor(results_demos$US_Congressional_District)
results_demos$median_income <- results_demos$median_income / 10000


results_demos$prob_voter <- results_demos$probationers / results_demos$voter_count
results_demos$inc_voter <- results_demos$small_res_doc / results_demos$voter_count
######################


eq2 <- as.character(as.expression(substitute(paste(italic(R)^2 == r2), 
                  list(r2 = format(summary(lm(prob_voter ~ inc_voter - 1,
                                              filter(results_demos, to <= 1)))$r.squared, digits = 3)))))

corr <- ggplot(filter(results_demos, to <= 1), aes(x = inc_voter, y = prob_voter)) +
  geom_point(shape = 1, size = 3) +
  lims(x = c(0, 0.1), y = c(0, 0.2)) +
  geom_smooth(method = "lm", se = F, color = "black", formula = y ~ x - 1) +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10")) +
  labs(x = "Formerly Incarcerated Residents per Voter",
       y = "Residents Sentenced to Felony Probation per Voter") +
  geom_text(x = .0975, y = 0.15, label = eq2, parse = TRUE, check_overlap = TRUE,
            family = "LM Roman 10")

saveRDS(corr, "temp/correlation_plot.rds")
###########
m1 <- lm(share_yes ~ small_res_doc + white + black + latino + asian +
           female + male + dem + rep + age +
           median_income + some_college + unem +
           General_2016_11_08 + General_2014_11_04 +
           General_2012_11_06 + General_2010_11_02 +
           US_Congressional_District, data = filter(results_demos, to <= 1))

m1_rob <- lm_robust(share_yes ~ small_res_doc + white + black + latino + asian +
                      female + male + dem + rep + age +
                      median_income + some_college + unem +
                      General_2016_11_08 + General_2014_11_04 +
                      General_2012_11_06 + General_2010_11_02 +
                      US_Congressional_District, data = filter(results_demos, to <= 1),
                    clusters = US_Congressional_District)
m1_ses <- data.frame(
  summary(m1_rob)$coefficients)[, 2]

m1b <- lm(share_yes ~ tot_disenf + white + black + latino + asian +
           female + male + dem + rep + age +
           median_income + some_college + unem +
           General_2016_11_08 + General_2014_11_04 +
           General_2012_11_06 + General_2010_11_02 +
           US_Congressional_District, data = filter(results_demos, to <= 1))

m1b_rob <- lm_robust(share_yes ~ tot_disenf + white + black + latino + asian +
                      female + male + dem + rep + age +
                      median_income + some_college + unem +
                      General_2016_11_08 + General_2014_11_04 +
                      General_2012_11_06 + General_2010_11_02 +
                      US_Congressional_District, data = filter(results_demos, to <= 1),
                    clusters = US_Congressional_District)
m1b_ses <- data.frame(
  summary(m1b_rob)$coefficients)[, 2]

save(m1, m1_ses, m1b, m1b_ses, file = "./temp/support_reg_hills.rdata")

#############
m2 <- lm(to ~ small_res_doc + white + black + latino + asian +
            female + male + dem + rep + age +
            median_income + some_college + unem +
            General_2016_11_08 + General_2014_11_04 +
            General_2012_11_06 + General_2010_11_02 +
            US_Congressional_District, data = filter(results_demos, to <= 1))

m2_rob <- lm_robust(to ~ small_res_doc + white + black + latino + asian +
                       female + male + dem + rep + age +
                       median_income + some_college + unem +
                       General_2016_11_08 + General_2014_11_04 +
                       General_2012_11_06 + General_2010_11_02 +
                       US_Congressional_District, data = filter(results_demos, to <= 1),
                     clusters = US_Congressional_District)

m2_ses <- data.frame(
  summary(m2_rob)$coefficients)[, 2]

m2b <- lm(to ~ tot_disenf + white + black + latino + asian +
           female + male + dem + rep + age +
           median_income + some_college + unem +
           General_2016_11_08 + General_2014_11_04 +
           General_2012_11_06 + General_2010_11_02 +
           US_Congressional_District, data = filter(results_demos, to <= 1))

m2b_rob <- lm_robust(to ~ tot_disenf + white + black + latino + asian +
                      female + male + dem + rep + age +
                      median_income + some_college + unem +
                      General_2016_11_08 + General_2014_11_04 +
                      General_2012_11_06 + General_2010_11_02 +
                      US_Congressional_District, data = filter(results_demos, to <= 1),
                    clusters = US_Congressional_District)

m2b_ses <- data.frame(
  summary(m2b_rob)$coefficients)[, 2]


save(m2, m2_ses, m2b, m2b_ses, file = "./temp/precinct_turnout_hills.rdata")

#############
m3 <- lm(roll_off ~ small_res_doc + white + black + latino + asian +
           female + male + dem + rep + age +
           median_income + some_college + unem +
           General_2016_11_08 + General_2014_11_04 +
           General_2012_11_06 + General_2010_11_02 +
           US_Congressional_District, data = filter(results_demos, to <= 1))

m3_rob <- lm_robust(roll_off ~ small_res_doc + white + black + latino + asian +
                      female + male + dem + rep + age +
                      median_income + some_college + unem +
                      General_2016_11_08 + General_2014_11_04 +
                      General_2012_11_06 + General_2010_11_02 +
                      US_Congressional_District, data = filter(results_demos, to <= 1),
                    clusters = US_Congressional_District)

m3_ses <- data.frame(
  summary(m3_rob)$coefficients)[, 2]

m3b <- lm(roll_off ~ tot_disenf + white + black + latino + asian +
           female + male + dem + rep + age +
           median_income + some_college + unem +
           General_2016_11_08 + General_2014_11_04 +
           General_2012_11_06 + General_2010_11_02 +
           US_Congressional_District, data = filter(results_demos, to <= 1))

m3b_rob <- lm_robust(roll_off ~ tot_disenf + white + black + latino + asian +
                      female + male + dem + rep + age +
                      median_income + some_college + unem +
                      General_2016_11_08 + General_2014_11_04 +
                      General_2012_11_06 + General_2010_11_02 +
                      US_Congressional_District, data = filter(results_demos, to <= 1),
                    clusters = US_Congressional_District)

m3b_ses <- data.frame(
  summary(m3b_rob)$coefficients)[, 2]

save(m3, m3_ses, m3b, m3b_ses, file = "./temp/precinct_rolloff_hills.rdata")


#############

history <- dbConnect(SQLite(), "D:/national_file_history.db")
fl_history <- dbGetQuery(history, "select LALVOTERID,
                                   General_2018_11_06
                                   from fl_history_18")

fl_file <- left_join(fl_file, fl_history, by = "LALVOTERID")

bg_level <- fl_file %>% 
  group_by(GEOID) %>% 
  summarize_at(vars(female, male, dem, rep, age),
               mean, na.rm = T)

bg2 <- fl_file %>% 
  group_by(GEOID) %>% 
  summarize_at(vars(General_2018_11_06,
                    General_2016_11_08,
                    General_2014_11_04,
                    General_2012_11_06,
                    General_2010_11_02),
               ~ sum(. == "Y"))

cd <- fl_file %>% 
  group_by(GEOID, US_Congressional_District) %>% 
  summarize(voter_count = n()) %>% 
  group_by(GEOID) %>% 
  filter(voter_count == max(voter_count)) %>% 
  group_by(GEOID) %>% 
  filter(row_number() == 1) %>% 
  select(-voter_count)

bg_level <- inner_join(bg_level, inner_join(bg2, cd))
rm(bg2, cd)

doc_bg <- readRDS("./temp/released_with_addresses.rds") %>% 
  mutate(county = substring(precinct, 1, 3),
         PrisonReleaseDate = as.Date(substring(PrisonReleaseDate, 1, 10), "%m/%d/%Y")) %>% 
  filter(!is.na(block_group),
         PrisonReleaseDate <= "2018-11-06") %>% 
  group_by(address1) %>% 
  mutate(big_release = n() >= 5,
         years_since = ifelse(big_release, NA, 2018 - year(PrisonReleaseDate))) %>% 
  group_by(GEOID = block_group) %>% 
  summarize(years_since = mean(years_since, na.rm = T),
            all_doc = n(),
            small_res_doc = sum(1 - big_release))

doc_bg_recent <- readRDS("./temp/released_with_addresses.rds") %>% 
  mutate(county = substring(precinct, 1, 3),
         PrisonReleaseDate = as.Date(substring(PrisonReleaseDate, 1, 10), "%m/%d/%Y")) %>% 
  filter(!is.na(block_group),
         PrisonReleaseDate >= "2015-01-01",
         PrisonReleaseDate <= "2018-11-06") %>% 
  group_by(address1) %>% 
  mutate(big_release = n() >= 5) %>% 
  group_by(GEOID = block_group) %>% 
  summarize(all_doc_recent = n(),
            small_res_doc_recent = sum(1 - big_release))

bg_level <- left_join(bg_level, full_join(doc_bg, doc_bg_recent)) %>% 
  mutate_at(vars(all_doc, small_res_doc), ~ ifelse(is.na(.), 0, .))
######################################
probation <- readRDS("./temp/hills_with_ads.rds") %>% 
  group_by(GEOID = block_group) %>% 
  summarize(probationers = n())


bg_level <- left_join(bg_level, probation) %>% 
  mutate(probationers = ifelse(is.na(probationers), 0, probationers),
         tot_disenf = probationers + small_res_doc)
######################################
cvap <- fread("../regular_data/CVAP_2014-2018_ACS_csv_files/BlockGr.csv") %>% 
  mutate(GEOID = substring(geoid, 8)) %>% 
  filter(substring(GEOID, 1, 2) == "12",
         lntitle == "Total") %>% 
  select(GEOID, cvap = cvap_est)

bg_level <- inner_join(bg_level, cvap)

bg_level$cvap <- bg_level$cvap - bg_level$all_doc - bg_level$probationers
bg_level$cvap2 <- bg_level$cvap - bg_level$all_doc_recent

bg_level$to_18 <- bg_level$General_2018_11_06 / bg_level$cvap
bg_level$to_16 <- bg_level$General_2016_11_08 / bg_level$cvap
bg_level$to_14 <- bg_level$General_2014_11_04 / bg_level$cvap
bg_level$to_12 <- bg_level$General_2012_11_06 / bg_level$cvap
bg_level$to_10 <- bg_level$General_2010_11_02 / bg_level$cvap
bg_level$to_18_2 <- bg_level$General_2018_11_06 / bg_level$cvap2
bg_level$to_16_2 <- bg_level$General_2016_11_08 / bg_level$cvap2
bg_level$to_14_2 <- bg_level$General_2014_11_04 / bg_level$cvap2
bg_level$to_12_2 <- bg_level$General_2012_11_06 / bg_level$cvap2
bg_level$to_10_2 <- bg_level$General_2010_11_02 / bg_level$cvap2
bg_level$US_Congressional_District <- as.factor(bg_level$US_Congressional_District)

bg_level <- filter(bg_level, !is.infinite(to_18))

bg_level <- inner_join(bg_level, readRDS("./temp/block_group_census_data.RDS"),
                       by = "GEOID")

bg_level$median_income <- bg_level$median_income / 10000


bg_level <- rename(bg_level,
                   white = nh_white,
                   black = nh_black)

m1bg <- lm(to_18 ~ small_res_doc +
           white + black + latino + asian +
           female + male + dem + rep + age +
           median_income + some_college + unem +
           to_16 + to_14 + to_12 + to_10 + 
           US_Congressional_District,
         data = filter(bg_level, to_18 <= 1))

m1bg_rob <- lm_robust(to_18 ~ small_res_doc + 
                      white + black + latino + asian +
                      female + male + dem + rep + age +
                      median_income + some_college + unem +
                      to_16 + to_14 + to_12 + to_10 + 
                      US_Congressional_District,
                    data = filter(bg_level, to_18 <= 1),
                    clusters = US_Congressional_District)


m1bg_ses <- data.frame(
  summary(m1bg_rob)$coefficients)[, 2]

m1bbg <- lm(to_18 ~ tot_disenf +
            white + black + latino + asian +
            female + male + dem + rep + age +
            median_income + some_college + unem +
            to_16 + to_14 + to_12 + to_10 + 
            US_Congressional_District,
          data = filter(bg_level, to_18 <= 1))

m1bbg_rob <- lm_robust(to_18 ~ tot_disenf +
                       white + black + latino + asian +
                       female + male + dem + rep + age +
                       median_income + some_college + unem +
                       to_16 + to_14 + to_12 + to_10 + 
                       US_Congressional_District,
                     data = filter(bg_level, to_18 <= 1),
                     clusters = US_Congressional_District)


m1bbg_ses <- data.frame(
  summary(m1bbg_rob)$coefficients)[, 2]

save(m1bg, m1bg_ses, m1bbg, m1bbg_ses, file = "./temp/bg_turnout_hills.rdata")

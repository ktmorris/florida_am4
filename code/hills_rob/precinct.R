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
  filter(!is.na(cp)) %>% 
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
         PrisonReleaseDate >= "2015-01-01") %>% 
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

###########
m1 <- lm(share_yes ~ tot_disenf + white + black + latino + asian +
           female + male + dem + rep + age +
           median_income + some_college + unem +
           General_2016_11_08 + General_2014_11_04 +
           General_2012_11_06 + General_2010_11_02 +
           US_Congressional_District, data = filter(results_demos, to <= 1))

m1_rob <- lm_robust(share_yes ~ tot_disenf + white + black + latino + asian +
                      female + male + dem + rep + age +
                      median_income + some_college + unem +
                      General_2016_11_08 + General_2014_11_04 +
                      General_2012_11_06 + General_2010_11_02 +
                      US_Congressional_District, data = filter(results_demos, to <= 1),
                    clusters = US_Congressional_District)
m1_ses <- data.frame(
  summary(m1_rob)$coefficients)[, 2]

save(m1, m1_ses, file = "./temp/support_reg_hills.rdata")

marg <- ggeffect(model = m1_rob, c("tot_disenf [all]"))

cm1 <- mean(filter(results_demos, to <= 1)$share_yes)

p1 <- ggplot() + 
  geom_histogram(aes(x = tot_disenf, y = ..count../250), position="identity", linetype=1,
                 fill="gray60", data = results_demos, alpha=0.5, bins = 30) + 
  geom_line(aes(x = x, y = predicted), data = marg, color = "black") +
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), fill= "black", alpha=0.25, data = marg) +
  xlab("Number of Residents with Felony Convictions") +
  ylab("Support for Amendment 4") + scale_x_continuous(labels = comma, limits = c(0, 300)) +
  scale_y_continuous(labels = percent) +
  labs(caption = "Notes: Distribution of number of residents with felony convictions shown at bottom.") +
  geom_hline(yintercept = cm1, linetype = 2) +
  geom_text(aes(300, cm1, label = "Average Precinct Support for Amendment 4",
                vjust = 1, family = "LM Roman 10", hjust = 1)) +
  theme(plot.caption = element_text(hjust = 0)) +
  theme_bw() + theme(plot.caption = element_text(hjust = 0),
                     text = element_text(family = "LM Roman 10"))
save(p1, cm1, file = "./temp/marg_support_am4_hills.rdata")

#############
m2 <- lm(to ~ tot_disenf + white + black + latino + asian +
           female + male + dem + rep + age +
           median_income + some_college + unem +
           General_2016_11_08 + General_2014_11_04 +
           General_2012_11_06 + General_2010_11_02 +
           US_Congressional_District, data = filter(results_demos, to <= 1))

m2_rob <- lm_robust(to ~ tot_disenf + white + black + latino + asian +
                      female + male + dem + rep + age +
                      median_income + some_college + unem +
                      General_2016_11_08 + General_2014_11_04 +
                      General_2012_11_06 + General_2010_11_02 +
                      US_Congressional_District, data = filter(results_demos, to <= 1),
                    clusters = US_Congressional_District)

m2_ses <- data.frame(
  summary(m2_rob)$coefficients)[, 2]

m2b <- lm(to ~ tot_disenf + I(tot_disenf^2) + I(tot_disenf^3) + white + black + latino + asian +
            female + male + dem + rep + age +
            median_income + some_college + unem +
            General_2016_11_08 + General_2014_11_04 +
            General_2012_11_06 + General_2010_11_02 +
            US_Congressional_District, data = filter(results_demos, to <= 1))

m2b_rob <- lm_robust(to ~ tot_disenf + I(tot_disenf^2) + I(tot_disenf^3) + white + black + latino + asian +
                       female + male + dem + rep + age +
                       median_income + some_college + unem +
                       General_2016_11_08 + General_2014_11_04 +
                       General_2012_11_06 + General_2010_11_02 +
                       US_Congressional_District, data = filter(results_demos, to <= 1),
                     clusters = US_Congressional_District)

m2b_ses <- data.frame(
  summary(m2b_rob)$coefficients)[, 2]

save(m2, m2_ses, m2b, m2b_ses, file = "./temp/precinct_turnout_hills.rdata")

marg <- ggeffect(model = m2_rob, "tot_disenf [all]")
cm2 <- mean(filter(results_demos, to <= 1)$to)
p2 <- ggplot() + 
  geom_histogram(aes(x = tot_disenf, y = ..count../250), position="identity", linetype=1,
                 fill="gray60", data = results_demos, alpha=0.5, bins = 30) + 
  geom_line(aes(x = x, y = predicted), data = marg, color = "black") +
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), fill= "black", alpha=0.25, data = marg) +
  xlab("Number of Residents with Felony Convictions") +
  ylab("Turnout Among Registered Voters") + scale_x_continuous(labels = comma, limits = c(0, 300)) +
  scale_y_continuous(labels = percent) +
  labs(caption = "Notes: Distribution of number of residents with felony convictions shown at bottom.") +
  geom_hline(yintercept = cm2, linetype = 2) +
  geom_text(aes(300, cm2, label = "Average Precinct Turnout",
                vjust = -.5, family = "LM Roman 10", hjust = 1)) +
  theme(plot.caption = element_text(hjust = 0)) +
  theme_bw() + theme(plot.caption = element_text(hjust = 0),
                     text = element_text(family = "LM Roman 10"))

save(p2, cm2, file = "./temp/marg_pct_to_hills.rdata")
#############
m3 <- lm(roll_off ~ tot_disenf + white + black + latino + asian +
           female + male + dem + rep + age +
           median_income + some_college + unem +
           General_2016_11_08 + General_2014_11_04 +
           General_2012_11_06 + General_2010_11_02 +
           US_Congressional_District, data = filter(results_demos, to <= 1))

m3_rob <- lm_robust(roll_off ~ tot_disenf + white + black + latino + asian +
                      female + male + dem + rep + age +
                      median_income + some_college + unem +
                      General_2016_11_08 + General_2014_11_04 +
                      General_2012_11_06 + General_2010_11_02 +
                      US_Congressional_District, data = filter(results_demos, to <= 1),
                    clusters = US_Congressional_District)

m3_ses <- data.frame(
  summary(m3_rob)$coefficients)[, 2]

save(m3, m3_ses, file = "./temp/precinct_rolloff_hills.rdata")

marg <- ggeffect(model = m3, "tot_disenf")

cm3 <- mean(filter(results_demos, to <= 1)$roll_off)

p3 <- ggplot() + 
  geom_histogram(aes(x = tot_disenf, y = ..count../5000), position="identity", linetype=1,
                 fill="gray60", data = results_demos, alpha=0.5, bins = 30) + 
  geom_line(aes(x = x, y = predicted), data = marg, color = "black") +
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), fill= "black", alpha=0.25, data = marg) +
  xlab("Number of Residents with Felony Convictions") +
  ylab("Precinct Amendment 4 Roll-Off") + scale_x_continuous(labels = comma, limits = c(0, 300)) +
  scale_y_continuous(labels = percent) +
  labs(caption = "Notes: Distribution of number of residents with felony convictions shown at bottom.") +
  geom_hline(yintercept = cm3, linetype = 2) +
  geom_text(aes(300, cm3, label = "Average Precinct Roll-Off",
                vjust = -.5, family = "LM Roman 10", hjust = 1)) +
  theme(plot.caption = element_text(hjust = 0)) +
  theme_bw() + theme(plot.caption = element_text(hjust = 0),
                     text = element_text(family = "LM Roman 10"))
save(p3, cm3, file = "./temp/marg_rolloff_hills.rdata")

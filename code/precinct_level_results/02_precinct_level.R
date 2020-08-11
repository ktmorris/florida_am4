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
### results stats for writing
results_stats <- rbindlist(lapply(
  list.files("./raw_data/election_results/precinctlevelelectionresults2018gen/", full.names = T),
  fread)) %>% 
  filter(!(V18 %in% c(901, 902)),
         V12 %in% c("United States Senator",
                    "Governor",
                    "Amendment No. 4: Voting Restoration Amendment")) %>% 
  group_by(V15, V12) %>% 
  summarize(votes = sum(V19)) %>% 
  group_by(V12) %>% 
  mutate(share = votes / sum(votes)) %>% 
  filter(share == max(share))

share_gov <- filter(results_stats, V12 == "Governor") %>% 
  select(share) %>% 
  pull()
saveRDS(share_gov, "./temp/share_gov.rds")

share_sen <- filter(results_stats, V12 == "United States Senator") %>% 
  select(share) %>% 
  pull()
saveRDS(share_sen, "./temp/share_sen.rds")
share_am4 <- filter(results_stats, V12 == "Amendment No. 4: Voting Restoration Amendment") %>% 
  select(share) %>% 
  pull()
saveRDS(share_am4, "./temp/share_am4.rds")
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
         PrisonReleaseDate = as.Date(substring(PrisonReleaseDate, 1, 10), "%m/%d/%Y"),
         years_since = 2018 - year(PrisonReleaseDate)) %>% 
  filter(!is.na(cp),
         PrisonReleaseDate <= "2018-11-06") %>% 
  group_by(address1) %>% 
  mutate(big_release = n() >= 5,
         years_since = ifelse(big_release, NA, years_since)) %>% 
  group_by(cp) %>% 
  summarize(years_since = mean(years_since, na.rm = T),
            all_doc = n(),
            small_res_doc = sum(1 - big_release),
            big_release = sum(big_release))

doc_recent <- readRDS("./temp/released_with_addresses.rds") %>% 
  mutate(county = substring(precinct, 1, 3),
         precinct = str_pad(substring(precinct, 4), width = 10, side = "left", pad = "0"),
         cp = paste0(county, precinct),
         PrisonReleaseDate = as.Date(substring(PrisonReleaseDate, 1, 10), "%m/%d/%Y"),
         years_since = 2018 - year(PrisonReleaseDate)) %>% 
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

results_demos$to <- results_demos$votes / results_demos$voter_count
results_demos$highest_to <- results_demos$highest_votes / results_demos$voter_count
results_demos$roll_off <- 1- (results_demos$votes / results_demos$highest_votes)
results_demos$US_Congressional_District <- as.factor(results_demos$US_Congressional_District)
results_demos$median_income <- results_demos$median_income / 10000

saveRDS(results_demos, "temp/results_demos_ll.rds")
###########

results_demos <- readRDS("temp/results_demos_ll.rds")

weighted.mean(filter(results_demos, to <= 1)$share_yes, filter(results_demos, to <= 1)$small_res_doc, na.rm = T)

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
  ggtitle("Marginal Effect of Disenfranchised Voters on Support for Amendment 4") +
  labs(caption = "Notes: Distribution of number of formerly incarcerated residents shown at bottom.") +
  geom_hline(yintercept = mean(results_demos$share_yes)) + theme_bw() +
  theme(plot.caption = element_text(hjust = 0))
###########
f1 <- share_yes ~ small_res_doc + white + black + latino + asian +
  female + male + dem + rep + age +
  median_income + some_college + unem +
  General_2016_11_08 + General_2014_11_04 +
  General_2012_11_06 + General_2010_11_02 +
  US_Congressional_District

f1b <- share_yes ~ small_res_doc + years_since + white + black + latino + asian +
  female + male + dem + rep + age +
  median_income + some_college + unem +
  General_2016_11_08 + General_2014_11_04 +
  General_2012_11_06 + General_2010_11_02 +
  US_Congressional_District

m1 <- lm(f1, data = filter(results_demos, to <= 1))

m1_rob <- lm_robust(f1, data = filter(results_demos, to <= 1),
                    clusters = US_Congressional_District)
m1_ses <- data.frame(
  summary(m1_rob)$coefficients)[, 2]


m1b <- lm((f1b), data = filter(results_demos, to <= 1))

m1b_rob <- lm_robust(f1b, data = filter(results_demos, to <= 1),
                    clusters = US_Congressional_District)
m1b_ses <- data.frame(
  summary(m1b_rob)$coefficients)[, 2]

save(m1, m1_ses, m1b, m1b_ses, file = "./temp/support_reg.rdata")

marg <- ggeffect(model = m1_rob, c("small_res_doc [all]"))

cm1 <- mean(filter(results_demos, to <= 1)$share_yes)

p1 <- ggplot() + 
  geom_histogram(aes(x = small_res_doc, y = ..count../2500), position="identity", linetype=1,
                 fill="gray60", data = results_demos, alpha=0.5, bins = 30) + 
  geom_line(aes(x = x, y = predicted), data = marg, color = "black") +
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), fill= "black", alpha=0.25, data = marg) +
  xlab("Number of Formerly Incarcerated Residents") +
  ylab("Support for Amendment 4") + scale_x_continuous(labels = comma, limits = c(0, 300)) +
  scale_y_continuous(labels = percent) +
  labs(caption = "Notes: Distribution of number of formerly incarcerated residents shown at bottom.") +
  geom_hline(yintercept = cm1, linetype = 2) +
  geom_text(aes(300, cm1-0.03, label = "Average Precinct Support for Amendment 4",
                family = "LM Roman 10", hjust = 1)) +
  theme(plot.caption = element_text(hjust = 0)) +
  theme_bw() + theme(plot.caption = element_text(hjust = 0),
                     text = element_text(family = "LM Roman 10"))
save(p1, cm1, file = "./temp/marg_support_am4.rdata")

#############
f2 <- to ~ small_res_doc + white + black + latino + asian +
  female + male + dem + rep + age +
  median_income + some_college + unem +
  General_2016_11_08 + General_2014_11_04 +
  General_2012_11_06 + General_2010_11_02 +
  US_Congressional_District

f2b <- to ~ small_res_doc + years_since + white + black + latino + asian +
  female + male + dem + rep + age +
  median_income + some_college + unem +
  General_2016_11_08 + General_2014_11_04 +
  General_2012_11_06 + General_2010_11_02 +
  US_Congressional_District

m2 <- lm(f2, data = filter(results_demos, to <= 1))

m2_rob <- lm_robust(f2, data = filter(results_demos, to <= 1),
                    clusters = US_Congressional_District)
m2_ses <- data.frame(
  summary(m2_rob)$coefficients)[, 2]


m2b <- lm(f2b, data = filter(results_demos, to <= 1))

m2b_rob <- lm_robust(f2b, data = filter(results_demos, to <= 1),
                     clusters = US_Congressional_District)
m2b_ses <- data.frame(
  summary(m2b_rob)$coefficients)[, 2]

save(m2, m2_ses, m2b, m2b_ses, file = "./temp/precinct_turnout.rdata")

marg <- ggeffect(model = m2_rob, "small_res_doc [all]")
cm2 <- mean(filter(results_demos, to <= 1)$to)
p2 <- ggplot() + 
  geom_histogram(aes(x = small_res_doc, y = ..count../2500), position="identity", linetype=1,
                 fill="gray60", data = results_demos, alpha=0.5, bins = 30) + 
  geom_line(aes(x = x, y = predicted), data = marg, color = "black") +
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), fill= "black", alpha=0.25, data = marg) +
  xlab("Number of Formerly Incarcerated Residents") +
  ylab("Turnout Among Registered Voters") + scale_x_continuous(labels = comma, limits = c(0, 300)) +
  scale_y_continuous(labels = percent) +
  labs(caption = "Notes: Distribution of number of formerly incarcerated residents shown at bottom.") +
  geom_hline(yintercept = cm2, linetype = 2) +
  geom_text(aes(300, cm2, label = "Average Precinct Turnout",
                vjust = -.5, family = "LM Roman 10", hjust = 1)) +
  theme(plot.caption = element_text(hjust = 0)) +
  theme_bw() + theme(plot.caption = element_text(hjust = 0),
                     text = element_text(family = "LM Roman 10"))

save(p2, cm2, file = "./temp/marg_pct_to.rdata")
#############
f3 <- roll_off ~ small_res_doc + white + black + latino + asian +
  female + male + dem + rep + age +
  median_income + some_college + unem +
  General_2016_11_08 + General_2014_11_04 +
  General_2012_11_06 + General_2010_11_02 +
  US_Congressional_District

f3b <- roll_off ~ small_res_doc + years_since + white + black + latino + asian +
  female + male + dem + rep + age +
  median_income + some_college + unem +
  General_2016_11_08 + General_2014_11_04 +
  General_2012_11_06 + General_2010_11_02 +
  US_Congressional_District

m3 <- lm(f3, data = filter(results_demos, to <= 1))

m3_rob <- lm_robust(f3, data = filter(results_demos, to <= 1),
                    clusters = US_Congressional_District)
m3_ses <- data.frame(
  summary(m3_rob)$coefficients)[, 2]


m3b <- lm((f3b), data = filter(results_demos, to <= 1))

m3b_rob <- lm_robust(f3b, data = filter(results_demos, to <= 1),
                     clusters = US_Congressional_District)
m3b_ses <- data.frame(
  summary(m3b_rob)$coefficients)[, 2]

save(m3, m3_ses, m3b, m3b_ses, file = "./temp/precinct_rolloff.rdata")

marg <- ggeffect(model = m3_rob, "small_res_doc [all]")

cm3 <- mean(filter(results_demos, to <= 1)$roll_off)

p3 <- ggplot() + 
  geom_histogram(aes(x = small_res_doc, y = ..count../50000), position="identity", linetype=1,
                 fill="gray60", data = results_demos, alpha=0.5, bins = 30) + 
  geom_line(aes(x = x, y = predicted), data = marg, color = "black") +
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), fill= "black", alpha=0.25, data = marg) +
  xlab("Number of Formerly Incarcerated Residents") +
  ylab("Precinct Amendment 4 Roll-Off") + scale_x_continuous(labels = comma, limits = c(0, 300)) +
  scale_y_continuous(labels = percent) +
  labs(caption = "Notes: Distribution of number of formerly incarcerated residents shown at bottom.") +
  geom_hline(yintercept = cm3, linetype = 2) +
  geom_text(aes(300, cm3, label = "Average Precinct Roll-Off",
                vjust = -.5, family = "LM Roman 10", hjust = 1)) +
  theme(plot.caption = element_text(hjust = 0)) +
  theme_bw() + theme(plot.caption = element_text(hjust = 0),
                     text = element_text(family = "LM Roman 10"))
save(p3, cm3, file = "./temp/marg_rolloff.rdata")

################ for appendix
m1_ap <- lm(share_yes ~ small_res_doc_recent + white + black + latino + asian +
           female + male + dem + rep + age +
           median_income + some_college + unem +
           General_2016_11_08 + General_2014_11_04 +
           General_2012_11_06 + General_2010_11_02 +
           US_Congressional_District, data = filter(results_demos, to <= 1))

m2_ap <- lm(to ~ small_res_doc_recent + white + black + latino + asian +
           female + male + dem + rep + age +
           median_income + some_college + unem +
           General_2016_11_08 + General_2014_11_04 +
           General_2012_11_06 + General_2010_11_02 +
           US_Congressional_District, data = filter(results_demos, to <= 1))

m3_ap <- lm(roll_off ~ small_res_doc_recent + white + black + latino + asian +
           female + male + dem + rep + age +
           median_income + some_college + unem +
           General_2016_11_08 + General_2014_11_04 +
           General_2012_11_06 + General_2010_11_02 +
           US_Congressional_District, data = filter(results_demos, to <= 1))

m1b_ap <- lm(share_yes ~ all_doc + white + black + latino + asian +
              female + male + dem + rep + age +
              median_income + some_college + unem +
              General_2016_11_08 + General_2014_11_04 +
              General_2012_11_06 + General_2010_11_02 +
              US_Congressional_District, data = filter(results_demos, to <= 1))

m2b_ap <- lm(to ~ all_doc + white + black + latino + asian +
              female + male + dem + rep + age +
              median_income + some_college + unem +
              General_2016_11_08 + General_2014_11_04 +
              General_2012_11_06 + General_2010_11_02 +
              US_Congressional_District, data = filter(results_demos, to <= 1))

m3b_ap <- lm(roll_off ~ all_doc + white + black + latino + asian +
              female + male + dem + rep + age +
              median_income + some_college + unem +
              General_2016_11_08 + General_2014_11_04 +
              General_2012_11_06 + General_2010_11_02 +
              US_Congressional_District, data = filter(results_demos, to <= 1))

save(m1_ap, m2_ap, m3_ap,
     m1b_ap, m2b_ap, m3b_ap, file = "./temp/precinct_regs_appendix.rdata")
#############

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

saveRDS(mean(bg_level$small_res_doc > 0), "./temp/share_bgs_have_lost.rds")

cvap <- fread("../regular_data/CVAP_2014-2018_ACS_csv_files/BlockGr.csv") %>% 
  mutate(GEOID = substring(geoid, 8)) %>% 
  filter(substring(GEOID, 1, 2) == "12",
         lntitle == "Total") %>% 
  select(GEOID, cvap = cvap_est)

bg_level <- inner_join(bg_level, cvap)

bg_level$cvap <- bg_level$cvap - bg_level$all_doc
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

saveRDS(bg_level, "temp/bg_level_reg_data.rds")

m1 <- lm(to_18 ~ small_res_doc + 
           white + black + latino + asian +
           female + male + dem + rep + age +
           median_income + some_college + unem +
           to_16 + to_14 + to_12 + to_10 + 
           US_Congressional_District,
         data = filter(bg_level, to_18 <= 1))

m1_rob <- lm_robust(to_18 ~ small_res_doc + 
                      white + black + latino + asian +
                      female + male + dem + rep + age +
                      median_income + some_college + unem +
                      to_16 + to_14 + to_12 + to_10 + 
                      US_Congressional_District,
                    data = filter(bg_level, to_18 <= 1),
                    clusters = US_Congressional_District)


m1_ses <- data.frame(
  summary(m1_rob)$coefficients)[, 2]

m1b <- lm(to_18 ~ small_res_doc + years_since +
           white + black + latino + asian +
           female + male + dem + rep + age +
           median_income + some_college + unem +
           to_16 + to_14 + to_12 + to_10 + 
           US_Congressional_District,
         data = filter(bg_level, to_18 <= 1))

m1b_rob <- lm_robust(to_18 ~ small_res_doc + years_since +
                      white + black + latino + asian +
                      female + male + dem + rep + age +
                      median_income + some_college + unem +
                      to_16 + to_14 + to_12 + to_10 + 
                      US_Congressional_District,
                    data = filter(bg_level, to_18 <= 1),
                    clusters = US_Congressional_District)


m1b_ses <- data.frame(
  summary(m1b_rob)$coefficients)[, 2]

save(m1, m1_ses, m1b, m1b_ses, file = "./temp/bg_turnout.rdata")

marg <- ggpredict(model = m1_rob, c("small_res_doc [all]"))
p2 <- ggplot() + 
  geom_histogram(aes(x = small_res_doc, y = ..count../2500), position="identity", linetype=1,
                 fill="gray60", data = results_demos, alpha=0.5, bins = 30) + 
  geom_line(aes(x = x, y = predicted), data = marg, color = "black") +
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), fill= "black", alpha=0.25, data = marg) +
  xlab("Number of Formerly Incarcerated Residents") +
  ylab("Turnout Among Registered Voters") + scale_x_continuous(labels = comma, limits = c(0, 115)) +
  scale_y_continuous(labels = percent) +
  labs(caption = "Notes: Distribution of number of formerly incarcerated residents shown at bottom.") +
  theme(plot.caption = element_text(hjust = 0)) +
  theme_bw() + theme(plot.caption = element_text(hjust = 0),
                     text = element_text(family = "LM Roman 10"))

save(p2, file = "./temp/marg_bg_to.rdata")
######
m1_ap <- lm(to_18 ~ small_res_doc_recent + 
           white + black + latino + asian +
           female + male + dem + rep + age +
           median_income + some_college + unem +
           to_16 + to_14 + to_12 + to_10 + 
           US_Congressional_District,
         data = filter(bg_level, to_18 <= 1))

m1b_ap <- lm(to_18 ~ all_doc + 
               white + black + latino + asian +
               female + male + dem + rep + age +
               median_income + some_college + unem +
               to_16 + to_14 + to_12 + to_10 + 
               US_Congressional_District,
             data = filter(bg_level, to_18 <= 1))

save(m1_ap, m1b_ap, file = "./temp/bg_regs_appendix.rdata")
######

bgs_new <- inner_join(readRDS("./temp/block_group_census_data.RDS"),
                      select(bg_level, GEOID, all_doc)) %>% 
  ungroup()


bgs_new <- rbind(
  bgs_new %>% 
    mutate(group = "former_inc",
           weight = all_doc),
  bgs_new %>% 
    mutate(group = "overall",
           weight = population)
)

######

tot <- rbindlist(lapply(c("median_income", "median_age", "unem", "some_college",
                          "nh_white", "nh_black", "latino"), function(m){
  ints <- rbindlist(lapply(unique(bgs_new$group), function(r){
    r <- as.character(r)
    t <- bgs_new %>% 
      filter(group == r) %>% 
      select(weight, measure = m) %>% 
      filter(!is.na(measure))
    j <- weighted.ttest.ci((t$measure), weights = t$weight,)
    j <- data.table(group = c(r),
                    measure = m,
                    lower = j[1],
                    upper = j[2])
  }))
  d <- data.table(sig = (ints$lower[1] > ints$upper[2]) | (ints$lower[2] > ints$upper[1]),
                  measure = m)
  return(d)
}))



ll <- bgs_new %>% 
  group_by(group) %>% 
  summarize_at(vars("median_income", "median_age", "unem", "some_college",
                    "nh_white", "nh_black", "latino"),
               ~ weighted.mean(., weight, na.rm = T))

ll2 <- group_by(bgs_new, group) %>% summarize(count = sum(weight))

ll <- left_join(ll, ll2) %>% 
  mutate(median_income = dollar(median_income, accuracy = 1),
         median_age = round(median_age, digits = 1),
         count = comma(count)) %>% 
  mutate_at(vars(unem, some_college,
                 nh_white, nh_black, latino), ~ percent(., accuracy = 0.1))

ll <- transpose(ll)
ll$var <- c("measure", "median_income", "median_age", "unem", "some_college",
            "nh_white", "nh_black", "latino", "count")

colnames(ll) <- ll[1,]
ll <- ll[2:nrow(ll),]

ll <- left_join(ll, tot)

ll$measure <- c( "Median Income", "Median Age", "% Unemployed", "% with Some College",
                 "% Non-Hispanic White", "% Non-Hispanic Black", "% Latino", "Count")
ll <- ll %>% 
  mutate(measure = ifelse(sig & !is.na(sig), paste0(measure, "*"), measure)) %>% 
  select(measure, overall, former_inc)

colnames(ll) <- c("Measure", "Average Neighborhood", "Average Neighborhood\\\\for Formerly Incarcerated")

saveRDS(ll, "./temp/demos_nhoods.rds")


# downloading census data works better county-by-county
# commenting out because it takes so long
# source("code/helpers/get_basic_census.R")
# census_data <- rbindlist(lapply(filter(fips_codes, state == "FL")$county_code, function(c){
#   inc <- census_income(geo = "block group", year = 2018, state = "FL", county = c)
#   some_college <- census_education(geo = "block group", year = 2018, state = "FL", county = c)
#   unem <- census_unemployment(geo = "block group", year = 2018, state = "FL", county = c)
#   
#   return(full_join(inc, full_join(some_college, unem)))
# }))
# saveRDS(census_data, "./temp/block_group_census_data.RDS")

census_data <- readRDS("./temp/block_group_census_data.RDS") %>% 
  select(GEOID, median_income, some_college, unem)

fl_file <- inner_join(readRDS("temp/anon_for_precincts.rds"), census_data)


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
doc <- readRDS("temp/full_doc_precinct.rds")

doc_recent <- readRDS("temp/recent_doc_precinct.rds")

results_demos <- left_join(results_demos, full_join(doc, doc_recent)) %>% 
  mutate_at(vars(all_doc, small_res_doc), ~ ifelse(is.na(.), 0, .))

results_demos$to <- results_demos$votes / results_demos$voter_count
results_demos$highest_to <- results_demos$highest_votes / results_demos$voter_count
results_demos$roll_off <- 1- (results_demos$votes / results_demos$highest_votes)
results_demos$US_Congressional_District <- as.factor(results_demos$US_Congressional_District)
results_demos$median_income <- results_demos$median_income / 10000

saveRDS(results_demos %>% 
          select(-precinct), "temp/precint_level_reg_data.rds")

#################################################
############# BLOCK GROUP  ######################
#################################################

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

doc_bg <- readRDS("temp/all_doc_bg.rds")

doc_bg_recent <- readRDS("temp/recent_doc_bg.rds")

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

saveRDS(bg_level %>% 
          select(-GEOID), "temp/bg_level_reg_data.rds")

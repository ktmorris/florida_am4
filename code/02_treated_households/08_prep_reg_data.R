fl_roll <- readRDS("./temp/fl_file_pre_match.rds")

ids <- fl_roll %>%
  mutate(id = row_number()) %>%
  dplyr::select(id, voter_id_anon)

load("./temp/mout_av.RData")

matches <- data.table(control = c(mout$index.control, mout$index.treated),
                      match_group = rep(mout$index.treated, 2),
                      weight = rep(mout$weights, 2)) %>% 
  group_by(match_group, control) %>% 
  summarize(weight = sum(weight)) %>% 
  ungroup()

matches <- left_join(matches, ids, by = c("match_group" = "id")) %>%
  dplyr::select(-match_group) %>%
  rename(match_group = voter_id_anon)

matches <- left_join(matches, ids, by = c("control" = "id")) %>%
  dplyr::select(-control) %>%
  rename(voter = voter_id_anon)
# 
# ######
# 
fl_history <- readRDS("temp/fl_history_anon.rds")

fl_ll <- fl_history %>% 
  group_by(year) %>% 
  dplyr::summarize(voted = mean(voted)) %>%
  mutate(treated = "All Florida Voters")

######

matches <- left_join(matches, fl_history, by = c("voter" = "voter_id_anon"))

matches <- left_join(matches, fl_roll, by = c("voter" = "voter_id_anon")) %>% 
  dplyr::select(-max_release)

matches <- left_join(matches, dplyr::select(fl_roll, voter_id_anon, max_release, match_reg_date = reg_date),
                     by = c("match_group" = "voter_id_anon")) %>% 
  filter(max_release <= "2018-11-06")

matches$interaction_term <- (matches$year == "2018") * matches$treated

matches$interfull <- matches$interaction_term * (2018 - year(matches$max_release))
matches$years_since <- (2018 - year(matches$max_release))
matches$d18 <- (matches$year == "2018")*1
matches$US_Congressional_District <- as.factor(matches$US_Congressional_District)
matches$pres <- matches$year %in% c("2012", "2016")

matches2 <- filter(matches, max_release >= (match_reg_date + as.Date("2000-01-01")))

save(matches, matches2, fl_ll, file = "temp/pre_reg_av.rdata")

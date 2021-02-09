fl_roll <- readRDS("./temp/hills_file_pre_match.rds")

ids <- fl_roll %>%
  mutate(id = row_number()) %>%
  dplyr::select(id, voter_id_anon)

load("./temp/mout_av_hills.RData")

matches <- data.table(control = c(mout$index.control, mout$index.treated),
                      match_group = rep(mout$index.treated, 2),
                      weight = rep(mout$weights)) %>% 
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
fl_history <- readRDS("temp/fl_history_anon.rds") %>% 
  filter(voter_id_anon %in% matches$voter)


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
cleanup(c("matches", "matches2"))
##################################
save(matches, matches2, file = "temp/pre_reg_hills.rdata")
load("temp/pre_reg_hills.rdata")

matches_hills <- matches
####################################### keep only hillsborough
load("temp/pre_reg.rdata")
rm(matches2)

matches <- filter(matches, match_group %in% matches_hills$voter)

f1 <- voted ~ d18*treated
f2 <- voted ~ d18*treated +
  white + black + latino + asian + female +
  male + reg_date + age + dem + rep +
  median_income + some_college + US_Congressional_District
f3 <- voted ~ d18*treated*years_since
f4 <- voted ~ d18*treated*years_since +
         white + black + latino + asian + female +
         male + reg_date + age + dem + rep +
         median_income + some_college + US_Congressional_District

models1 <- lapply(c(f1, f2, f3, f4), function(f){
  m <- lm(f, data = matches,
          weight = matches$weight)
})
       
models2 <- lapply(c(f1, f2, f3, f4), function(f){
  m <- lm(f, data = matches_hills,
           weight = matches_hills$weight)
})


ses_cl <- list(
  summary(lm.cluster(formula = f1, data = matches, weights = matches$weight, cluster = matches$match_group))[ , 2],
  summary(lm.cluster(formula = f2, data = matches, weights = matches$weight, cluster = matches$match_group))[ , 2],
  summary(lm.cluster(formula = f3, data = matches, weights = matches$weight, cluster = matches$match_group))[ , 2],
  summary(lm.cluster(formula = f4, data = matches, weights = matches$weight, cluster = matches$match_group))[ , 2],
  summary(lm.cluster(formula = f1, data = matches_hills, weights = matches_hills$weight, cluster = matches_hills$match_group))[ , 2],
  summary(lm.cluster(formula = f2, data = matches_hills, weights = matches_hills$weight, cluster = matches_hills$match_group))[ , 2],
  summary(lm.cluster(formula = f3, data = matches_hills, weights = matches_hills$weight, cluster = matches_hills$match_group))[ , 2],
  summary(lm.cluster(formula = f4, data = matches_hills, weights = matches_hills$weight, cluster = matches_hills$match_group))[ , 2]
)

save(models1, models2, ses_cl, file = "./temp/full_match_reg_hills_av.rdata")
##make regression table
source("./code/misc/make_big_reg_table_hills.R")

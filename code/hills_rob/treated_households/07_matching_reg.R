

fl_roll <- readRDS("./temp/hills_file_pre_match.rds")

ids <- fl_roll %>% 
  mutate(id = row_number()) %>% 
  select(id, LALVOTERID)

load("./temp/mout_t1_hills.RData")

matches <- data.table(match_group = c(mout$index.treated, unique(mout$index.treated)),
                      control = c(mout$index.control, unique(mout$index.treated)),
                      weight = c(mout$weights, rep(1, length(unique(mout$index.treated)))))

matches <- left_join(matches, ids, by = c("match_group" = "id")) %>% 
  select(-match_group) %>% 
  rename(match_group = LALVOTERID)

matches <- left_join(matches, ids, by = c("control" = "id")) %>% 
  select(-control) %>% 
  rename(voter = LALVOTERID)

######

history <- dbConnect(SQLite(), "D:/national_file_history.db")
fl_history <- dbGetQuery(history, "select LALVOTERID,
                                   General_2018_11_06,
                                   General_2016_11_08,
                                   General_2014_11_04,
                                   General_2012_11_06,
                                   General_2010_11_02
                                   from fl_history_18") %>% 
  filter(LALVOTERID %in% matches$voter)

fl_history <- reshape2::melt(fl_history, id.vars = "LALVOTERID") %>% 
  mutate(year = substring(variable, 9, 12),
         voted = ifelse(value == "Y", 1, 0)) %>% 
  select(-variable, -value)


######

matches <- left_join(matches, fl_history, by = c("voter" = "LALVOTERID"))

matches <- left_join(matches, fl_roll, by = c("voter" = "LALVOTERID")) %>% 
  select(-max_release)

matches <- left_join(matches, select(fl_roll, LALVOTERID, max_release, match_reg_date = reg_date),
                     by = c("match_group" = "LALVOTERID")) %>% 
  filter(max_release <= "2018-11-06")

matches$interaction_term <- (matches$year == "2018") * matches$treated

matches$interfull <- matches$interaction_term * (2018 - year(matches$max_release))
matches$years_since <- (2018 - year(matches$max_release))
matches$d18 <- (matches$year == "2018")*1
matches$US_Congressional_District <- as.factor(matches$US_Congressional_District)
matches$pres <- matches$year %in% c("2012", "2016")

matches2 <- filter(matches, max_release >= (match_reg_date + as.Date("2000-01-01")))
cleanup(c("matches"))
##################################
save(matches, file = "temp/pre_reg_hills.rdata")

matches_hills <- matches
####################################### keep only hillsborough
load("temp/pre_reg.rdata")
rm(matches2)

matches <- filter(matches, match_group %in% matches_hills$voter)

######################
## I HAVE TO RUN THIS ON THE HPC BECAUSE OF RAM CONSTRAINTS

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

save(models1, models2, ses_cl, file = "./temp/full_match_reg_hills.rdata")
##make regression table
source("./code/misc/make_big_reg_table_hills.R")

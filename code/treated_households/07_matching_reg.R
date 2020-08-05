

fl_roll <- readRDS("./temp/fl_file_pre_match.rds")

ids <- fl_roll %>% 
  mutate(id = row_number()) %>% 
  select(id, LALVOTERID)

load("./temp/mout_t1.RData")

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
                                   from fl_history_18")

fl_history <- reshape2::melt(fl_history, id.vars = "LALVOTERID") %>% 
  mutate(year = substring(variable, 9, 12),
         voted = ifelse(value == "Y", 1, 0)) %>% 
  select(-variable, -value)

fl_ll <- fl_history %>% 
  group_by(year) %>% 
  summarize(voted = mean(voted)) %>%
  mutate(treated = "All Florida Voters")

fl_history <- filter(fl_history, LALVOTERID %in% matches$voter)

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
cleanup(c("matches", "matches2", "fl_ll"))
##################################
save(matches, matches2, fl_ll, file = "temp/pre_reg.rdata")


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
  m <- lm(f, data = matches2,
          weight = matches2$weight)
})

ses_cl <- list(
  summary(lm.cluster(formula = f1, data = matches, weights = matches$weight, cluster = matches$match_group))[ , 2],
  summary(lm.cluster(formula = f2, data = matches, weights = matches$weight, cluster = matches$match_group))[ , 2],
  summary(lm.cluster(formula = f3, data = matches, weights = matches$weight, cluster = matches$match_group))[ , 2],
  summary(lm.cluster(formula = f4, data = matches, weights = matches$weight, cluster = matches$match_group))[ , 2],
  summary(lm.cluster(formula = f1, data = matches2, weights = matches2$weight, cluster = matches2$match_group))[ , 2],
  summary(lm.cluster(formula = f2, data = matches2, weights = matches2$weight, cluster = matches2$match_group))[ , 2],
  summary(lm.cluster(formula = f3, data = matches2, weights = matches2$weight, cluster = matches2$match_group))[ , 2],
  summary(lm.cluster(formula = f4, data = matches2, weights = matches2$weight, cluster = matches2$match_group))[ , 2]
)

save(models1, models2, ses_cl, file = "./temp/full_match_reg.rdata")
##make regression table
source("./code/misc/make_big_reg_table.R")
####
matches3 <- filter(matches2, max_release < "2010-01-01")
m1c <- lm(f1, data = matches3,
           weight = matches3$weight)

m1c_ses <- summary(lm.cluster(formula = f1, data = matches3,
                              weights = matches3$weight, cluster = matches3$match_group))[ , 2]

m2c <- lm(f2,
           data = matches3,
           weight = matches3$weight)

m2c_ses <- summary(lm.cluster(formula = f2, data = matches3,
                              weights = matches3$weight, cluster = matches3$match_group))[ , 2]

source("./code/misc/make_medium_reg_table.R")
####
# 
# marg <- ggpredict(model = m3b, c("years_since [all]", "treated [all]", "d18 [all]"))
# 
# marg_end <- marg %>% 
#   filter(facet == 1) %>% 
#   mutate(group = ifelse(group == 1, "Treated", "Control"))
# 
# ll <- matches %>% 
#   group_by(years_since) %>% 
#   tally()
# 
# p <- ggplot() + 
#   geom_col(data = ll, aes(x = years_since, y = n/(2500*1000)), position="identity", linetype=1,
#            fill="gray60", alpha=0.5) +
#   geom_line(aes(x = x, y = predicted, linetype = group), data = marg_end) +
#   geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high, group = group), alpha=0.25, fill = "black",
#               data = marg_end) +
#   xlab("Years Since Last Household Imprisonment") +
#   ylab("Predicted 2018 Turnout Among Registered Voters") + scale_x_continuous(labels = comma_format(accuracy = 1)) +
#   scale_y_continuous(labels = percent) +
#   labs(caption = "Notes: Distribution of years since latest imprisonment at bottom.") +
#   theme(plot.caption = element_text(hjust = 0)) +
#   theme_bw() + theme(plot.caption = element_text(hjust = 0),
#                      text = element_text(family = "LM Roman 10")) +
#   labs(linetype = "Treatment Group")
# 
# saveRDS(p, "./temp/years_out.rds")

# 
# c1 <- exp(confint(glm.cluster(voted ~ I(year == "2018")*treated, data = matches,
#                               family = "binomial",
#                               weight = matches$weight,
#                               cluster = matches$match_group)))
# 
# c2 <- exp(confint(glm.cluster(voted ~ I(year == "2018")*treated +
#                                 white + black + latino + asian + female +
#                                 male + reg_date + age + dem + rep +
#                                 median_income + some_college, data = matches,
#                               family = "binomial",
#                               weight = matches$weight,
#                               cluster = matches$match_group)))
# 
# #### subgroup
# 
# black_voters <- filter(matches, match_group %in%
#                          filter(fl_roll, black == 1)$LALVOTERID)
# 
# c1_b <- exp(confint(glm.cluster(voted ~ I(year == "2018")*treated, data = black_voters,
#                                 family = "binomial",
#                                 weight = black_voters$weight,
#                                 cluster = black_voters$match_group)))
# 
# c2_b <- exp(confint(glm.cluster(voted ~ I(year == "2018")*treated +
#                                   white + black + latino + asian + female +
#                                   male + reg_date + age + dem + rep +
#                                   median_income + some_college, data = black_voters,
#                                 family = "binomial",
#                                 weight = black_voters$weight,
#                                 cluster = black_voters$match_group)))
# 
# #### white
# 
# white_voters <- filter(matches, match_group %in%
#                          filter(fl_roll, white == 1)$LALVOTERID)
# 
# c1_w <- exp(confint(glm.cluster(voted ~ I(year == "2018")*treated, data = white_voters,
#                                 family = "binomial",
#                                 weight = white_voters$weight,
#                                 cluster = white_voters$match_group)))
# 
# c2_w <- exp(confint(glm.cluster(voted ~ I(year == "2018")*treated +
#                                   white + black + latino + asian + female +
#                                   male + reg_date + age + dem + rep +
#                                   median_income + some_college, data = white_voters,
#                                 family = "binomial",
#                                 weight = white_voters$weight,
#                                 cluster = white_voters$match_group)))
# 
# #### latino
# 
# latino_voters <- filter(matches, match_group %in%
#                           filter(fl_roll, latino == 1)$LALVOTERID)
# 
# c1_l <- exp(confint(glm.cluster(voted ~ I(year == "2018")*treated, data = latino_voters,
#                                 family = "binomial",
#                                 weight = latino_voters$weight,
#                                 cluster = latino_voters$match_group)))
# 
# c2_l <- exp(confint(glm.cluster(voted ~ I(year == "2018")*treated +
#                                   white + black + latino + asian + female +
#                                   male + reg_date + age + dem + rep +
#                                   median_income + some_college, data = latino_voters,
#                                 family = "binomial",
#                                 weight = latino_voters$weight,
#                                 cluster = latino_voters$match_group)))
# #### women
# 
# female_voters <- filter(matches, match_group %in%
#                           filter(fl_roll, female == 1)$LALVOTERID)
# 
# c1_f <- exp(confint(glm.cluster(voted ~ I(year == "2018")*treated, data = female_voters,
#                                 family = "binomial",
#                                 weight = female_voters$weight,
#                                 cluster = female_voters$match_group)))
# 
# c2_f <- exp(confint(glm.cluster(voted ~ I(year == "2018")*treated +
#                                   white + black + latino + asian + female +
#                                   male + reg_date + age + dem + rep +
#                                   median_income + some_college, data = female_voters,
#                                 family = "binomial",
#                                 weight = female_voters$weight,
#                                 cluster = female_voters$match_group)))
# 
# #### men
# 
# male_voters <- filter(matches, match_group %in%
#                         filter(fl_roll, male == 1)$LALVOTERID)
# 
# c1_m <- exp(confint(glm.cluster(voted ~ I(year == "2018")*treated, data = male_voters,
#                                 family = "binomial",
#                                 weight = male_voters$weight,
#                                 cluster = male_voters$match_group)))
# 
# c2_m <- exp(confint(glm.cluster(voted ~ I(year == "2018")*treated +
#                                   white + black + latino + asian + female +
#                                   male + reg_date + age + dem + rep +
#                                   median_income + some_college, data = male_voters,
#                                 family = "binomial",
#                                 weight = male_voters$weight,
#                                 cluster = male_voters$match_group)))
# 
# save(c1, c2, c1_b, c2_b, c1_w, c2_w, c1_l, c2_l,
#      c1_f, c2_f, c1_m, c2_m, file = "./temp/confints.rdata")
###################
###################
###################
ll <- matches %>% 
  mutate(treated) %>% 
  group_by(treated, year) %>% 
  summarize(voted = mean(voted)) %>% 
  ungroup() %>% 
  mutate(treated = ifelse(treated == 1, "Treated", "Control"))

ll <- bind_rows(ll, fl_ll)

plot <- ggplot(ll, aes(x = as.integer(year), y = voted,
                       linetype = treated)) +
  geom_line() + geom_point() +
  scale_linetype_manual(values = c("dotted", "longdash", "solid")) +
  labs(linetype = "Treatment Group",
       x = "Year", y = "Turnout Among Registered Voters") +
  scale_y_continuous(labels = percent) +
  theme_bw() + theme(text = element_text(family = "LM Roman 10"))
saveRDS(plot, "./temp/parallel_trends_plot.rds")

##########
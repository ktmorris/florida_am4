

fl_roll <- readRDS("./temp/fl_file_pre_match.rds")

ids <- fl_roll %>% 
  mutate(id = row_number()) %>% 
  select(id, LALVOTERID)

load("./temp/mout_t1.RData")


matches <- data.table(treated = mout$index.treated,
                      control = mout$index.control)

matches <- left_join(matches, ids, by = c("treated" = "id")) %>% 
  select(-treated) %>% 
  rename(match_group = LALVOTERID)

matches <- left_join(matches, ids, by = c("control" = "id")) %>% 
  select(-control) %>% 
  rename(voter = LALVOTERID)

matches <- rbind(matches,
                 data.table(match_group = matches$match_group,
                            voter = matches$match_group))

matches <- matches %>% 
  group_by(match_group, voter) %>% 
  summarize(weight = n()) %>% 
  ungroup()
######

history <- dbConnect(SQLite(), "D:/national_file_history.db")
fl_history <- dbGetQuery(history, "select LALVOTERID,
                                   General_2018_11_06,
                                   General_2016_11_08,
                                   General_2014_11_04,
                                   General_2012_11_06,
                                   General_2010_11_02
                                   from fl_history_18")
fl_history <- filter(fl_history, LALVOTERID %in% matches$voter)

fl_history <- reshape2::melt(fl_history, id.vars = "LALVOTERID") %>% 
  mutate(year = substring(variable, 9, 12),
         voted = ifelse(value == "Y", 1, 0)) %>% 
  select(-variable, -value)


######

matches <- full_join(matches, fl_history, by = c("voter" = "LALVOTERID"))

matches <- left_join(matches, fl_roll, by = c("voter" = "LALVOTERID"))

m1 <- glm(voted ~ I(year == "2018")*treated, data = matches,
                  family = "binomial",
                  weight = matches$weight)

m2 <- glm(voted ~ I(year == "2018")*treated +
                    white + black + latino + asian + female +
                    male + reg_date + age + dem + rep +
                    median_income + some_college, data = matches,
                  family = "binomial",
                  weight = matches$weight)

save(m1, m2, file = "./temp/full_match_reg.rdata")

c1 <- exp(confint(glm.cluster(voted ~ I(year == "2018")*treated, data = matches,
                              family = "binomial",
                              weight = matches$weight,
                              cluster = matches$match_group)))

c2 <- exp(confint(glm.cluster(voted ~ I(year == "2018")*treated +
                                white + black + latino + asian + female +
                                male + reg_date + age + dem + rep +
                                median_income + some_college, data = matches,
                              family = "binomial",
                              weight = matches$weight,
                              cluster = matches$match_group)))

#### subgroup

black_voters <- filter(matches, match_group %in%
                         filter(fl_roll, black == 1)$LALVOTERID)

c1_b <- exp(confint(glm.cluster(voted ~ I(year == "2018")*treated, data = black_voters,
                              family = "binomial",
                              weight = black_voters$weight,
                              cluster = black_voters$match_group)))

c2_b <- exp(confint(glm.cluster(voted ~ I(year == "2018")*treated +
                                white + black + latino + asian + female +
                                male + reg_date + age + dem + rep +
                                median_income + some_college, data = black_voters,
                              family = "binomial",
                              weight = black_voters$weight,
                              cluster = black_voters$match_group)))

#### white

white_voters <- filter(matches, match_group %in%
                         filter(fl_roll, white == 1)$LALVOTERID)

c1_w <- exp(confint(glm.cluster(voted ~ I(year == "2018")*treated, data = white_voters,
                              family = "binomial",
                              weight = white_voters$weight,
                              cluster = white_voters$match_group)))

c2_w <- exp(confint(glm.cluster(voted ~ I(year == "2018")*treated +
                                white + black + latino + asian + female +
                                male + reg_date + age + dem + rep +
                                median_income + some_college, data = white_voters,
                              family = "binomial",
                              weight = white_voters$weight,
                              cluster = white_voters$match_group)))

#### latino

latino_voters <- filter(matches, match_group %in%
                         filter(fl_roll, latino == 1)$LALVOTERID)

c1_l <- exp(confint(glm.cluster(voted ~ I(year == "2018")*treated, data = latino_voters,
                              family = "binomial",
                              weight = latino_voters$weight,
                              cluster = latino_voters$match_group)))

c2_l <- exp(confint(glm.cluster(voted ~ I(year == "2018")*treated +
                                white + black + latino + asian + female +
                                male + reg_date + age + dem + rep +
                                median_income + some_college, data = latino_voters,
                              family = "binomial",
                              weight = latino_voters$weight,
                              cluster = latino_voters$match_group)))
#### women

female_voters <- filter(matches, match_group %in%
                          filter(fl_roll, female == 1)$LALVOTERID)

c1_f <- exp(confint(glm.cluster(voted ~ I(year == "2018")*treated, data = female_voters,
                              family = "binomial",
                              weight = female_voters$weight,
                              cluster = female_voters$match_group)))

c2_f <- exp(confint(glm.cluster(voted ~ I(year == "2018")*treated +
                                white + black + latino + asian + female +
                                male + reg_date + age + dem + rep +
                                median_income + some_college, data = female_voters,
                              family = "binomial",
                              weight = female_voters$weight,
                              cluster = female_voters$match_group)))

#### men

male_voters <- filter(matches, match_group %in%
                          filter(fl_roll, male == 1)$LALVOTERID)

c1_m <- exp(confint(glm.cluster(voted ~ I(year == "2018")*treated, data = male_voters,
                              family = "binomial",
                              weight = male_voters$weight,
                              cluster = male_voters$match_group)))

c2_m <- exp(confint(glm.cluster(voted ~ I(year == "2018")*treated +
                                white + black + latino + asian + female +
                                male + reg_date + age + dem + rep +
                                median_income + some_college, data = male_voters,
                              family = "binomial",
                              weight = male_voters$weight,
                              cluster = male_voters$match_group)))

save(c1, c2, c1_b, c2_b, c1_w, c2_w, c1_l, c2_l,
     c1_f, c2_f, c1_m, c2_m, file = "./temp/confints.rdata")
####
ll <- matches %>% 
  mutate(treated) %>% 
  group_by(treated, year) %>% 
  summarize(voted = mean(voted)) %>% 
  ungroup() %>% 
  mutate(treated = ifelse(treated == 1, "Treated", "Control"))


plot <- ggplot(ll, aes(x = as.integer(year), y = voted,
                       linetype = treated, shape = treated)) +
  geom_line() + geom_point() +
  labs(linetype = "Treatment Group",
       shape = "Treatment Group",
       x = "Year", y = "Turnout Among Registered Voters") +
  scale_y_continuous(labels = percent) +
  theme_bw() + theme(text = element_text(family = "LM Roman 10"))
saveRDS(plot, "./temp/parallel_trends_plot.rds")

##########
order <- fread("./raw_data/var_orders.csv")

matches1 <- data.frame("id" = mout[["index.control"]],
                       "weight" = mout[["weights"]]) %>% 
  group_by(id) %>% 
  summarize(weight = sum(weight)) %>% 
  mutate(treat = F)

matches2 <- data.frame("id" = mout[["index.treated"]],
                       "weight" = mout[["weights"]]) %>% 
  group_by(id) %>% 
  summarize(weight = sum(weight)) %>% 
  mutate(treat = T)

matches <- bind_rows(matches1, matches2)

fl_roll <- fl_roll %>% 
  mutate(id = row_number())

matches <- left_join(matches, fl_roll, by = "id")

##########
means_prematch <- fl_roll %>% 
  group_by(treated) %>% 
  summarize_at(vars(white, black, latino, asian, female,
                      male, reg_date, age, dem, rep,
                      median_income, some_college), mean)

means_postmatch <- matches %>% 
  group_by(treat) %>% 
  summarize_at(vars(white, black, latino, asian, female,
                    male, reg_date, age, dem, rep,
                    median_income, some_college), ~ weighted.mean(., weight))

rm(matches, matches1, matches2)

qqs_post <- lapply(c("white", "black", "latino", "asian", "female",
                     "male", "reg_date", "age", "dem", "rep",
                     "median_income", "some_college"), function(var){
  j <- select(fl_roll, var)
  colnames(j) <- c("t")
  
  qqout  <- qqstats(j$t[mout$index.treated], j$t[mout$index.control])
  return(qqout)
})

qqs_pre <- lapply(c("white", "black", "latino", "asian", "female",
                    "male", "reg_date", "age", "dem", "rep",
                    "median_income", "some_college"), function(var){
  j <- select(fl_roll, var, treated)
  colnames(j) <- c("t", "uncontested")
  
  qqout  <- qqstats(j$t[j$uncontested == T], j$t[j$uncontested == F])
  return(qqout)
})


TrMean <- c()
PreMean <- c()
PreQQmed <- c()
PreQQmean <- c()
PreQQmax <- c()
PostMean <- c()
PostQQmed <- c()
PostQQmean <- c()
PostQQmax <- c()

i = 1
for(var in c("white", "black", "latino", "asian", "female",
             "male", "reg_date", "age", "dem", "rep",
             "median_income", "some_college")){
  TrMean <- unlist(c(TrMean, filter(means_prematch, treated == T) %>% select(var) %>% pull()))
  PreMean <- unlist(c(PreMean, filter(means_prematch, treated == F) %>% select(var) %>% pull()))
  
  PreQQmed <- unlist(c(PreQQmed, qqs_pre[[i]][["mediandiff"]]))
  PreQQmean <- unlist(c(PreQQmean, qqs_pre[[i]][["meandiff"]]))
  PreQQmax <- unlist(c(PreQQmax, qqs_pre[[i]][["maxdiff"]]))
  
  PostMean <- unlist(c(PostMean, filter(means_postmatch, treat == F) %>% select(var) %>% pull()))
  PostQQmed <- unlist(c(PostQQmed, qqs_post[[i]][["mediandiff"]]))
  PostQQmean <- unlist(c(PostQQmean, qqs_post[[i]][["meandiff"]]))
  PostQQmax <- unlist(c(PostQQmax, qqs_post[[i]][["maxdiff"]]))
  
  i = i + 1
}



varnames <- c("white", "black", "latino", "asian", "female",
              "male", "reg_date", "age", "dem", "rep",
              "median_income", "some_college")


df <- data.frame("TrMean" = TrMean,
                 "TrMean2" = TrMean,
                 "PreMean" = PreMean,
                 "PreQQmed" = PreQQmed,
                 "PreQQmean" = PreQQmean,
                 "PreQQmax" = PreQQmax,
                 "PostMean" = PostMean,
                 "PostQQmed" = PostQQmed,
                 "PostQQmean" = PostQQmean,
                 "PostQQmax" = PostQQmax,
                 "names" = varnames) %>%
  mutate(change_mean = 1 - (abs(TrMean - PostMean) / abs(TrMean - PreMean)),
         change_eqqmed = 1 - abs(PostQQmed / PreQQmed),
         change_eqqmean = 1 - abs(PostQQmean / PreQQmean),
         change_eqqmax = 1 - abs(PostQQmax / PreQQmax)) %>%
  mutate_at(vars(TrMean, PreMean, TrMean2, PostMean), funs(comma(round(., 2), accuracy = .01))) %>%
  mutate_at(vars(change_mean, change_eqqmed, change_eqqmean, change_eqqmax), funs(round(. * 100, 2))) %>% 
  filter(names != "voted_primary")

df <- full_join(df, order, by = c("names" = "variable")) %>%
  arrange(order) %>%
  select(name, TrMean, PreMean, TrMean2, PostMean, change_mean, change_eqqmed, change_eqqmean, change_eqqmax) %>%
  filter(!is.na(TrMean))


df <- df %>% 
  mutate_at(vars(TrMean, PreMean, TrMean2, PostMean),
            ~ ifelse(name == "Median Income", dollar(round(as.numeric(gsub(",", "", .)))), .)) %>% 
  mutate_at(vars(TrMean, PreMean, TrMean2, PostMean),
            ~ ifelse(name == "Registration Date",
                     as.numeric(gsub(",", "", .)), .)) %>% 
  mutate_at(vars(TrMean, PreMean, TrMean2, PostMean),
            ~ ifelse(name == "Registration Date",
                     as.character(as.integer(.) + as.Date("2000-01-01")),
                     .)) %>% 
  mutate_at(vars(TrMean, PreMean, TrMean2, PostMean),
            ~ ifelse(substring(name, 1, 1) == "%", percent(as.numeric(.), accuracy = .1), .))

colnames(df) <- c("", "Treated", "Control", "Treated", "Control", "Mean Diff", "eQQ Med", "eQQ Mean", "eQQ Max")

saveRDS(df, "./temp/balance_table.rds")

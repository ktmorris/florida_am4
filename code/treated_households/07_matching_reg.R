

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

m1 <- glm.cluster(voted ~ I(year == "2018")*treated, data = matches,
                  family = "binomial", cluster = matches$match_group,
                  weight = matches$weight)

m2 <- glm.cluster(voted ~ I(year == "2018")*treated +
                    white + black + latino + asian + female +
                    male + reg_date + age + dem + rep +
                    median_income + some_college, data = matches,
                  family = "binomial", cluster = matches$match_group,
                  weight = matches$weight)

#### subgroup

black_voters <- filter(matches, match_group %in%
                         filter(fl_roll, black == 1)$LALVOTERID)

m1_black <- glm.cluster(voted ~ I(year == "2018")*treated, data = black_voters,
                        family = "binomial", cluster = black_voters$match_group,
                        weight = black_voters$weight)

m2_black <- glm.cluster(voted ~ I(year == "2018")*treated +
                          white + black + latino + asian + female +
                          male + reg_date + age + dem + rep +
                          median_income + some_college, data = black_voters,
                        family = "binomial", cluster = black_voters$match_group,
                        weight = black_voters$weight)

#### white

white_voters <- filter(matches, match_group %in%
                         filter(fl_roll, white == 1)$LALVOTERID)

m1_white <- glm.cluster(voted ~ I(year == "2018")*treated, data = white_voters,
                        family = "binomial", cluster = white_voters$match_group,
                        weight = white_voters$weight)

m2_white <- glm.cluster(voted ~ I(year == "2018")*treated +
                          white + black + latino + asian + female +
                          male + reg_date + age + dem + rep +
                          median_income + some_college, data = white_voters,
                        family = "binomial", cluster = white_voters$match_group,
                        weight = white_voters$weight)

#### latino

latino_voters <- filter(matches, match_group %in%
                         filter(fl_roll, latino == 1)$LALVOTERID)

m1_latino <- glm.cluster(voted ~ I(year == "2018")*treated, data = latino_voters,
                        family = "binomial", cluster = latino_voters$match_group,
                        weight = latino_voters$weight)

m2_latino <- glm.cluster(voted ~ I(year == "2018")*treated +
                          white + black + latino + asian + female +
                          male + reg_date + age + dem + rep +
                          median_income + some_college, data = latino_voters,
                        family = "binomial", cluster = latino_voters$match_group,
                        weight = latino_voters$weight)
#### women

female_voters <- filter(matches, match_group %in%
                          filter(fl_roll, female == 1)$LALVOTERID)

m1_female <- glm.cluster(voted ~ I(year == "2018")*treated, data = female_voters,
                         family = "binomial", cluster = female_voters$match_group,
                         weight = female_voters$weight)

m2_female <- glm.cluster(voted ~ I(year == "2018")*treated +
                           white + black + latino + asian + female +
                           male + reg_date + age + dem + rep +
                           median_income + some_college, data = female_voters,
                         family = "binomial", cluster = female_voters$match_group,
                         weight = female_voters$weight)

#### men

male_voters <- filter(matches, match_group %in%
                          filter(fl_roll, male == 1)$LALVOTERID)

m1_male <- glm.cluster(voted ~ I(year == "2018")*treated, data = male_voters,
                         family = "binomial", cluster = male_voters$match_group,
                         weight = male_voters$weight)

m2_male <- glm.cluster(voted ~ I(year == "2018")*treated +
                           white + black + latino + asian + female +
                           male + reg_date + age + dem + rep +
                           median_income + some_college, data = male_voters,
                         family = "binomial", cluster = male_voters$match_group,
                         weight = male_voters$weight)

save(m1, m2, m1_black, m2_black, m1_white, m2_white, m1_latino, m2_latino,
     m1_female, m2_female, m1_male, m2_male, file = "./temp/matched_reg_output.rdata")
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

colnames(df) <- c("", "Treated", "Control", "Treated", "Control", "Mean Diff", "eQQ Med", "eQQ Mean", "eQQ Max")


saveRDS(df, "./temp/balance_table.rds")

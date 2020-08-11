
order <- fread("./raw_data/var_orders.csv")

load("./temp/mout_t1.RData")

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

fl_roll <- readRDS("./temp/fl_file_pre_match.rds") %>% 
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
  mutate_at(vars(TrMean, PreMean, TrMean2, PostMean), ~ ifelse(names %in% c("asian", "black", "dem",
                                                                            "female", "latino", "male",
                                                                            "rep", "some_college", "white"),
                                                               round(., 3),
                                                               comma(round(., 2), accuracy = .01))) %>%
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

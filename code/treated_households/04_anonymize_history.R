### ANONYMIZE HISTORY

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


scrambled_ids <- readRDS("temp/id_lookup_anon.rds")

fl_history <- left_join(fl_history, scrambled_ids) %>% 
  select(-LALVOTERID)


saveRDS(fl_history, "temp/fl_history_anon.rds")

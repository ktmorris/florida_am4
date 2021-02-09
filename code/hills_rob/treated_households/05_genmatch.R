## set seed for reproducibility. this is the same seed used in body
set.seed(85132)

library(Matching)
library(data.table)
library(scales)
library(kableExtra)
library(tidyverse)
require(snow)
require(parallel)


Sys.info()

NodeFile = Sys.getenv("MY_HOSTFILE")

print(NodeFile)

readLines(NodeFile)

cl<-makeCluster(c(readLines(NodeFile)), type="SOCK")
cl


fl_file_pre_match <- readRDS("./temp/hills_file_pre_match.rds") 

samp <- fl_file_pre_match %>% 
  group_by(treated) %>% 
  sample_frac(0.05) %>% 
  ungroup()

match_data <- samp %>% 
  select(-voter_id_anon, -treated, -max_release)

genout <- GenMatch(Tr = samp$treated, X = match_data, replace = T, cluster = cl,
                   exact = c(rep(T, 6), F, F, T, T, F, F, T), pop.size = 1000)
saveRDS(genout, "temp/genout_av_hills.rds")

stopCluster(cl)
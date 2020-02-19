## this can be run locally or on NYU's HPC. Set option in next step
## option allowed because of how long GenMatch can take

on_nyu <- F

if(on_nyu){
  library(Matching)
  library(data.table)
  library(snow)
  library(parallel)
  library(scales)
  library(kableExtra)
  library(tidyverse)
  
  setwd("/scratch/km3815/matching")
  
  NodeFile = Sys.getenv("MY_HOSTFILE")
  
  
  cl <- makeCluster(c(readLines(NodeFile)), type="SOCK")
}else{
  source("./code/misc/AutoCluster4.R")
  cl <- NCPUS(detectCores() - 1)
}

fl_file_pre_match <- readRDS("./temp/fl_file_pre_match.rds") 

samp <- fl_file_pre_match %>% 
  group_by(treated) %>% 
  sample_frac(0.05)

match_data <- samp %>% 
  select(-LALVOTERID, -treated, -max_release)

genout <- GenMatch(Tr = samp$treated, X = match_data, replace = T, cluster = cl,
                   exact = c(rep(T, 6), F, F, T, T, F, F, T), pop.size = 1000)
saveRDS(genout, "./temp/genout_t1.rds")

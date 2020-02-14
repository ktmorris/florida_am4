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

samp <- rbind(
  filter(fl_file_pre_match, treated == 1) %>% 
    sample_frac(0.05),
  filter(fl_file_pre_match, !treated) %>% 
    sample_frac(0.05)
)

match_data <- samp %>% 
  select(-LALVOTERID, -treated, -max_release, -US_Congressional_District)

genout <- GenMatch(Tr = samp$treated, X = match_data, replace = T, cluster = cl)
saveRDS(genout, "./temp/genout_t1.rds")

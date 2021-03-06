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
}



#####
fl_roll <- readRDS("./temp/fl_file_pre_match.rds") %>% 
  ungroup()

##########

ids <- fl_roll %>% 
  mutate(id = row_number()) %>% 
  select(id, voter_id_anon)

X <- fl_roll %>%
  dplyr::select(-voter_id_anon, -treated, -starts_with("General"),
                -max_release)


genout <- readRDS("./temp/genout_av.rds")

mout <- Matchby(Tr = fl_roll$treated, X = X,
                by = c(X$US_Congressional_District,
                       X$white,
                       X$black,
                       X$latino,
                       X$asian,
                       X$female,
                       X$male,
                       X$dem,
                       X$rep), estimand = "ATT", Weight.matrix = genout, M = 5,
                exact = c(rep(F, 12), T))

save(mout, file = "./temp/mout_av.RData")
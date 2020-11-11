## THIS FILE LOADS THE NECESSARY PACKAGES AND CONNECTS TO
## A SQL DATABASE HOLDING THE VOTER FILE DATA

library(butcher)
library(tidycensus)
library(extrafont)
library(miceadds)
library(Hmisc)
library(lubridate)
library(lme4)
library(scales)
library(estimatr)
library(ggeffects)
library(rgdal)
library(MatchIt)
library(Matching)
library(kevostools)
library(ggmap)
library(RSQLite)
library(tidyverse)
library(data.table)


db <- dbConnect(SQLite(), "D:/national_file.db")

weighted.ttest.ci <- function(x, weights, conf.level = 0.95) {
  nx <- length(x)
  df <- nx - 1
  vx <- wtd.var(x, weights, normwt = TRUE) ## From Hmisc
  mx <- weighted.mean(x, weights)
  stderr <- sqrt(vx/nx)
  tstat <- mx/stderr ## not mx - mu
  alpha <- 1 - conf.level
  cint <- qt(1 - alpha/2, df)
  cint <- tstat + c(-cint, cint)
  cint * stderr
}

save <- c("db", "cleanup", "theme_bc", "save", "weighted.ttest.ci")


cleanup <- function(...){
  save2 <- c(save, ...)
  rm(list=ls(envir = .GlobalEnv)[! ls(envir = .GlobalEnv) %in% save2], envir = .GlobalEnv)
}
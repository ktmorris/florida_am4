load("temp/pre_reg.rdata")

matches <- filter(matches, as.Date(match_reg_date + as.Date("2000-01-01")) <= "2018-10-09")
matches2 <- filter(matches2, as.Date(match_reg_date + as.Date("2000-01-01")) <= "2018-10-09")

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
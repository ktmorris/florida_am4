results_demos <- readRDS("temp/results_demos_ll.rds")

f1 <- share_yes ~ small_res_doc + white + black + latino + asian +
  female + male + dem + rep + age +
  median_income + some_college + unem +
  General_2016_11_08 + General_2014_11_04 +
  General_2012_11_06 + General_2010_11_02 +
  US_Congressional_District

f1b <- share_yes ~ small_res_doc + years_since + white + black + latino + asian +
  female + male + dem + rep + age +
  median_income + some_college + unem +
  General_2016_11_08 + General_2014_11_04 +
  General_2012_11_06 + General_2010_11_02 +
  US_Congressional_District

m1 <- lm(f1, data = filter(results_demos, to <= 1))

m1_rob <- lm_robust(f1, data = filter(results_demos, to <= 1),
                    clusters = US_Congressional_District)
m1_ses <- data.frame(
  summary(m1_rob)$coefficients)[, 2]


m1b <- lm((f1b), data = filter(results_demos, to <= 1))

m1b_rob <- lm_robust(f1b, data = filter(results_demos, to <= 1),
                     clusters = US_Congressional_District)
m1b_ses <- data.frame(
  summary(m1b_rob)$coefficients)[, 2]

save(m1, m1_ses, m1b, m1b_ses, file = "./temp/support_reg.rdata")

marg <- ggeffect(model = m1_rob, c("small_res_doc [all]"))

cm1 <- mean(filter(results_demos, to <= 1)$share_yes)

p1 <- ggplot() + 
  geom_histogram(aes(x = small_res_doc, y = ..count../2500), position="identity", linetype=1,
                 fill="gray60", data = results_demos, alpha=0.5, bins = 30) + 
  geom_line(aes(x = x, y = predicted), data = marg, color = "black") +
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), fill= "black", alpha=0.25, data = marg) +
  xlab("Number of Formerly Incarcerated Residents") +
  ylab("Support for Amendment 4") + scale_x_continuous(labels = comma, limits = c(0, 300)) +
  scale_y_continuous(labels = percent) +
  labs(caption = "Notes: Distribution of number of formerly incarcerated residents shown at bottom.") +
  geom_hline(yintercept = cm1, linetype = 2) +
  geom_text(aes(300, cm1-0.03, label = "Average Precinct Support for Amendment 4",
                family = "LM Roman 10", hjust = 1)) +
  theme(plot.caption = element_text(hjust = 0)) +
  theme_bw() + theme(plot.caption = element_text(hjust = 0),
                     text = element_text(family = "LM Roman 10"))
save(p1, cm1, file = "./temp/marg_support_am4.rdata")

#############
f2 <- to ~ small_res_doc + white + black + latino + asian +
  female + male + dem + rep + age +
  median_income + some_college + unem +
  General_2016_11_08 + General_2014_11_04 +
  General_2012_11_06 + General_2010_11_02 +
  US_Congressional_District

f2b <- to ~ small_res_doc + years_since + white + black + latino + asian +
  female + male + dem + rep + age +
  median_income + some_college + unem +
  General_2016_11_08 + General_2014_11_04 +
  General_2012_11_06 + General_2010_11_02 +
  US_Congressional_District

m2 <- lm(f2, data = filter(results_demos, to <= 1))

m2_rob <- lm_robust(f2, data = filter(results_demos, to <= 1),
                    clusters = US_Congressional_District)
m2_ses <- data.frame(
  summary(m2_rob)$coefficients)[, 2]


m2b <- lm(f2b, data = filter(results_demos, to <= 1))

m2b_rob <- lm_robust(f2b, data = filter(results_demos, to <= 1),
                     clusters = US_Congressional_District)
m2b_ses <- data.frame(
  summary(m2b_rob)$coefficients)[, 2]

save(m2, m2_ses, m2b, m2b_ses, file = "./temp/precinct_turnout.rdata")

marg <- ggeffect(model = m2_rob, "small_res_doc [all]")
cm2 <- mean(filter(results_demos, to <= 1)$to)
p2 <- ggplot() + 
  geom_histogram(aes(x = small_res_doc, y = ..count../2500), position="identity", linetype=1,
                 fill="gray60", data = results_demos, alpha=0.5, bins = 30) + 
  geom_line(aes(x = x, y = predicted), data = marg, color = "black") +
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), fill= "black", alpha=0.25, data = marg) +
  xlab("Number of Formerly Incarcerated Residents") +
  ylab("Turnout Among Registered Voters") + scale_x_continuous(labels = comma, limits = c(0, 300)) +
  scale_y_continuous(labels = percent) +
  labs(caption = "Notes: Distribution of number of formerly incarcerated residents shown at bottom.") +
  geom_hline(yintercept = cm2, linetype = 2) +
  geom_text(aes(300, cm2, label = "Average Precinct Turnout",
                vjust = -.5, family = "LM Roman 10", hjust = 1)) +
  theme(plot.caption = element_text(hjust = 0)) +
  theme_bw() + theme(plot.caption = element_text(hjust = 0),
                     text = element_text(family = "LM Roman 10"))

save(p2, cm2, file = "./temp/marg_pct_to.rdata")
#############
f3 <- roll_off ~ small_res_doc + white + black + latino + asian +
  female + male + dem + rep + age +
  median_income + some_college + unem +
  General_2016_11_08 + General_2014_11_04 +
  General_2012_11_06 + General_2010_11_02 +
  US_Congressional_District

f3b <- roll_off ~ small_res_doc + years_since + white + black + latino + asian +
  female + male + dem + rep + age +
  median_income + some_college + unem +
  General_2016_11_08 + General_2014_11_04 +
  General_2012_11_06 + General_2010_11_02 +
  US_Congressional_District

m3 <- lm(f3, data = filter(results_demos, to <= 1))

m3_rob <- lm_robust(f3, data = filter(results_demos, to <= 1),
                    clusters = US_Congressional_District)
m3_ses <- data.frame(
  summary(m3_rob)$coefficients)[, 2]


m3b <- lm((f3b), data = filter(results_demos, to <= 1))

m3b_rob <- lm_robust(f3b, data = filter(results_demos, to <= 1),
                     clusters = US_Congressional_District)
m3b_ses <- data.frame(
  summary(m3b_rob)$coefficients)[, 2]

save(m3, m3_ses, m3b, m3b_ses, file = "./temp/precinct_rolloff.rdata")

marg <- ggeffect(model = m3_rob, "small_res_doc [all]")

cm3 <- mean(filter(results_demos, to <= 1)$roll_off)

p3 <- ggplot() + 
  geom_histogram(aes(x = small_res_doc, y = ..count../50000), position="identity", linetype=1,
                 fill="gray60", data = results_demos, alpha=0.5, bins = 30) + 
  geom_line(aes(x = x, y = predicted), data = marg, color = "black") +
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), fill= "black", alpha=0.25, data = marg) +
  xlab("Number of Formerly Incarcerated Residents") +
  ylab("Precinct Amendment 4 Roll-Off") + scale_x_continuous(labels = comma, limits = c(0, 300)) +
  scale_y_continuous(labels = percent) +
  labs(caption = "Notes: Distribution of number of formerly incarcerated residents shown at bottom.") +
  geom_hline(yintercept = cm3, linetype = 2) +
  geom_text(aes(300, cm3, label = "Average Precinct Roll-Off",
                vjust = -.5, family = "LM Roman 10", hjust = 1)) +
  theme(plot.caption = element_text(hjust = 0)) +
  theme_bw() + theme(plot.caption = element_text(hjust = 0),
                     text = element_text(family = "LM Roman 10"))
save(p3, cm3, file = "./temp/marg_rolloff.rdata")

################ for appendix
m1_ap <- lm(share_yes ~ small_res_doc_recent + white + black + latino + asian +
              female + male + dem + rep + age +
              median_income + some_college + unem +
              General_2016_11_08 + General_2014_11_04 +
              General_2012_11_06 + General_2010_11_02 +
              US_Congressional_District, data = filter(results_demos, to <= 1))

m2_ap <- lm(to ~ small_res_doc_recent + white + black + latino + asian +
              female + male + dem + rep + age +
              median_income + some_college + unem +
              General_2016_11_08 + General_2014_11_04 +
              General_2012_11_06 + General_2010_11_02 +
              US_Congressional_District, data = filter(results_demos, to <= 1))

m3_ap <- lm(roll_off ~ small_res_doc_recent + white + black + latino + asian +
              female + male + dem + rep + age +
              median_income + some_college + unem +
              General_2016_11_08 + General_2014_11_04 +
              General_2012_11_06 + General_2010_11_02 +
              US_Congressional_District, data = filter(results_demos, to <= 1))

m1b_ap <- lm(share_yes ~ all_doc + white + black + latino + asian +
               female + male + dem + rep + age +
               median_income + some_college + unem +
               General_2016_11_08 + General_2014_11_04 +
               General_2012_11_06 + General_2010_11_02 +
               US_Congressional_District, data = filter(results_demos, to <= 1))

m2b_ap <- lm(to ~ all_doc + white + black + latino + asian +
               female + male + dem + rep + age +
               median_income + some_college + unem +
               General_2016_11_08 + General_2014_11_04 +
               General_2012_11_06 + General_2010_11_02 +
               US_Congressional_District, data = filter(results_demos, to <= 1))

m3b_ap <- lm(roll_off ~ all_doc + white + black + latino + asian +
               female + male + dem + rep + age +
               median_income + some_college + unem +
               General_2016_11_08 + General_2014_11_04 +
               General_2012_11_06 + General_2010_11_02 +
               US_Congressional_District, data = filter(results_demos, to <= 1))

save(m1_ap, m2_ap, m3_ap,
     m1b_ap, m2b_ap, m3b_ap, file = "./temp/precinct_regs_appendix.rdata")

#################################################
############# BLOCK GROUP REGRESSIONS ###########
#################################################

bg_level <- readRDS("temp/bg_level_reg_data.rds")

m1 <- lm(to_18 ~ small_res_doc + 
           white + black + latino + asian +
           female + male + dem + rep + age +
           median_income + some_college + unem +
           to_16 + to_14 + to_12 + to_10 + 
           US_Congressional_District,
         data = filter(bg_level, to_18 <= 1))

m1_rob <- lm_robust(to_18 ~ small_res_doc + 
                      white + black + latino + asian +
                      female + male + dem + rep + age +
                      median_income + some_college + unem +
                      to_16 + to_14 + to_12 + to_10 + 
                      US_Congressional_District,
                    data = filter(bg_level, to_18 <= 1),
                    clusters = US_Congressional_District)


m1_ses <- data.frame(
  summary(m1_rob)$coefficients)[, 2]

m1b <- lm(to_18 ~ small_res_doc + years_since +
            white + black + latino + asian +
            female + male + dem + rep + age +
            median_income + some_college + unem +
            to_16 + to_14 + to_12 + to_10 + 
            US_Congressional_District,
          data = filter(bg_level, to_18 <= 1))

m1b_rob <- lm_robust(to_18 ~ small_res_doc + years_since +
                       white + black + latino + asian +
                       female + male + dem + rep + age +
                       median_income + some_college + unem +
                       to_16 + to_14 + to_12 + to_10 + 
                       US_Congressional_District,
                     data = filter(bg_level, to_18 <= 1),
                     clusters = US_Congressional_District)


m1b_ses <- data.frame(
  summary(m1b_rob)$coefficients)[, 2]

save(m1, m1_ses, m1b, m1b_ses, file = "./temp/bg_turnout.rdata")


######
m1_ap <- lm(to_18 ~ small_res_doc_recent + 
              white + black + latino + asian +
              female + male + dem + rep + age +
              median_income + some_college + unem +
              to_16 + to_14 + to_12 + to_10 + 
              US_Congressional_District,
            data = filter(bg_level, to_18 <= 1))

m1b_ap <- lm(to_18 ~ all_doc + 
               white + black + latino + asian +
               female + male + dem + rep + age +
               median_income + some_college + unem +
               to_16 + to_14 + to_12 + to_10 + 
               US_Congressional_District,
             data = filter(bg_level, to_18 <= 1))

save(m1_ap, m1b_ap, file = "./temp/bg_regs_appendix.rdata")
######

bgs_new <- inner_join(readRDS("./temp/block_group_census_data.RDS"),
                      select(bg_level, GEOID, all_doc)) %>% 
  ungroup()


bgs_new <- rbind(
  bgs_new %>% 
    mutate(group = "former_inc",
           weight = all_doc),
  bgs_new %>% 
    mutate(group = "overall",
           weight = population)
)

######

tot <- rbindlist(lapply(c("median_income", "median_age", "unem", "some_college",
                          "nh_white", "nh_black", "latino"), function(m){
                            ints <- rbindlist(lapply(unique(bgs_new$group), function(r){
                              r <- as.character(r)
                              t <- bgs_new %>% 
                                filter(group == r) %>% 
                                select(weight, measure = m) %>% 
                                filter(!is.na(measure))
                              j <- weighted.ttest.ci((t$measure), weights = t$weight,)
                              j <- data.table(group = c(r),
                                              measure = m,
                                              lower = j[1],
                                              upper = j[2])
                            }))
                            d <- data.table(sig = (ints$lower[1] > ints$upper[2]) | (ints$lower[2] > ints$upper[1]),
                                            measure = m)
                            return(d)
                          }))



ll <- bgs_new %>% 
  group_by(group) %>% 
  summarize_at(vars("median_income", "median_age", "unem", "some_college",
                    "nh_white", "nh_black", "latino"),
               ~ weighted.mean(., weight, na.rm = T))

ll2 <- group_by(bgs_new, group) %>% summarize(count = sum(weight))

ll <- left_join(ll, ll2) %>% 
  mutate(median_income = dollar(median_income, accuracy = 1),
         median_age = round(median_age, digits = 1),
         count = comma(count)) %>% 
  mutate_at(vars(unem, some_college,
                 nh_white, nh_black, latino), ~ percent(., accuracy = 0.1))

ll <- transpose(ll)
ll$var <- c("measure", "median_income", "median_age", "unem", "some_college",
            "nh_white", "nh_black", "latino", "count")

colnames(ll) <- ll[1,]
ll <- ll[2:nrow(ll),]

ll <- left_join(ll, tot)

ll$measure <- c( "Median Income", "Median Age", "% Unemployed", "% with Some College",
                 "% Non-Hispanic White", "% Non-Hispanic Black", "% Latino", "Count")
ll <- ll %>% 
  mutate(measure = ifelse(sig & !is.na(sig), paste0(measure, "*"), measure)) %>% 
  select(measure, overall, former_inc)

colnames(ll) <- c("Measure", "Average Neighborhood", "Average Neighborhood\\\\for Formerly Incarcerated")

saveRDS(ll, "./temp/demos_nhoods.rds")
